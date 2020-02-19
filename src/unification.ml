open Core
open Terms

let log_unif = Console.new_logger 'v' "suni" "syntactic unification"
let log_unif = log_unif.logger

(** Name of rewriting variables.  We distinguish (bound) variables due
    to higher order and rewrite variables.  In [\x.f(X, x)], [X] is a
    rewrite variable and [x] is a bound variable (bound by [\x]). *)
type vname = string

module VnMap = Map.Make(struct type t = vname let compare = String.compare end)
module HoVarSet = Set.Make(struct
    type t = term Bindlib.var
    let compare = Bindlib.compare_vars
  end)

type substitution = (vname * term) list

let pp_subst : Format.formatter -> substitution -> unit = fun fmt s ->
  let pp_sep fmt () = Format.fprintf fmt ", " in
  let pp_subst fmt (vn, t) = Format.fprintf fmt "%s := %a" vn Print.pp t in
  Format.pp_print_list ~pp_sep pp_subst fmt s

(** [rename vm nm] returns a fresh name if [nm] is not bound in [vm],
    else it returns the value bound in [vm].  If [nm] was not bound in
    [vm], the returned mapping is [vm ∪ {nm ↦ nm'}] where [nm'] is an
    integer which has been added at the end of [nm] to create a fresh
    name. *)
let rename : int VnMap.t -> vname -> int VnMap.t * vname =
  let counter = ref 0 in
  fun seen vn ->
  let vnuntagged = Filename.remove_extension vn in
  let newtag, seen = match VnMap.find_opt vn seen with
    | None    -> incr counter; (!counter, VnMap.add vn !counter seen)
    | Some(n) -> (n, seen)
  in
  seen, vnuntagged ^ "." ^ (string_of_int newtag)

let rename : term -> term = fun te ->
  let rec loop : int VnMap.t -> term -> int VnMap.t * term = fun seen te ->
    match te with
    | Appl(t, u) ->
      let seen, t = loop seen t in
      let seen, u = loop seen u in
      seen, Appl(t, u)
    | Patt(a, v, b) ->
      let seen, name = rename seen v in
      seen, Patt(a, name, b)
    | t -> seen, t
  in
  loop VnMap.empty te |> snd

(** [indom n s] is true iff (rewriting) variable [n] is bound in
    substitution [s]. *)
let indom : vname -> substitution -> bool = fun vn->
  List.exists (fun (y, _) -> y = vn)

(** [app s v] applies substitution [s] to variable [v].

    @raise Not_found if [v] is not bound by [s]. *)
let app : substitution -> vname -> term = fun s x -> List.assoc x s

let rec lift : substitution -> term -> term = fun s t ->
  match Basics.get_args t with
  | Patt(_, v, _), ts when indom v s ->
    Basics.add_args (app s v) (List.map (lift s) ts)
  | Patt(_) as h, ts -> Basics.add_args h (List.map (lift s) ts)
  | Symb(_) as u, ts -> Basics.add_args u (List.map (lift s) ts)
  | Abst(a, t)  , ts ->
    (* Substitute into the body *)
    let x, t = Bindlib.unbind t in
    let t = lift s t in
    (* and build back the abstraction *)
    let t = Bindlib.unbox (Bindlib.bind_var x (Terms.lift t)) in
    Basics.add_args (Abst(a, t)) (List.map (lift s) ts)
  | _ -> assert false

(** [occurs v t] is true iff variable [v] appears in term [t]. *)
let rec occurs : vname -> term -> bool = fun v t ->
  match Basics.get_args t with
  | Patt(_, w, _), args -> v = w || List.exists (occurs v) args
  | Symb(_)      , args -> List.exists (occurs v) args
  | _ -> assert false

exception CantUnify

(** [solve eqs s ctx] returns the substitution [s] with the additional bindings
    to unify equations in [eqs] in context [ctx]. An element [(t, u)] of [eqs]
    is a unification [t =? u].

    @raise CantUnify if unification is impossible. *)
let rec solve : (term * term) list -> substitution -> HoVarSet.t -> substitution =
  fun eqs s ctx ->
  match eqs with
  | (t, u) :: tl ->
    log_unif "solve [%a =? %a]" Print.pp t Print.pp u;
    begin match (Basics.get_args t, Basics.get_args u) with
    (*  q =? r
     * -------- q <> r
     *   fail          *)
    | (Symb(q,_)     ,_ ), (Symb(r,_), _) when q != r -> raise CantUnify
    (*        t1 =? u1 ... tn =? un
     * ----------------------------------
     *  q(t1, ..., tn) =? q(u1, ..., un)  *)
    | (Symb(_)       ,ts), (Symb(_)  ,us)             ->
      solve (List.combine ts us @ tl) s ctx
    | (Symb(_)       ,_) , (_        , _)             ->
      solve ((u, t) :: tl) s ctx
    (* -----------
     *   &x =? v   *)
    | (Patt(_) as v  ,_) , (t        , _) when v = t  -> solve tl s ctx
    | (Patt(_,x,[||]),_) , (_        , _)             -> elim x u tl s ctx
    (*  t[z/x] =? u[z/y]
     * ------------------
     *  λ x, t =? λ y, u  *)
    | (Abst(_,t)     ,_) , (Abst(_,u), _)             ->
      let x, t, u = Bindlib.unbind2 t u in
      let ctx = HoVarSet.add x ctx in
      solve ((u, t) :: tl) s ctx
    | (Patt(_,x,ar)  ,_) , (t        , _)             ->
      (* FIXME using a set might be a bad idea (losing order of vars). *)
      let allowed = Array.to_seq ar |> Seq.map Basics.to_tvar |>
                    HoVarSet.of_seq
      in
      let t = Terms.lift t in
      let cond v = HoVarSet.mem v allowed || not @@ Bindlib.occur v t in
      if HoVarSet.for_all cond ctx then elim x u tl s ctx else raise CantUnify
    | (Abst(_)       ,_) , (Symb(_)  , _)             -> raise CantUnify
    | (Abst(_)       ,_) , (_        , _)             ->
      solve ((u, t) :: tl) s ctx
    | _                                               -> assert false
    end
  | [] -> s

(** [elim v t eqs s] eliminates variable [v] replacing it by [t] in
    equations [eqs] and in substitution [s]. *)
and elim : vname -> term -> (term * term) list -> substitution -> HoVarSet.t
  -> substitution =
  fun x t eqs s ctx ->
  if occurs x t then raise CantUnify else
  let xt = lift [(x, t)] in
  solve (List.map (fun (u, u') -> (xt u, xt u')) eqs)
    ((x, t) :: (List.map (fun (v, u) -> (v, xt u)) s)) ctx

let unify : term -> term -> substitution = fun t u ->
  log_unif "unifying [%a] =? [%a]" Print.pp t Print.pp u;
  solve [(t, u)] [] HoVarSet.empty
