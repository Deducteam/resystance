open Core
open Term
open Common.Logger
let log_unif = (Common.Logger.make 'v' "suni" "syntactic unification").pp
(*let log_unif = log_unif.logger*)

(** Name of rewriting variables.  We distinguish (bound) variables due
    to higher order and rewrite variables.  In [\x.f(X, x)], [X] is a
    rewrite variable and [x] is a bound variable (bound by [\x]). *)
type vname = string

module VnMap = Map.Make(struct type t = vname let compare = String.compare end)

type substitution = (vname * term) list

let pp_subst : Format.formatter -> substitution -> unit = fun fmt s ->
  let pp_sep fmt () = Format.fprintf fmt ", " in
  let pp_subst fmt (vn, t) = Format.fprintf fmt "%s := %a" vn Print.term t in
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
  | Symb(_) as u , ts -> Basics.add_args u (List.map (lift s) ts)
  | Abst(t1,t2), ts ->
     let (x,u) = Bindlib.unbind t2 in
     Basics.add_args
       (Bindlib.(unbox(_Abst(box t1)(bind_var x (Terms.lift (lift s u))))))
       (List.map (lift s) ts)
  | Vari _, _ -> t
  | _ ->
     Format.eprintf "cannot lift %a\n" Print.term t;
     assert false

(*let rec lz_app : substitution -> vname -> term Lazy.t =
  fun s x -> let t = List.assoc x s in lazy (lz_lift s t)

and lz_lift : substitution -> term -> term = fun s t ->
  match Basics.get_args t with
  | Patt(_, v, _), ts when indom v s ->
    Basics.add_args (Lazy.force (lz_app s v)) (List.map (lz_lift s) ts)
  | Patt(_) as h, ts -> Basics.add_args h (List.map (lz_lift s) ts)
  | Symb(_) as u , ts -> Basics.add_args u (List.map (lz_lift s) ts)
  | _ -> assert false*)
(*
let lz_lift s t =
  let ls = List.map (fun (v,t) -> (v,lazy(proc t)) in
  let rec proc t =
  match Basics.get_args t with
  | Patt(_, v, _), ts when indom v s ->
    Basics.add_args (Lazy.force (List.assoc v ls)) (List.map proc ts)
  | Patt(_)|Symb(_) as h, ts -> Basics.add_args h (List.map proc ts)
  | _ -> assert false
 *)     
        
(** [occurs v t] is true iff variable [v] appears in term [t]. *)
let rec occurs : vname -> term -> bool = fun v t ->
  match Basics.get_args t with
  | Patt(_, w, _), args -> v = w || List.exists (occurs v) args
  | Symb(_)      , args -> List.exists (occurs v) args
  | Vari _       , args -> List.exists (occurs v) args
  | Abst(_,u)    , args ->
     let (_,u) = Bindlib.unbind u in
     List.exists (occurs v) (u::args)
  | _ -> assert false

exception CantUnify

(** [solve eqs s] returns the substitution [s] with the additional bindings to
    unify equations in [eqs].  An element [(t, u)] of [eqs] is a unification
    [t =? u].

    @raise CantUnify if unification is impossible. *)
let rec solve : (term * term) list -> substitution -> substitution =
  fun eqs s ->
  match eqs with
  | (t, u) :: tl ->
    log_unif "solve [%a =? %a]" Print.term t Print.term u;
    begin match (Term.get_args t, Term.get_args u) with
    | (Abst _,_), (Symb _,_) | (Symb _,_),(Abst _,_) -> raise CantUnify (* eta! *)
    | (Symb(q)   ,ts), (Symb(r),us) when q != r || List.compare_lengths ts us <> 0 ->
       raise CantUnify
    | (Symb(_)     ,ts), (Symb(_)  ,us)             ->
      solve (List.combine ts us @ tl) s
    | ((Abst _|Symb(_)),_) , (_        , _)             -> solve ((u, t) :: tl) s
    | (Patt(_) as v,_) , (t        , _) when v = t  -> solve tl s
    | (Patt(_,x,_) ,_) , (_        , _)             -> elim x u tl s
    | _                                             ->
       Format.eprintf "Unification failed:\n %a != %a\n" Print.term t Print.term u;
       assert false
    end
  | [] -> s

(** [elim v t eqs s] eliminates variable [v] replacing it by [t] in
    equations [eqs] and in substitution [s]. *)
and elim : vname -> term -> (term * term) list -> substitution -> substitution =
  fun x t eqs s ->
  if occurs x t then raise CantUnify else
  let xt = lift [(x, t)] in
  solve (List.map (fun (u, u') -> (xt u, xt u')) eqs)
    ((x, t) :: (List.map (fun (v, u) -> (v, xt u)) s))

let unify : term -> term -> substitution = fun t u -> solve [(t, u)] []
