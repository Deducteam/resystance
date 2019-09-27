open Core
open Terms

type vname = string

module VnMap = Map.Make(struct type t = vname let compare = String.compare end)

type substitution = (vname * term) list

let pp_subst : Format.formatter -> substitution -> unit = fun fmt s ->
  let pp_sep fmt () = Format.fprintf fmt ", " in
  let pp_subst fmt (vn, t) = Format.fprintf fmt "%s := %a" vn Print.pp t in
  Format.pp_print_list ~pp_sep pp_subst fmt s

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

let indom : vname -> substitution -> bool = fun vn->
  List.exists (fun (y, _) -> y = vn)

let rec app : substitution -> vname -> term = fun s x ->
  match s with
  | (v, t) :: _ when v = x -> t
  | _ :: tl                -> app tl x
  | []                     -> invalid_arg "app"

let rec lift : substitution -> term -> term = fun s t ->
  match Basics.get_args t with
  | Patt(_, v, _), ts when indom v s ->
    Basics.add_args (app s v) (List.map (lift s) ts)
  | Patt(_) as h, ts -> Basics.add_args h (List.map (lift s) ts)
  | Symb(_) as u , ts -> Basics.add_args u (List.map (lift s) ts)
  | _ -> assert false

let rec occurs : vname -> term -> bool = fun v t ->
  match Basics.get_args t with
  | Patt(_, w, _), args -> v = w || List.exists (occurs v) args
  | Symb(_)      , args -> List.exists (occurs v) args
  | _ -> assert false

exception CantUnify

let rec solve : (term * term) list -> substitution -> substitution =
  fun eqs s ->
  match eqs with
  | (t, u) :: tl ->
    begin match (Basics.get_args t, Basics.get_args u) with
    | (Symb(q,_)   ,_ ), (Symb(r,_), _) when q != r -> raise CantUnify
    | (Symb(_)     ,ts), (Symb(_)  ,us)             ->
      solve (List.combine ts us @ tl) s
    | (Symb(_) as q,ts), (_        , _)             ->
      solve ((u, Basics.add_args q ts) :: tl) s
    | (Patt(_) as v,_) , (t        , _) when v = t  -> solve tl s
    | (Patt(_,x,_) ,_) , (_        , _)             -> elim x u tl s
    | _                                             -> assert false
    end
  | [] -> s

and elim : vname -> term -> (term * term) list -> substitution -> substitution =
  fun x t eqs s ->
  if occurs x t then raise CantUnify else
  let xt = lift [(x, t)] in
  solve (List.map (fun (u, u') -> (xt u, xt u')) eqs)
    ((x, t) :: (List.map (fun (v, u) -> (v, xt u)) s))

let unify : term -> term -> substitution = fun t u -> solve [(t, u)] []
