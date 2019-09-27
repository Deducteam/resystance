open Core
open Terms

type vname = string

type substitution = (vname * term) list

let indom : vname -> substitution -> bool = fun vn->
  List.exists (fun (y, _) -> y = vn)

let rec app : substitution -> vname -> term = fun s x ->
  match s with
  | (v, t) :: _ when v = x -> t
  | _ :: tl                -> app tl x
  | []                     -> invalid_arg "app"

let rec lift : substitution -> term -> term = fun s t ->
  let h, args = Basics.get_args t in
  match h, args with
  | Patt(_, v, _), _  -> app s v
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
    | (Patt(_) as v,_) , (t        , _) when v = t  -> solve tl s
    | (Patt(_,x,_) ,_) , (t        , _)             -> elim x t tl s
    | _ -> assert false end
  | [] -> s

and elim : vname -> term -> (term * term) list -> substitution -> substitution =
  fun x t eqs s ->
  if occurs x t then raise CantUnify else
  let xt = lift [(x, t)] in
  solve (List.map (fun (u, u') -> (xt u, xt u')) eqs)
    ((x, t) :: (List.map (fun (v, u) -> (v, xt u)) s))

let unify : term -> term -> substitution = fun t u -> solve [(t, u)] []
