open Core
open Extra
open Timed
open Terms

module U = Unification

(* It is possible to be more subtle than that, removing refs from
   head symbols should be enough. *)
(** [deep_untref t] removes all references from term [t]. *)
let rec deep_untref : term -> term = fun t ->
  match Basics.get_args t with
  | TRef(t), args ->
    begin match !t with
    | None    -> assert false
    | Some(t) -> Basics.add_args t (List.map deep_untref args)
    end
  | t      , args -> Basics.add_args t (List.map deep_untref args)

(** [unify t u] returns a unifier of [t =? u] or [None]. *)
let unify : term -> term -> U.substitution option = fun t u ->
  let t = deep_untref t in
  let u = deep_untref u in
  Format.printf "Unifiying [%a =? %a]\n" Print.pp t Print.pp u;
  try Some(Unification.unify t u)
  with Unification.CantUnify -> None

(** [subterms_of t] returns the subterms of term [t] which are not rewriting
    variables.

    [subterms_of (f a b) = [(f a b); a; b]]. *)
let rec subterms_of : term -> term list = fun t ->
  match Basics.get_args t with
  | Meta(_)  , _    -> []
  | Patt(_)  , _    -> []
  | Symb(_)  , args
  | Vari(_)  , args -> t :: (List.map subterms_of args |> List.concat)
  | Abst(_,u), []   ->
    let _, u = Bindlib.unbind u in
    t :: subterms_of u
  | Abst(_)  , _    -> failwith "Pattern not in β normal form"
  | _               -> assert false

(** [sizeof t] computes the size of term [t] (number of symbols). *)
let rec sizeof : term -> int = fun t ->
  match Basics.get_args t with
  | Abst(_, t), [] ->
    let _, t = Bindlib.unbind t in
    1 + sizeof t
  | Abst(_)   , _  -> failwith "Pattern not in β normal form"
  | _         , tl -> List.fold_right (fun e acc -> sizeof e + acc) tl 1

(** [cps l1 l2] searches for critical peaks involving lhs [l1] and
    subterms of lhs [l2].  A returned quadruple [(l1r, l2r, ls, s)]
    contains
    - [l1r] [l1] renamed;
    - [l2r] [l2] renamed;
    - [ls] [l2] renamed with the unifier [s] applied;
    - [s] the unifier. *)
let cps : term -> term -> (term * term * term * U.substitution) list =
  fun l1 l2 ->
  let l1 = U.rename l1 in
  let l2 = U.rename l2 in
  let l1size = sizeof l1 in
  subterms_of l2
  (* sizeof t = sizeof l1 <=> t = l1 (because t is a subterm of l1) *)
  |> List.filter (fun t -> l1size <> (sizeof t))
  |> List.filter_map (unify l1)
  |> List.map (fun s -> (l1, l2, U.lift s l2, s))

let cps : sym list -> (term * term * term * U.substitution) list = fun syms ->
  let term_of_lhs s =
    List.to_seq !(s.sym_rules)
    |> Seq.map (fun l -> Basics.add_args (Symb(s, Nothing)) l.lhs)
  in
  let lhs =
    List.to_seq syms |> Seq.flat_map term_of_lhs |> List.of_seq
  in
  let f l1 =
    List.map (fun l2 -> cps l1 l2) lhs |> List.flatten
  in
  List.map f lhs |> List.flatten
