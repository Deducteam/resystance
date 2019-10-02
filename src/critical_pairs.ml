open Core
open Extra
open Timed
open Terms

module U = Unification

type rhs = (term_env, term) Bindlib.mbinder

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

let rec deep_untenv : term -> term = function
  | Appl(t, u) -> Appl(deep_untenv t, deep_untenv u)
  | TEnv(TE_Some(b), ar) -> deep_untenv (Bindlib.msubst b ar)
  | t -> t

let solve = Unif.solve StrMap.empty false

(** [unifiable t u] returns a unifier of [t =? u] or [None]. *)
let unifiable : term -> term -> U.substitution option = fun t u ->
  let t = deep_untref t in
  let u = deep_untref u in
  Format.printf "Unifying [%a =? %a]... " Print.pp_term t Print.pp_term u;
  let mgu =
    try Some(Unification.unify t u) with
    | Unification.CantUnify -> None
  in
  Format.printf (match mgu with Some(_) -> "success\n" | None -> "failure\n");
  mgu

(** [build_r_subst t s] builds an array to perform bindlib
    substitution into Dedukti3 style rhs. *)
let build_r_subst : term -> U.substitution -> int -> term_env array =
  fun t s size ->
  let rec loop t =
    match Basics.get_args t with
    | Patt(Some(i), v, _), args ->
      let rest = List.map loop args |> List.concat in
      Format.printf "%s/{%a}\n" v U.pp_subst s;
      if U.indom v s then (i, U.app s v) :: rest else rest
    | _, args -> List.map loop args |> List.concat
  in
  let assoc = loop t in
  let ar = Array.make size TE_None in
  let f (i, t) =
    let b = Bindlib.raw_mbinder [||] [||] 0 mkfree (fun _ -> unfold t) in
    ar.(i) <- TE_Some(b)
  in
  List.iter f assoc;
  ar

(** [subterms_of t] returns the subterms of term [t] which are not rewriting
    variables. *)
let rec subterms_of : term -> term list = fun t ->
  match Basics.get_args t with
  | Meta(_, _)   , _ -> []
  | Patt(_, _, _), _ -> []
  | Symb(_, _), args
  | Abst(_, _), args
  | Vari(_)   , args -> t :: (List.map subterms_of args |> List.concat)
  | _ -> assert false

(** [cps l1 l2] searches for critical peaks involving lhs [l1] and
    subterms of lhs [l2].  A returned quadruplet [(l1r, l2r, ls, s)]
    contains
    - [l1r] [l1] renamed;
    - [l2r] [l2] renamed;
    - [ls] [l2] renamed with the unifier [s] applied;
    - [s] the unifier. *)
let cps : (term * rhs) -> (term * rhs)
  -> (term * term * term * U.substitution) list =
  fun (l1, r1) (l2, r2) ->
  let _, _ = r1, r2 in
  let l1 = U.rename l1 in
  let l2 = U.rename l2 in
  let substs =
    subterms_of l2 |> List.filter_map (unifiable l1)
  in
  List.map (fun s -> (l1, l2, U.lift s l2, s)) substs


(** [critical_pairs s] returns a list of critical pairs emerging from
    rewrite rules of signature [s]. *)
let critical_pairs : Sign.t -> (term * term * term * U.substitution) list =
  fun sign ->
  let syms = !(sign.sign_symbols) |> StrMap.map fst in
  (* Build terms from lhs of rules of symbol [s]. *)
  let term_of_lhs s =
    List.to_seq !(s.sym_rules)
    |> Seq.map (fun l -> (Basics.add_args (Symb(s, Nothing)) l.lhs, l.rhs))
  in
  let lhs =
    StrMap.to_seq syms |> Seq.map snd |> Seq.flat_map term_of_lhs |> List.of_seq
  in
  let f (l1, r1) =
    List.map (fun (l2, r2) -> cps (l1, r1) (l2, r2)) lhs
    |> List.flatten
  in
  List.map f lhs |> List.flatten
