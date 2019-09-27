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

let solve = Unif.solve StrMap.empty false

(** [unifiable t u] returns whether [t] can be unified with [u]. *)
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

(** [cps l lp] searches for critical peaks involving lhs [l] and
    subterms of lhs [lp]. *)
let rec cps : term -> term -> (term * term * term * U.substitution) list =
  fun l lp ->
  match Basics.get_args (U.rename lp) with
  | Meta(_, _)   , _
  | Patt(_, _, _), _ -> []
  | Symb(_, _), args
  | Abst(_, _), args
  | Vari(_)   , args ->
    let argunif = List.map (cps l) args |> List.flatten in
    if l == lp then argunif else (* Don't compare same terms *)
    begin match unifiable l lp with
      | Some(s) -> (l, lp, U.lift s l, s) :: argunif
      | None    -> argunif
    end
  | _         , _    -> assert false

let critical_pairs : Sign.t -> (term * term * term * U.substitution) list =
  fun sign ->
  let syms = !(sign.sign_symbols) |> StrMap.map fst in
  (* Build terms from lhs of rules of symbol [s]. *)
  let term_of_lhs s =
    List.to_seq !(s.sym_rules)
    |> Seq.map (fun l -> Basics.add_args (Symb(s, Nothing)) l.lhs)
  in
  let lhs =
    StrMap.to_seq syms |> Seq.map snd |> Seq.flat_map term_of_lhs |> List.of_seq
  in
  let f l1 = List.map (fun l2 -> cps l1 l2) lhs |> List.flatten in
  List.map f lhs |> List.flatten
