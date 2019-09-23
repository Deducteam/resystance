open Core
open Extra
open Timed

let solve = Unif.solve StrMap.empty false

(** [unifiable t u] returns whether [t] can be unified with [u]. *)
let unifiable : Terms.term -> Terms.term -> bool = fun t u ->
  let prob = { Unif.no_problems with Unif.to_solve = [(t, u)] } in
  solve prob <> None

(** [cps l lp] searches for critical peaks involving lhs [l] and
    subterms of lhs [lp]. *)
let rec cps : Terms.term -> Terms.term -> Terms.term list =
  fun l lp ->
  let open Terms in
  let unified = if unifiable l lp then [lp] else [] in
  match lp with
  | Appl(t, u) -> unified @ (cps l t) @ (cps l u)
  | Abst(_, b) -> let _, te = Bindlib.unbind b in unified @ (cps l te)
  | TRef(t)    ->
    begin match !t with
      | Some(t) -> unified @ (cps l t)
      | None    -> unified end
  | Symb(_, _)
  | Vari(_)    -> unified
  | Meta(_, _) -> unified
  | _          -> unified

let critical_pairs : Sign.t -> Terms.term list = fun sign ->
  let open Terms in
  let syms = !(sign.sign_symbols) |> StrMap.map fst in
  let term_of_lhs s =
    List.map (fun l -> Basics.add_args (Symb(s, Nothing)) l.lhs)
      !(s.sym_rules)
  in
  let lhs = StrMap.fold (fun _ s acc -> (term_of_lhs s) @ acc) syms [] in
  let noh t =
    let _, args = Basics.get_args t in
    List.flatten (List.map (cps t) args)
  in
  List.map noh lhs |> List.flatten
