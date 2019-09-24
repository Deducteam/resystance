open Core
open Extra
open Timed

let _ =
  Console.set_default_debug "res"

(* It is possible to be more subtle than that, removing refs from
   head symbols should be enough. *)
(** [deep_untref t] removes all references from term [t]. *)
let rec deep_untref : Terms.term -> Terms.term = fun t ->
  match Basics.get_args t with
  | TRef(t), args ->
    begin match !t with
    | None    -> assert false
    | Some(t) -> Basics.add_args t (List.map deep_untref args)
    end
  | t      , args -> Basics.add_args t (List.map deep_untref args)

let solve = Unif.solve StrMap.empty true

(** [unifiable t u] returns whether [t] can be unified with [u]. *)
let unifiable : Terms.term -> Terms.term -> bool = fun t u ->
  let t = deep_untref t in
  let u = deep_untref u in
  let prob = { Unif.no_problems with Unif.to_solve = [(t, u)] } in
  solve prob <> None

(** [cps l lp] searches for critical peaks involving lhs [l] and
    subterms of lhs [lp]. *)
let rec cps : Terms.term -> Terms.term -> Terms.term list =
  fun l lp ->
  let open Terms in
  match Basics.get_args lp with
  | Meta(_, _)   , _
  | Patt(_, _, _), _ -> []
  | Symb(_, _), args
  | Abst(_, _), args
  | Vari(_)   , args ->
    let argunif = List.map (cps l) args |> List.flatten in
    if unifiable l lp then lp :: argunif else argunif
  | _         , _    -> assert false

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
