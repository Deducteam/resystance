open Core (* LambdaPi core *)
open Lplib.Base
open Lplib.Extra

module D = Distribution

(** The CSV field separator. *)
let csv_sep = ","

(** A record containing all data from a file (or several files). *)
type t =
  { fname : string option
  (** Filename if separated output *)
  ; sym : int
  (** Number of symbols declared *)
  ; rul : int
  (** Number of rules declared *)
  ; nlr : int
  (** Number of nonlinear rules *)
  ; hor : int
  (** Number of higher order rules *)
  ; ari : D.t
  (** Distribution of the arity of rules*)
  ; siz : D.t
  (** Distribution of the size of the rules *)
  ; hgt : D.t
  (** Distribution of the height of the rules. *) }

let empty : t =
  { fname = None
  ; sym = 0
  ; rul = 0
  ; nlr = 0
  ; hor = 0
  ; ari = D.empty
  ; siz = D.empty
  ; hgt = D.empty }

(** [count_symbols s] counts the number of symbols declared in the
    signature [s]. *)
let count_symbols : Sign.t -> int = fun sign ->
  StrMap.cardinal Timed.(!(sign.sign_symbols))

(** [count_rules s] count the number of rules declared in the signature [s]. *)
let count_rules : Sign.t -> int = fun sign ->
  let rul_of_sym (sy:Term.sym) = List.length Timed.(!(sy.sym_rules)) in
  StrMap.fold (fun _ (sy, _) -> (+) (rul_of_sym sy))
    Timed.(!(sign.sign_symbols)) 0

(** [nonlin r] returns true if rule [r] is left nonlinear. *)
let nonlin : Term.rule -> bool = fun { lhs ; _ } ->
  let slots =
    List.filter_map (function Term.Patt(io, _, _) -> io | _ -> None) lhs
  in
  let slots_uniq = List.sort_uniq Int.compare slots in
  List.compare_lengths slots slots_uniq <> 0

(** [count_nlrules s] counts the number of non left linear rules in
    signature [s]. *)
let count_nlrules : Sign.t -> int = fun sign ->
  let nr_of_sym (sy:Term.sym) =
    List.fold_left
      (fun acc rul -> if nonlin rul then acc + 1 else acc)
      0
      Timed.(!(sy.sym_rules)) in
  StrMap.fold (fun _ (sy, _) acc -> acc + (nr_of_sym sy))
    Timed.(!(sign.sign_symbols)) 0

(** [ho r] returns true if rule [r] contains higher order terms. *)
let ho : Term.rule -> bool = fun { lhs ; _ } ->
  let rec ho te =
    let open Term in
    match te with
    | Appl(t, u) -> ho t || ho u
    | Abst(_, _) -> true
    | _          -> false in
  List.exists ho lhs

(** [count_horules s] counts the number of higher order rules in
    signature [s]. *)
let count_horules : Sign.t -> int = fun sign ->
  let ho_of_sym (sy:Term.sym) =
    List.fold_left
      (fun acc rul -> if ho rul then acc + 1 else acc)
      0
      Timed.(!(sy.sym_rules)) in
  StrMap.fold (fun _ (sy, _) acc -> acc + (ho_of_sym sy))
    Timed.(!(sign.sign_symbols)) 0

(** [height_of_rules r] returns the height of rule [r]. *)


let height_of_rule : Term.rule -> int = fun { lhs ; _ } ->
  let open Term in
  (* [depth t] returns the depth of term [t] defined as
     - [depth f t1 ... tn = 1 + max {depth t | t in t1 ... tn}]
     - [depth x = 0]. *)
  let rec depth : term -> int = function
    | Appl(u, v) -> max (depth u) (depth v) + 1
    | Abst(_, u) -> let _, u = Bindlib.unbind u in
      depth u + 1
    | _          -> 0 in
  depth (Term.add_args mk_Kind lhs) - 1




(** [rules_heights s] returns the distribution of heights of rules in
    signature [s]. *)
let rules_heights : Sign.t -> D.t = fun sign ->
  let heights_of_sym (sy:Term.sym) : int list =
    List.map height_of_rule Timed.(!(sy.sym_rules)) in
  D.of_list @@
  StrMap.fold (fun _ (sy, _) acc -> heights_of_sym sy @ acc)
    Timed.(!(sign.sign_symbols)) []







(** [size_of_rule r] returns the size of the lhs of [r], the size
 ** being the number of (sub) terms. *)
let size_of_rule : Term.rule -> int = fun { lhs ; _ } ->
  let open Term in
  let rec sot : term -> int = function
    | Appl(u, v)    -> (sot u) + (sot v)
    | Abst(_, u)    -> let _, u = Bindlib.unbind u in sot u + 1
    | Symb(_)    -> 1
    | Vari(_)       -> 1
    | Patt(_, _, _) -> 1
    | Meta(_, _)    -> 1
    | Kind          -> 0 (* Really? *)
    | _             -> assert false in
  sot (Term.add_args mk_Kind lhs) - (sot mk_Kind)

(** [rules_arity s] returns the distribution of the arity of the root symbol
    of the rules in signature [s]. *)
let rules_arity : Sign.t -> D.t = fun sign ->
  let open Term in
  let sizes_of_sym (sy:sym) : int list =
    List.map (fun { lhs ; _ } -> List.length lhs) Timed.(!(sy.sym_rules)) in
  D.of_list @@
  StrMap.fold (fun _ (sy, _) acc -> (sizes_of_sym sy) @ acc)
    Timed.(!(sign.sign_symbols)) []

(** [rules_size s] returns the distribution of sizes of rules in
 ** signature [s]. *)
let rules_sizes : Sign.t -> D.t = fun sign ->
  let sizes_of_sym sy =
    List.map size_of_rule Timed.(Term.(!(sy.sym_rules))) in
  D.of_list @@
    StrMap.fold (fun _ (sy, _) acc -> sizes_of_sym sy @ acc)
  Timed.(!(sign.sign_symbols)) []

(** [of_sig s] computes statistics on rules of signature [s]. *)
let of_sig : Sign.t -> t = fun sign ->
  let spp_path () mp = String.concat "." mp in
  let fname = Format.sprintf "%a" spp_path sign.Sign.sign_path in
  { fname = Some(fname)
  ; sym = count_symbols sign
  ; rul = count_rules sign
  ; nlr = count_nlrules sign
  ; hor = count_horules sign
  ; ari = rules_arity sign
  ; siz = rules_sizes sign
  ; hgt = rules_heights sign }

(** [merge d e] merges datasets [d] and [e] into one. *)
let merge : t -> t -> t = fun d e ->
  { fname = None
  ; sym = d.sym + e.sym
  ; rul = d.rul + e.rul
  ; nlr = d.nlr + e.nlr
  ; hor = d.hor + e.hor
  ; ari = D.merge d.ari e.ari
  ; siz = D.merge d.siz e.siz
  ; hgt = D.merge d.hgt e.hgt }

(** [pp f d] pretty prints data [d] to formatter [f]. *)
let pp : Format.formatter -> t -> unit = fun fmt d ->
  let module F = Format in
  F.fprintf fmt "@[<v>SUMMARY@," ;
  begin match d.fname with
  | Some(n) -> F.fprintf fmt "File: %s@," n
  | None    -> () end ;
  F.fprintf fmt "Symbols         : %d@," d.sym ;
  F.fprintf fmt "Rules           : %d@," d.rul ;
  F.fprintf fmt "Non linear rules: %d@," d.nlr ;
  F.fprintf fmt "HO rules        : %d" d.hor ;
  F.fprintf fmt "@]@."

(** [csv_hdr_distrib l] returns the csv header containing column
 ** fields for distributions prefixed by a label. *)
let csv_hdr_distrib : string -> string = fun label ->
  List.map ((^) label) ["_avg"; "_25th_pct"; "_med"; "_75th_pct"] |>
  String.concat csv_sep

let csv_hdr : string =
  let main =
    String.concat csv_sep
      ["File"; "Symbols"; "Rules"; "NL_rules"; "HO_rules"] in
  let ari = csv_hdr_distrib "Arity" in
  let siz = csv_hdr_distrib "Size" in
  let hgt = csv_hdr_distrib "Height" in
  main ^ ari ^ hgt ^ siz

(** [pp_distr_csv fmt distr] pretty prints distribution [distr] to
 ** formatter [fmt]. *)
let pp_distr_csv : Format.formatter -> D.t -> unit = fun fmt d ->
  let module F = Format in
  let open D in
  let pp_sep fmt () = F.pp_print_string fmt "," in
  F.fprintf fmt "%f%a%a" (average d) pp_sep ()
    (F.pp_print_list ~pp_sep F.pp_print_int)
    [percentile 25 d; percentile 50 d; percentile 75 d]

(** [pp_csv f d] outputs a line in csv format containing some of the
    information in a dataset. *)
let pp_csv : Format.formatter -> t -> unit = fun fmt d ->
  let module F = Format in
  let fname = match d.fname with Some(f) -> f | None -> "N/A" in
  let pp_sep fmt () = F.pp_print_string fmt csv_sep in
  F.fprintf fmt "%s%s%a%s%a%s%a%s%a"
    fname
    csv_sep
    (F.pp_print_list ~pp_sep F.pp_print_int) [d.sym; d.rul; d.nlr; d.hor]
    csv_sep
    pp_distr_csv d.ari
    csv_sep
    pp_distr_csv d.hgt
    csv_sep
    pp_distr_csv d.siz
