open Core (* LambdaPi core *)
open Extra

module D = Distribution

(** Inventory of signature, i.e. number of elements in a
    signature. *)
type inventory =
  { sym : int
  (** Number of symbols declared. *)
  ; rul : int
  (** Number of rules declared. *)
  ; nlr : int
  (** Number of nonlinear rules. *)
  ; hor : int
  (** Number of higher order rules. *) }
[@@deriving yojson]

(** The distributions built from a signature. *)
type distributions =
  { rul_size : D.t
  (** Size of rules. *)
  ; rul_height : D.t
  (** Height of rules. *) }

(** Contains aggregates of the distributions. *)
type agg_dist =
  { arul_size : D.aggregate
  (** Aggregate of rules. *)
  ; arul_height : D.aggregate
  (** Aggregate of heights. *) }
  [@@deriving yojson]

(** Inventory along with distributions. *)
type t =
  { sig_inv : inventory
  ; sig_dist : distributions }

(** Ready to print data of a signature. *)
type numeric =
  { catalogue : inventory
  (** Quantities *)
  ; stats : agg_dist
  (** Values computed from distributions. *) }
[@@deriving yojson]

let compile = Compile.compile true

(** [count_symbols s] counts the number of symbols declared in the
    signature [s]. *)
let count_symbols : Sign.t -> int = fun sign ->
  StrMap.cardinal Timed.(!(sign.sign_symbols))

(** [count_rules s] count the number of rules declared in the signature [s]. *)
let count_rules : Sign.t -> int = fun sign ->
  let rul_of_sym (sy:Terms.sym) = List.length Timed.(!(sy.sym_rules)) in
  StrMap.fold (fun _ (sy, _) -> (+) (rul_of_sym sy))
    Timed.(!(sign.sign_symbols)) 0

(** [nonlin r] returns true if rule [r] is left nonlinear. *)
let nonlin : Terms.rule -> bool = fun { lhs ; _ } ->
  let slots = List.to_seq lhs |>
                Seq.filter_map
                  (function Terms.Patt(io, _, _) -> io | _ -> None) |>
                List.of_seq in
  let slots_uniq = List.sort_uniq Int.compare slots in
  List.compare_lengths slots slots_uniq <> 0

(** [count_nlrules s] counts the number of non left linear rules in
    signature [s]. *)
let count_nlrules : Sign.t -> int = fun sign ->
  let nr_of_sym (sy:Terms.sym) =
    List.fold_left
      (fun acc rul -> if nonlin rul then acc + 1 else acc)
      0
      Timed.(!(sy.sym_rules)) in
  StrMap.fold (fun _ (sy, _) acc -> acc + (nr_of_sym sy))
    Timed.(!(sign.sign_symbols)) 0

(** [ho r] returns true if rule [r] contains higher order terms. *)
let ho : Terms.rule -> bool = fun { lhs ; _ } ->
  let rec ho te =
    let open Terms in
    match te with
    | Appl(t, u) -> ho t || ho u
    | Abst(_, _) -> true
    | _          -> false in
  List.exists ho lhs

(** [count_horules s] counts the number of higher order rules in
    signature [s]. *)
let count_horules : Sign.t -> int = fun sign ->
  let ho_of_sym (sy:Terms.sym) =
    List.fold_left
      (fun acc rul -> if ho rul then acc + 1 else acc)
      0
      Timed.(!(sy.sym_rules)) in
  StrMap.fold (fun _ (sy, _) acc -> acc + (ho_of_sym sy))
    Timed.(!(sign.sign_symbols)) 0

(** [rules_sizes s] returns the distribution of sizes of the rules in
    signature [s]. *)
let rules_sizes : Sign.t -> D.t = fun sign ->
  let open Terms in
  let sizes_of_sym (sy:sym) : int list =
    List.map (fun { lhs ; _ } -> List.length lhs) Timed.(!(sy.sym_rules)) in
  D.of_list @@
  StrMap.fold (fun _ (sy, _) acc -> (sizes_of_sym sy) @ acc)
    Timed.(!(sign.sign_symbols)) []

(** [height_of_rules r] returns the height of rule [r]. *)
let height_of_rule : Terms.rule -> int = fun { lhs ; _ } ->
  let open Terms in
  (** [depth t] returns the depth of term [t] defined as
      - [depth f t1 ... tn = 1 + max {depth t | t in t1 ... tn}]
      - [depth x = 0]. *)
  let rec depth : term -> int = function
    | Appl(u, v) -> max (depth u + 1) (depth v + 1)
    | Abst(_, u) -> let _, u = Bindlib.unbind u in
      depth u + 1
    | _          -> 0 in
  depth (Basics.add_args Kind lhs)

(** [rules_heights s] returns the distribution of heights of rules in
    signature [s]. *)
let rules_heights : Sign.t -> D.t = fun sign ->
  let heights_of_sym (sy:Terms.sym) : int list =
    List.map height_of_rule Timed.(!(sy.sym_rules)) in
  D.of_list @@
  StrMap.fold (fun _ (sy, _) acc -> heights_of_sym sy @ acc)
    Timed.(!(sign.sign_symbols)) []

(** [of_file f] computes statistics on rules of file [f]. *)
let of_file : string -> t = fun fname ->
  let mp = Files.module_path fname in
  compile mp ;
  let sign = Files.PathMap.find mp Sign.(Timed.(!loaded)) in
  let sig_inv =
    { sym = count_symbols sign
    ; rul = count_rules sign
    ; nlr = count_nlrules sign
    ; hor = count_horules sign } in
  let sig_dist =
    { rul_height = rules_heights sign
    ; rul_size = rules_sizes sign } in
  { sig_inv ; sig_dist }

let aggregate : distributions -> agg_dist =
  fun { rul_size ; rul_height } ->
  { arul_size = D.compute rul_size ; arul_height = D.compute rul_height }

(** [pp f d] pretty prints data [d] to formatter [f]. *)
let pp : Format.formatter -> t -> unit = fun fmt { sig_dist ; sig_inv } ->
  let stats = aggregate sig_dist in
  let num = { catalogue = sig_inv ; stats } in
  num |> numeric_to_yojson |> Yojson.Safe.pretty_print fmt
