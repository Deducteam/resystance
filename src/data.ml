open Core (* LambdaPi core *)
open Extra

module D = Distribution

(** Type of data and statistics. *)
type t =
  { sym_cardinal : int
  (** Number of symbols declared. *)
  ; rul_cardinal : int
  (** Number of rules declared. *)
  ; nlr_cardinal : int
  (** Number of nonlinear rules. *)
  ; hor_cardinal : int
  (** Number of higher order rules. *)
  ; rul_size : D.aggregate
  (** Size distribution of the rules. *)
  ; rul_height : D.aggregate
  (** Height distribution of the rules. *) }
  [@@deriving yojson]

(** Initial data. *)
let empty = { sym_cardinal = 0
            ; rul_cardinal = 0
            ; nlr_cardinal = 0
            ; hor_cardinal = 0
            ; rul_size = D.compute D.init
            ; rul_height = D.compute D.init }

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

(** [of_file f] computes statistics on rules of file [f]. *)
let of_file : string -> t = fun fname ->
  let mp = Files.module_path fname in
  compile mp ;
  let sign = Files.PathMap.find mp Sign.(Timed.(!loaded)) in
  let sym_cardinal = count_symbols sign in
  let rul_cardinal = count_rules sign in
  let nlr_cardinal = count_nlrules sign in
  let hor_cardinal = count_horules sign in
  let rul_size = rules_sizes sign |> D.compute in
  { empty with sym_cardinal ; rul_cardinal ; nlr_cardinal ; hor_cardinal
  ; rul_size }

(** [pp f d] pretty prints data [d] to formatter [f]. *)
let pp : Format.formatter -> t -> unit = fun fmt d ->
  d |> to_yojson |> Yojson.Safe.pretty_print fmt
