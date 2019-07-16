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
  { fname : string option
  ; sig_inv : inventory
  ; sig_dist : distributions }

(** Ready to print data of a signature. *)
type numeric =
  { catalogue : inventory
  (** Quantities *)
  ; stats : agg_dist
  (** Values computed from distributions. *) }
[@@deriving yojson]

let empty_inventory : inventory =
  { sym = 0
  ; rul = 0
  ; nlr = 0
  ; hor = 0 }

let empty_distributions : distributions =
  { rul_size = D.empty
  ; rul_height = D.empty }

let empty = { fname = None ; sig_inv = empty_inventory
            ; sig_dist = empty_distributions }

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
    | Appl(u, v) -> max (depth u) (depth v) + 1
    | Abst(_, u) -> let _, u = Bindlib.unbind u in
      depth u + 1
    | _          -> 0 in
  depth (Basics.add_args Kind lhs) - 1

(** [rules_heights s] returns the distribution of heights of rules in
    signature [s]. *)
let rules_heights : Sign.t -> D.t = fun sign ->
  let heights_of_sym (sy:Terms.sym) : int list =
    List.map height_of_rule Timed.(!(sy.sym_rules)) in
  D.of_list @@
  StrMap.fold (fun _ (sy, _) acc -> heights_of_sym sy @ acc)
    Timed.(!(sign.sign_symbols)) []

(** [aggregate d] transforms distributions [d] into numerical
    statistics. *)
let aggregate : distributions -> agg_dist =
  fun { rul_size ; rul_height } ->
  { arul_size = D.compute rul_size ; arul_height = D.compute rul_height }

(** [of_file f] computes statistics on rules of file [f]. *)
let of_file : string -> t = fun fname ->
  let mp = Files.module_path fname in
  begin let module C = Console in
    try compile mp
    with C.Fatal(None,    msg) -> C.exit_with "%s" msg
       | C.Fatal(Some(p), msg) -> C.exit_with "[%a] %s" Pos.print p msg end ;
  let sign = Files.PathMap.find mp Sign.(Timed.(!loaded)) in
  let sig_inv =
    { sym = count_symbols sign
    ; rul = count_rules sign
    ; nlr = count_nlrules sign
    ; hor = count_horules sign } in
  let sig_dist =
    { rul_height = rules_heights sign
    ; rul_size = rules_sizes sign } in
  { fname = Some(fname) ; sig_inv ; sig_dist }

(** [inventory_merge i j] merges inventories [i] and [j] into one. *)
let inventory_merge : inventory -> inventory -> inventory = fun i j ->
  { sym = i.sym + j.sym
  ; rul = i.rul + j.rul
  ; nlr = i.nlr + j.nlr
  ; hor = i.hor + j.hor }

(** [distributions_merge d e] merges distributions [d] and [e] into one. *)
let distributions_merge : distributions -> distributions -> distributions =
  fun d e ->
  { rul_size = D.merge d.rul_size e.rul_size
  ; rul_height = D.merge d.rul_height e.rul_height }

(** [merge d e] merge datasets [d] and [e] into one. *)
let merge : t list -> t =
  List.fold_left
    (fun acc elt -> { fname = None
                    ; sig_inv = inventory_merge acc.sig_inv elt.sig_inv
                    ; sig_dist = distributions_merge acc.sig_dist
                          elt.sig_dist }) empty

(** [pp f d] pretty prints data [d] to formatter [f]. *)
let pp : Format.formatter -> t -> unit =
  fun fmt { sig_dist ; sig_inv ; _ } ->
  let stats = aggregate sig_dist in
  { catalogue = sig_inv ; stats } |>
  numeric_to_yojson |> Yojson.Safe.pretty_print fmt

(** [pp_csv f d] outputs a line in csv format containing some of the
    information in a dataset. *)
let pp_csv : Format.formatter -> t -> unit =
  fun fmt { fname ; sig_dist ; sig_inv } ->
  let open Yojson.Safe.Util in
  let stats = aggregate sig_dist in
  let stj = { catalogue = sig_inv ; stats } |> numeric_to_yojson in
  let fnm = match fname with Some(s) -> s | None -> "N/A" in
  let cat = stj |> member "catalogue" in
  let sym = cat |> member "sym" |> to_int in
  let rul = cat |> member "rul" |> to_int in
  let nlr = cat |> member "nlr" |> to_int in
  let hor = cat |> member "hor" |> to_int in
  Format.fprintf fmt "%s, %d, %d, %d, %d" fnm sym rul nlr hor
