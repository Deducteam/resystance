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
  ; rul_size : int D.t
  (** Size distribution of the rules. *)
  ; rul_height : int D.t
  (** Height distribution of the rules. *) }
  [@@deriving yojson]

(** Initial data. *)
let empty = { sym_cardinal = 0
            ; rul_cardinal = 0
            ; nlr_cardinal = 0
            ; hor_cardinal = 0
            ; rul_size = D.init 0
            ; rul_height = D.init 0 }

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

(** [of_file f] computes statistics on rules of file [f]. *)
let of_file : string -> t = fun fname ->
  let mp = Files.module_path fname in
  compile mp ;
  let sign = Files.PathMap.find mp Sign.(Timed.(!loaded)) in
  let sym_cardinal = count_symbols sign in
  let rul_cardinal = count_rules sign in
  { empty with sym_cardinal ; rul_cardinal }

(** [pp f d] pretty prints data [d] to formatter [f]. *)
let pp : Format.formatter -> t -> unit = fun fmt d ->
  d |> to_yojson |> Yojson.Safe.pretty_print fmt
