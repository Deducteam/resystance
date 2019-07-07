open Core
open Extra

(** Type of data and statistics. *)
type t =
  { sym_cardinal : int
  (** Number of symbols declared. *) }
  [@@deriving yojson]

let compile = Compile.compile true

(** [of_file f] computes statistics on rules of file [f]. *)
let of_file : string -> t = fun fname ->
  let mp = Files.module_path fname in
  compile mp ;
  let sign = Files.PathMap.find mp Sign.(Timed.(!loaded)) in
  let sym_cardinal = StrMap.cardinal Timed.(!(sign.sign_symbols)) in
  { sym_cardinal }

(** [pp f d] pretty prints data [d] to formatter [f]. *)
let pp : Format.formatter -> t -> unit = fun fmt d ->
  d |> to_yojson |> Yojson.Safe.pretty_print fmt
