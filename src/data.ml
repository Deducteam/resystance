open Core
open Extra

type t =
  { cardinal : int }
  [@@deriving yojson]

let compile = Compile.compile true

let of_file : string -> t = fun fname ->
  let mp = Files.module_path fname in
  compile mp ;
  let sign = Files.PathMap.find mp Sign.(Timed.(!loaded)) in
  let cardinal = StrMap.cardinal Timed.(!(sign.sign_symbols)) in
  { cardinal }
