module C = Core

type t =
  { cardinal : int }

let compile = C.Compile.compile true

let of_file : string -> t = fun fname ->
  let mp = C.Files.module_path fname in
  compile mp ;
  let sign = C.Files.PathMap.find mp C.Sign.(Timed.(!loaded)) in
  { cardinal = 0 }
