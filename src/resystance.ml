open Core

let spec =
  let sp = Arg.align
             [ ] in
  List.sort (fun (f1, _, _) (f2, _, _) -> String.compare f1 f2) sp

let _ =
  Console.set_default_verbose 0 ;
  let usage = Printf.sprintf "Usage: %s [OPTIONS] [FILES]" Sys.argv.(0) in
  let files = ref [] in
  Arg.parse spec (fun s -> files := s :: !files) usage ;
  let stats = List.map Data.of_file (!files) in
  let final = Data.merge stats in
  Data.pp Format.std_formatter final ;
  print_newline ()
