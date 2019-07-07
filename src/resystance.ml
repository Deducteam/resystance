let spec =
  let sp = Arg.align
             [ ] in
  List.sort (fun (f1, _, _) (f2, _, _) -> String.compare f1 f2) sp

let _ =
  let usage = Printf.sprintf "Usage: %s [OPTIONS] [FILES]" Sys.argv.(0) in
  let files = ref [] in
  Arg.parse spec (fun s -> files := s :: !files) usage ;
  let stats = List.map Data.of_file (!files) in
  let dy = List.map Data.to_yojson stats in
  List.iter
    (Yojson.Safe.pretty_print Format.std_formatter)
    dy ;
  print_newline ()
