open Core

module F = Format

(** Whether to output as a csv line. *)
let csv : bool ref = ref false

(** Whether to output the stats of each file as csv lines. *)
let separate : bool ref = ref false

let spec =
  let sp = Arg.align
      [ ( "--csv"
        , Arg.Set csv
        , " Output as a csv line" )
      ; ( "--separate"
        , Arg.Set separate
        , " One csv output line per file" ) ] in
  List.sort (fun (f1, _, _) (f2, _, _) -> String.compare f1 f2) sp

let _ =
  Console.set_default_verbose 0 ;
  let usage = Printf.sprintf "Usage: %s [OPTIONS] [FILES]" Sys.argv.(0) in
  let files = ref [] in
  Arg.parse spec (fun s -> files := s :: !files) usage ;
  let stats = List.map Data.of_file (!files) in
  let ppf = F.std_formatter in
  let pp = if !csv || !separate
           then begin
               Format.pp_print_string ppf Data.csv_hdr ;
               Format.pp_print_newline ppf () ;
               Data.pp_csv end
           else Data.pp
  in
  if !separate then List.iter (F.fprintf ppf "%a\n" pp) stats else
  List.fold_right Data.merge stats Data.empty |>
  F.fprintf ppf "%a\n" pp
