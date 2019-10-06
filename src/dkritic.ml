open Core
open Extra
module F = Format
module U = Unification

(** [sig_of_file f] returns the signature of the file path [f]. *)
let sig_of_file : string -> Sign.t = fun fname ->
  let mp = Files.module_path fname in
  let module C = Console in
  begin try Compile.compile true mp
    with C.Fatal(None, msg) -> C.exit_with "%s" msg
       | C.Fatal(Some(p), msg) -> C.exit_with "[%a] %s" Pos.print p msg end;
  Files.PathMap.find mp Sign.(Timed.(!loaded))

(** [syms_of_sig s] returns the list of symbols of signature [s]. *)
let syms_of_sig : Sign.t -> Terms.sym list = fun sign ->
  Timed.(!(sign.sign_symbols)) |> StrMap.to_seq |> Seq.map snd |> Seq.map fst |> List.of_seq

let spec =
  Arg.align []
let _ =
  let usage = Printf.sprintf "Usage: %s [OPTIONS] [FILES]" Sys.argv.(0) in
  let files = ref [] in
  Arg.parse spec (fun s -> files := s :: !files) usage;
  files := List.rev !files;
  let ppf = F.std_formatter in
  let pp_quadruple_s fmt (l1, l2, lr, s) =
    F.fprintf fmt "[%a] =? [%a] -> [%a] {%a}"
      Print.pp l1 Print.pp l2 Print.pp lr U.pp_subst s
  in
  let pp_sig_cp fmt scps =
    F.pp_print_list ~pp_sep:(F.pp_print_newline) pp_quadruple_s fmt scps
  in
  let pp_sep fmt () = F.fprintf fmt "\n%%%%\n" in
  let cps_of_file s = s |> sig_of_file |> syms_of_sig |> Critical_pairs.cps in
  let cps = List.map cps_of_file !files in
  F.pp_print_list ~pp_sep pp_sig_cp ppf cps;
  Format.fprintf ppf "\n"
