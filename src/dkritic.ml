open Core
open Lplib
open Lplib.Extra
open Console
module F = Format
module U = Unification
module CP = Critical_pairs

(** [sig_of_file f] returns the signature of the file path [f]. *)
let sig_of_file : string -> Sign.t = fun fname ->
  let mp = Files.file_to_module fname in
  let module C = Console in
  C.handle_exceptions(fun()-> ignore(Compile.compile true mp));
  Files.PathMap.find mp Sign.(Timed.(!loaded))

(** [syms_of_sig s] returns the list of symbols of signature [s]. *)
let syms_of_sig : Sign.t -> Terms.sym list = fun sign ->
  Timed.(!(sign.sign_symbols)) |> StrMap.bindings
  |> List.map (fun x -> fst (snd x))

let debug_flags =
  let fn acc l = acc ^ "\n        " ^ l in
  List.fold_left fn "\n      Available flags:" (Console.log_summary ())

let verbose_values = "\n" ^ String.concat "\n"
    [ "      Available values:"
    ; "        ≤ 0 : no output at all"
    ; "        = 1 : only file loading information (default)"
    ; "        = 2 : more file loading information"
    ; "        ≥ 3 : show the results of commands" ]

let spec =
  Arg.align
    [ ( "--verbose"
      , Arg.Int set_default_verbose
      , "<int> Set the default verbosity level" ^ verbose_values )
    ; ( "--debug"
      , Arg.String set_default_debug
      , "<flags> Enables given debugging flags by default " ^ debug_flags ) ]

let _ =
  let usage = Printf.sprintf "Usage: %s [OPTIONS] [FILES]" Sys.argv.(0) in
  let files = ref [] in
  Arg.parse spec (fun s -> files := s :: !files) usage;
  files := List.rev !files;
  let ppf = F.std_formatter in
  let term_of_lhs (s,r) =
    let open Terms in
    Basics.add_args (Symb(s, Nothing)) r.lhs
  in
  let pp_rule_conflict fmt (p,r1,r2) =
    F.fprintf fmt "%% LHS1: %a\n%% LHS2: %a\n%% subterm path in LHS2: [%a]"
      Print.pp_term (term_of_lhs r1) Print.pp_term (term_of_lhs r2)
      (F.pp_print_list ~pp_sep:(fun fmt()->F.pp_print_string fmt" ") CP.pp_path_elt) p in
  let open_cp = ref 0 in
  let pp_critical fmt cp =
    let (lhs,(p,r1,rhs1),(r2,rhs2)) = CP.reduce cp in
    if Eval.eq_modulo rhs1 rhs2 then
      (* display LHS and RHSs only *)
      F.eprintf (*fmt*) "%% OK\n%a\n[%a]\n +-1-> [%a]\n |     ==\n +-2-> [%a]\n"
        pp_rule_conflict (p,r1,r2)
        Print.pp_term lhs Print.pp_term rhs1 Print.pp_term rhs2
    else
      (incr open_cp;
      (* display LHS, RHSs and their SNFs *)
      F.fprintf fmt "%% KO\n%a\n[%a]\n +-1-> [%a]\n |     +->* [%a]\n |          !=\n +-2-> [%a]\n       +->* [%a]\n"
        pp_rule_conflict (p,r1,r2)
        Print.pp_term lhs Print.pp rhs1 Print.pp_term (Eval.snf rhs1)
        Print.pp_term rhs2 Print.pp_term (Eval.snf rhs2))
  in
  let pp_sig_cp fmt scps =
    F.pp_print_list ~pp_sep:(F.pp_print_newline) pp_critical fmt scps
  in
(*  let pp_sep fmt () = F.fprintf fmt "\n%%%%\n" in
  let cps_of_file s = s |> List.map sig_of_file |> syms_of_sig |> CP.cps in
  let cps = List.map cps_of_file !files in
  F.pp_print_list ~pp_sep pp_sig_cp ppf cps;*)
  let cps =
    !files |> List.map sig_of_file |> List.map syms_of_sig |> List.flatten
    |> Critical_pairs.cps in
  pp_sig_cp ppf cps;
  Format.fprintf ppf "\n";
  if Timed.(!)Console.log_enabled then
    CP.log_cp "%d pairs do not join\n" !open_cp;
