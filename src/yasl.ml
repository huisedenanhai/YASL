open Parser
open Syntax
open Ty
open Codegen

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let print_tokens tokens =
  List.iter (fun tk -> print_endline (desc_string_of_token tk)) tokens

let print_toplevel toplevel =
  List.iter
    (fun tp -> print_endline (desc_string_of_toplevel_term tp))
    toplevel

let get_tp_tms_from_file file =
  file
  |> read_whole_file
  |> str_to_tokens
  |> parse_toplevel
  |> type_toplevel builtin_ctx

let main =
  let input_file = ref "" in
  let output_file = ref "~" in
  let dump_ast = ref false in
  let emit_uniform = ref false in
  let speclist =
    [
      ("-ast", Arg.Set dump_ast, "dump ast");
      ("-o", Arg.Set_string output_file, "set output file name");
      ("-u", Arg.Set emit_uniform, "emit uniform");
    ]
  in
  let usage_msg = "yasl [-ast] filename [-o output]" in
  let anon_fun filename = input_file := filename in
  Arg.parse speclist anon_fun usage_msg;
  let tp_tms = get_tp_tms_from_file !input_file in
  if !dump_ast then print_toplevel tp_tms else ();
  let glsl = gen_glsl tp_tms !emit_uniform in
  let output_file =
    if !output_file = "~" then !input_file ^ ".gen.frag" else !output_file
  in
  let oc = open_out output_file in
  Printf.fprintf oc "%s\n" glsl;
  close_out oc
