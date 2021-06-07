open Parser
open Syntax

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let print_tokens tokens =
  List.iter (fun tk -> print_endline (desc_string_of_token tk)) tokens

let print_toplevel toplevel =
  List.iter (fun tp -> print_endline (desc_string_of_toplevel_term tp)) toplevel

let main =
  if Array.length Sys.argv < 2 then print_endline "please specify input file"
  else
    Sys.argv.(1) |> read_whole_file |> str_to_tokens |> parse_toplevel
    |> print_toplevel
