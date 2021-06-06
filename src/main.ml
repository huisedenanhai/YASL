open Foo
open Parser
(* open Syntax *)

let () = hello ()

let () = hi ()

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let main =
  if Array.length Sys.argv < 2 then print_endline "please specify input file"
  else
    Sys.argv.(1) |> read_whole_file |> str_to_tokens
    |> List.iter (fun tk -> print_endline (desc_string_of_token tk))
