open Syntax

let gen_glsl toplevel =
  toplevel |> List.map desc_string_of_toplevel_term |> String.concat "\n"
