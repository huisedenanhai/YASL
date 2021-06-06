open Syntax

type token = tm_info * string

let desc_string_of_token tk =
  let info, name = tk in
  string_of_tm_info info ^ ": " ^ name

exception TokenError of string

let raise_token_err tm_info desc =
  raise (TokenError (string_of_tm_info tm_info ^ ": " ^ desc))

let split_str_at str n =
  (String.sub str 0 n, String.sub str n (String.length str - n))

let strip_str str n =
  let _, tl = split_str_at str n in
  tl

let rec skip_until buf tm_info reg =
  if String.length buf = 0 then (buf, tm_info)
  else if Str.string_match reg buf 0 then (buf, tm_info)
  else if String.get buf 0 = '\n' then
    skip_until (strip_str buf 1) (info_inc_line tm_info) reg
  else skip_until (strip_str buf 1) (info_inc_column tm_info) reg

let rec skip_white_space buf tm_info =
  if String.length buf = 0 then (buf, tm_info)
  else if Str.string_match (Str.regexp "[ \t\r]") buf 0 then
    skip_white_space (strip_str buf 1) (info_inc_column tm_info)
  else if String.get buf 0 = '\n' then
    skip_white_space (strip_str buf 1) (info_inc_line tm_info)
  else (buf, tm_info)

let skip_comment buf tm_info =
  let buf, tm_info = skip_until buf tm_info (Str.regexp "\\*/") in
  (strip_str buf 2, info_inc_column_n tm_info 2)

let get_token_with_len buf tm_info len =
  let hd, tl = split_str_at buf len in
  ((tm_info, hd), tl, info_inc_column_n tm_info len)

let get_token buf tm_info =
  let do_split n = get_token_with_len buf tm_info n in
  let acc_result res pt =
    match res with
    | Some r -> Some r
    | None ->
        if Str.string_match (Str.regexp pt) buf 0 then
          Some (do_split (Str.match_end ()))
        else None
  in
  match
    [
      "[0-9]+\\(\\.[0-9]+\\)?";
      "[a-zA-Z_][a-zA-Z_0-9]*";
      "\\->";
      "\\[";
      "\\]";
      "[-\\+\\*/]";
      "[=()\\.:,{}]";
    ]
    |> List.fold_left acc_result None
  with
  | Some r -> r
  | None -> raise_token_err tm_info "invalid token."

let rec split_tokens buf tm_info =
  let buf, tm_info = skip_white_space buf tm_info in
  if String.length buf = 0 then []
  else if Str.string_match (Str.regexp "/\\*") buf 0 then
    let buf, tm_info = skip_comment buf tm_info in
    split_tokens buf tm_info
  else
    let tk, buf, tm_info = get_token buf tm_info in
    tk :: split_tokens buf tm_info

let str_to_tokens str = split_tokens str init_tm_info
