open Syntax

type token = tm_info * string

let is_ident str = Str.string_match (Str.regexp "[a-zA-Z_][a-zA-Z_0-9]*$") str 0

let desc_string_of_token tk =
  let info, name = tk in
  string_of_tm_info info ^ ": " ^ name

exception TokenError of string

exception ParseError of string

let raise_token_err tm_info desc =
  raise (TokenError (string_of_tm_info tm_info ^ ": " ^ desc))

let raise_parse_err tm_info desc =
  raise (ParseError (string_of_tm_info tm_info ^ ": " ^ desc))

let raise_parse_err_eof () = raise (ParseError "unexpected eof")

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

let rec parse_plain_type = function
  | [] -> raise_parse_err_eof ()
  | (_, "float") :: tl -> (TyFloat, tl)
  | (_, "int") :: tl -> (TyInt, tl)
  | (_, "bool") :: tl -> (TyBool, tl)
  | (_, "[") :: tl ->
      let tys, tl = parse_tuple_type tl in
      (TyTuple tys, tl)
  | (_, name) :: tl when is_ident name -> (TyCustom name, tl)
  | (info, _) :: _ -> raise_parse_err info "invalid plain type"

and parse_tuple_type = function
  | [] -> raise_parse_err_eof ()
  | (_, "]") :: tl -> ([], tl)
  | tks -> (
      let ty, tl = parse_plain_type tks in
      match tl with
      | [] -> raise_parse_err_eof ()
      | (_, ",") :: tl ->
          let tytl, tl = parse_tuple_type tl in
          (ty :: tytl, tl)
      | (_, "]") :: tl -> ([ ty ], tl)
      | (info, _) :: _ -> raise_parse_err info "invalid tuple type")

let id_of_tm_info info = (info.line * 100000) + info.column

let rec parse_type tks =
  let pt, tl = parse_plain_type tks in
  match tl with
  | (info, "->") :: tl ->
      let nt, tl = parse_type tl in
      (TyArrow (id_of_tm_info info, pt, nt), tl)
  | _ -> (TyPlain pt, tl)

let rec parse_record_type = function
  | [] -> raise_parse_err_eof ()
  | (_, "}") :: tl -> ([], tl)
  | (_, label) :: (_, ":") :: tl when is_ident label -> (
      let ty, tl = parse_plain_type tl in
      match tl with
      | [] -> raise_parse_err_eof ()
      | (_, "}") :: tl -> ([ (label, ty) ], tl)
      | (_, ",") :: tl ->
          let kts, tl = parse_record_type tl in
          ((label, ty) :: kts, tl)
      | (info, _) :: _ -> raise_parse_err info "invalid record type")
  | (info, _) :: _ -> raise_parse_err info "invalid record type"

let parse_type_declare name = function
  | (_, "[") :: tl ->
      let tys, tl = parse_tuple_type tl in
      (TyDeclTuple (name, tys), tl)
  | (_, "{") :: tl ->
      let kts, tl = parse_record_type tl in
      (TyDeclRecord (name, kts), tl)
  | [] -> raise_parse_err_eof ()
  | (info, _) :: _ -> raise_parse_err info "invalid type declare"

let parse_term tks = (TmAtom (init_tm_info, FloatLiteral 1.0), tks)

let rec parse_toplevel = function
  | [] -> []
  | (info, "uniform") :: (_, name) :: (_, ":") :: tks when is_ident name ->
      let ty, tks = parse_plain_type tks in
      TopTmUnfiorm (info, name, ty) :: parse_toplevel tks
  | (info, "extern") :: (_, name) :: (_, ":") :: tks when is_ident name ->
      let ty, tks = parse_type tks in
      TopTmExtern (info, name, ty) :: parse_toplevel tks
  | (info, "type") :: (_, name) :: tks when is_ident name ->
      let decl, tks = parse_type_declare name tks in
      TopTmTyDeclare (info, decl) :: parse_toplevel tks
  | (info, "let") :: (_, name) :: tks when is_ident name ->
      let tm, tks = parse_term tks in
      TopTmLet (info, name, tm) :: parse_toplevel tks
  | (info, "entry") :: (_, name) :: tks when is_ident name ->
      let tm, tks = parse_term tks in
      TopTmEntry (info, name, tm) :: parse_toplevel tks
  | (info, _) :: _ -> raise_parse_err info "invalid top level declare"
