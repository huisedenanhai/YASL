open Syntax

type token = tm_info * string

let keywords =
  [ "fn"; "let"; "in"; "uniform"; "entry"; "extern"; "if"; "else"; "then" ]

let is_keyword str = List.exists (fun k -> k = str) keywords

let is_ident str =
  (not (is_keyword str))
  && Str.string_match (Str.regexp "[a-zA-Z_][a-zA-Z_0-9]*$") str 0

let is_int_literal str = Str.string_match (Str.regexp "[0-9]+$") str 0

let is_number_literal str =
  Str.string_match (Str.regexp "[0-9]+\\(\\.[0-9]+\\)?$") str 0

let is_float_literal str = is_number_literal str && not (is_int_literal str)

let is_bool_literal str = str = "true" || str = "false"

let is_literal str = is_number_literal str || is_bool_literal str

let value_of_bool_literal str = if str = "true" then true else false

let value_of_int_literal str = int_of_string str

let value_of_float_literal str = float_of_string str

let is_tight_packed = function
  | (info1, str1), info2 ->
      info1.line = info2.line
      && info1.column + String.length str1 = info2.column

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

let raise_parse_err_tks tks desc =
  match tks with
  | [] -> raise_parse_err_eof ()
  | (info, _) :: _ -> raise_parse_err info desc

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
      "==";
      "<=";
      ">=";
      "<";
      ">";
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

let create_tm_atom info v =
  let v =
    if is_bool_literal v then BoolLiteral (value_of_bool_literal v)
    else if is_float_literal v then FloatLiteral (value_of_float_literal v)
    else IntLiteral (value_of_int_literal v)
  in
  TmAtom (info, v)

let binary_ops =
  [ [ "<"; ">"; "<="; ">="; "==" ]; [ "+"; "-" ]; [ "*"; "/" ] ]

let get_op_pred op =
  let rec impl ops i =
    match ops with
    | [] -> 1
    | hd :: tl ->
        if List.exists (fun s -> s = op) hd then i else impl tl (i + 1)
  in
  impl binary_ops 2

let is_binary_op str =
  List.flatten binary_ops |> List.exists (fun s -> s = str)

let rec resolve_tm_stack tm_stk pred =
  match tm_stk with
  | (op, info, tm) :: tl when List.length tl >= 1 -> (
      let op_pred = get_op_pred op in
      if op_pred <= pred then tm_stk
      else
        let tm_stk = resolve_tm_stack tl pred in
        match tm_stk with
        | (prev_op, prev_info, prev_tm) :: tl ->
            let composite_tm =
              if is_binary_op op then
                TmOp (info, op, TmTuple (info, [ prev_tm; tm ]))
              else TmApp (info, prev_tm, tm)
            in
            (prev_op, prev_info, composite_tm) :: tl
        | _ ->
            raise (ParseError "fatal resolve term stack. should not happen"))
  | _ -> tm_stk

let rec resolve_tm_stack_all tm_stk =
  if List.length tm_stk <= 1 then tm_stk
  else
    let op, _, _ = List.hd tm_stk in
    let pred = get_op_pred op in
    resolve_tm_stack_all (resolve_tm_stack tm_stk (pred - 1))

let rec parse_tm_stack tm_stk tks =
  let is_stop_symbol str =
    keywords @ [ ")"; "]"; "}"; "," ] |> List.exists (fun s -> s = str)
  in
  let parse_op info op tl =
    let tm, tl = parse_single_term tl in
    let tm_stk = resolve_tm_stack tm_stk (get_op_pred op) in
    parse_tm_stack ((op, info, tm) :: tm_stk) tl
  in
  let finish () =
    let _, _, tm = List.hd (resolve_tm_stack_all tm_stk) in
    (tm, tks)
  in
  match tks with
  | (_, stop) :: _ when is_stop_symbol stop -> finish ()
  | (info, op) :: tl when is_binary_op op -> parse_op info op tl
  | [] -> finish ()
  | (info, _) :: _ -> parse_op info "" tks

and parse_term tks =
  let tm, tl = parse_single_term tks in
  parse_tm_stack [ ("", get_tm_info tm, tm) ] tl

and parse_tm_abs info name tks =
  let ty, tl = parse_plain_type tks in
  match tl with
  | (_, ".") :: tl ->
      let tm, tl = parse_term tl in
      (TmAbs (info, name, ty, tm), tl)
  | _ -> raise_parse_err info "function expect body"

and parse_tm_let info name tks =
  let t1, tl = parse_term tks in
  match tl with
  | (_, "in") :: tl ->
      let t2, tl = parse_term tl in
      (TmLet (info, name, t1, t2), tl)
  | _ -> raise_parse_err info "expect 'in' for let binding"

and parse_tm_if info tks =
  let t1, tl = parse_term tks in
  match tl with
  | (_, "then") :: tl -> (
      let t2, tl = parse_term tl in
      match tl with
      | (_, "else") :: tl ->
          let t3, tl = parse_term tl in
          (TmIf (info, t1, t2, t3), tl)
      | _ -> raise_parse_err info "expect 'else' for if clause")
  | _ -> raise_parse_err info "expect 'then' for if clause"

and parse_tm_loop info tks =
  let t1, tl = parse_term tks in
  match tl with
  | (_, "in") :: tl ->
      let t2, tl = parse_term tl in
      (TmLoop (info, t1, t2), tl)
  | _ -> raise_parse_err info "expect 'in' for loop clause"

and parse_tm_tuple info = function
  | (_, "]") :: tl -> ([], tl)
  | tl -> (
      let tm, tl = parse_term tl in
      match tl with
      | (info, ",") :: tl ->
          let tms, tl = parse_tm_tuple info tl in
          (tm :: tms, tl)
      | (_, "]") :: tl -> ([ tm ], tl)
      | _ -> raise_parse_err info "invalid tuple")

and parse_kv_pair = function
  | (_, label) :: (_, "=") :: tl ->
      let tm, tl = parse_term tl in
      ((label, tm), tl)
  | tl -> raise_parse_err_tks tl "expect key-value pair"

and parse_kv_list = function
  | (_, "}") :: tl -> ([], tl)
  | tl -> (
      let kv, tl = parse_kv_pair tl in
      match tl with
      | (_, ",") :: tl ->
          let kvs, tl = parse_kv_list tl in
          (kv :: kvs, tl)
      | (_, "}") :: tl -> ([ kv ], tl)
      | tl -> raise_parse_err_tks tl "invalid key-value list")

and parse_single_element_term = function
  | (info, v) :: tl when is_literal v -> (create_tm_atom info v, tl)
  | (info, "fn") :: (_, name) :: (_, ":") :: tl when is_ident name ->
      parse_tm_abs info name tl
  | (info, "let") :: (_, name) :: (_, "=") :: tl when is_ident name ->
      parse_tm_let info name tl
  | (info, "if") :: tl -> parse_tm_if info tl
  | (info, "loop") :: tl -> parse_tm_loop info tl
  | (info, "[") :: tl ->
      let tms, tl = parse_tm_tuple info tl in
      (TmTuple (info, tms), tl)
  | (info, "(") :: tl -> (
      let tm, tl = parse_term tl in
      match tl with
      | (_, ")") :: tl -> (tm, tl)
      | _ -> raise_parse_err info "unpaired (")
  | (info, ident) :: tl when is_ident ident -> (
      match tl with
      | (info2, "[") :: tl when is_tight_packed ((info, ident), info2) ->
          let tms, tl = parse_tm_tuple info2 tl in
          (TmNamedTuple (info, ident, tms), tl)
      | (info2, "{") :: tl when is_tight_packed ((info, ident), info2) ->
          let kvs, tl = parse_kv_list tl in
          (TmRecord (info, ident, kvs), tl)
      | _ -> (TmIdent (info, ident), tl))
  | tl -> raise_parse_err_tks tl "expect single term"

and decorate_accessor tm tl =
  match tl with
  | (_, ".") :: (info, label) :: tl when is_ident label ->
      decorate_accessor (TmRecordAccess (info, tm, label)) tl
  | (_, ".") :: (info, index) :: tl when is_int_literal index ->
      decorate_accessor
        (TmTupleAccess (info, tm, value_of_int_literal index))
        tl
  | _ -> (tm, tl)

and parse_single_decorated_element_term tks =
  let tm, tl = parse_single_element_term tks in
  decorate_accessor tm tl

and parse_single_term = function
  | (info, "-") :: tl ->
      let tm, tl = parse_single_decorated_element_term tl in
      (TmOp (info, "-", tm), tl)
  | tl -> parse_single_decorated_element_term tl

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
  | (info, "let") :: (_, name) :: (_, "=") :: tks when is_ident name ->
      let tm, tks = parse_term tks in
      TopTmLet (info, name, tm) :: parse_toplevel tks
  | (info, "entry") :: (_, name) :: (_, "=") :: tks when is_ident name ->
      let tm, tks = parse_term tks in
      TopTmEntry (info, name, tm) :: parse_toplevel tks
  | (info, _) :: _ -> raise_parse_err info "invalid top level declare"
