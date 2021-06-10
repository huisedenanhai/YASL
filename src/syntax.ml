type atomic_value =
  | IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool

let desc_string_of_atomic_value = function
  | IntLiteral v -> Printf.sprintf "IntLiteral(%d)" v
  | FloatLiteral v -> Printf.sprintf "FloatLiteral(%f)" v
  | BoolLiteral v ->
      Printf.sprintf "BoolLiteral(%s)" (if v then "true" else "false")

type plain_ty = TyNamed of string | TyTuple of plain_ty list

type ty_declare =
  | TyDeclTuple of string * plain_ty list
  | TyDeclRecord of string * (string * plain_ty) list
  | TyDeclOpaque of string

let name_of_ty_decl = function
  | TyDeclTuple (name, _) -> name
  | TyDeclRecord (name, _) -> name
  | TyDeclOpaque name -> name

let rec desc_string_of_plain_ty = function
  | TyNamed name -> name
  | TyTuple ts ->
      let ts_desc =
        ts
        |> List.map (fun pt -> desc_string_of_plain_ty pt)
        |> String.concat ", "
      in
      Printf.sprintf "[%s]" ts_desc

let rec is_same_plain_ty pt1 pt2 =
  match (pt1, pt2) with
  | TyNamed n1, TyNamed n2 -> n1 = n2
  | TyTuple ts1, TyTuple ts2 ->
      List.for_all2 (fun t1 t2 -> is_same_plain_ty t1 t2) ts1 ts2
  | _ -> false

type ty = TyPlain of plain_ty | TyArrow of int * plain_ty * ty

let is_plain_ty = function
  | TyPlain _ -> true
  | _ -> false

let is_arrow_ty = function
  | TyArrow _ -> true
  | _ -> false

let rec desc_string_of_type = function
  | TyPlain pt -> desc_string_of_plain_ty pt
  | TyArrow (id, pt, dt) ->
      Printf.sprintf "%s <%d>-> %s"
        (desc_string_of_plain_ty pt)
        id (desc_string_of_type dt)

let rec is_same_ty ty1 ty2 =
  match (ty1, ty2) with
  | TyPlain pt1, TyPlain pt2 -> is_same_plain_ty pt1 pt2
  | TyArrow (id1, st1, dt1), TyArrow (id2, st2, dt2) ->
      id1 = id2 && is_same_plain_ty st1 st2 && is_same_ty dt1 dt2
  | _ -> false

type tm_info = { line : int; column : int; ty : ty option }

let create_tm_info l c = { line = l; column = c; ty = None }

let init_tm_info = create_tm_info 1 1

let info_inc_column_n info n = { info with column = info.column + n }

let info_inc_column info = info_inc_column_n info 1

let info_inc_line info = { info with line = info.line + 1; column = 1 }

let info_set_type info ty = { info with ty = Some ty }

let id_of_tm_info info = (info.line * 100000) + info.column

let string_of_tm_info info =
  let ty_desc =
    match info.ty with
    | None -> "Unknown"
    | Some t -> desc_string_of_type t
  in
  Printf.sprintf "line: %d, column: %d, ty: %s" info.line info.column ty_desc

let desc_string_of_ty_declare = function
  | TyDeclTuple (name, ts) ->
      let ts_desc =
        ts
        |> List.map (fun pt -> desc_string_of_plain_ty pt)
        |> String.concat ", "
      in
      Printf.sprintf "TyDeclTuple(%s, [%s])" name ts_desc
  | TyDeclRecord (name, kts) ->
      let kts_desc =
        kts
        |> List.map (fun (k, pt) ->
               Printf.sprintf "%s: %s" k (desc_string_of_plain_ty pt))
        |> String.concat ", "
      in
      Printf.sprintf "TyDeclRecord(%s, {%s})" name kts_desc
  | TyDeclOpaque name -> Printf.sprintf "TyDeclOpaque(%s)" name

type term =
  | TmAtom of tm_info * atomic_value
  | TmAbs of tm_info * string * plain_ty * term
  | TmApp of tm_info * term * term
  | TmLet of tm_info * string * term * term
  | TmIf of tm_info * term * term * term
  | TmLoop of tm_info * term * term
  | TmTuple of tm_info * term list
  | TmNamedTuple of tm_info * string * term list
  | TmTupleAccess of tm_info * term * int
  | TmRecord of tm_info * string * (string * term) list
  | TmRecordAccess of tm_info * term * string
  | TmOp of tm_info * string * term
  | TmIdent of tm_info * string

let get_tm_info = function
  | TmAtom (info, _) -> info
  | TmAbs (info, _, _, _) -> info
  | TmApp (info, _, _) -> info
  | TmLet (info, _, _, _) -> info
  | TmIf (info, _, _, _) -> info
  | TmLoop (info, _, _) -> info
  | TmTuple (info, _) -> info
  | TmNamedTuple (info, _, _) -> info
  | TmTupleAccess (info, _, _) -> info
  | TmRecord (info, _, _) -> info
  | TmRecordAccess (info, _, _) -> info
  | TmOp (info, _, _) -> info
  | TmIdent (info, _) -> info

let rec desc_string_of_term_indent indent tm =
  let next_indent = indent ^ "  " in
  match tm with
  | TmAtom (info, av) ->
      Printf.sprintf "%sTmAtom<%s>(%s)" indent (string_of_tm_info info)
        (desc_string_of_atomic_value av)
  | TmAbs (info, name, ty, tm) ->
      Printf.sprintf "%sTmAbs<%s>(%s: %s,\n%s)" indent
        (string_of_tm_info info) name
        (desc_string_of_plain_ty ty)
        (desc_string_of_term_indent next_indent tm)
  | TmApp (info, t1, t2) ->
      Printf.sprintf "%sTmApp<%s>(\n%s,\n%s)" indent (string_of_tm_info info)
        (desc_string_of_term_indent next_indent t1)
        (desc_string_of_term_indent next_indent t2)
  | TmLet (info, name, t1, t2) ->
      Printf.sprintf "%sTmLet<%s>(%s =\n%s\n%sin\n%s)" indent
        (string_of_tm_info info) name
        (desc_string_of_term_indent next_indent t1)
        indent
        (desc_string_of_term_indent next_indent t2)
  | TmIf (info, t1, t2, t3) ->
      Printf.sprintf "%sTmIf<%s>(\n%s\n%sthen\n%s\n%selse\n%s)" indent
        (string_of_tm_info info)
        (desc_string_of_term_indent next_indent t1)
        indent
        (desc_string_of_term_indent next_indent t2)
        indent
        (desc_string_of_term_indent next_indent t3)
  | TmLoop (info, t1, t2) ->
      Printf.sprintf "%sTmLoop<%s>(\n%s,\n%s)" indent
        (string_of_tm_info info)
        (desc_string_of_term_indent next_indent t1)
        (desc_string_of_term_indent next_indent t2)
  | TmTuple (info, tms) ->
      let tms_desc =
        tms
        |> List.map (fun t -> desc_string_of_term_indent next_indent t)
        |> String.concat ",\n"
      in
      Printf.sprintf "%sTmTuple<%s>[\n%s\n%s]" indent
        (string_of_tm_info info) tms_desc indent
  | TmNamedTuple (info, name, tms) ->
      let tms_desc =
        tms
        |> List.map (fun t -> desc_string_of_term_indent next_indent t)
        |> String.concat ",\n"
      in
      Printf.sprintf "%sTmNamedTuple<%s>(%s)[\n%s\n%s]" indent
        (string_of_tm_info info) name tms_desc indent
  | TmRecord (info, name, kts) ->
      let kts_desc =
        kts
        |> List.map (fun (k, tm) ->
               Printf.sprintf "%s%s: %s" next_indent k
                 (desc_string_of_term_indent (next_indent ^ "  ") tm))
        |> String.concat ",\n"
      in
      Printf.sprintf "%sTmRecord<%s>(%s){\n%s\n%s}" indent
        (string_of_tm_info info) indent name kts_desc
  | TmTupleAccess (info, tm, index) ->
      Printf.sprintf "%sTmTupleAccess<%s>(\n%s\n%s.%d)" indent
        (string_of_tm_info info)
        (desc_string_of_term_indent next_indent tm)
        indent index
  | TmRecordAccess (info, tm, label) ->
      Printf.sprintf "%sTmRecordAccess<%s>(\n%s\n%s.%s)" indent
        (string_of_tm_info info)
        (desc_string_of_term_indent next_indent tm)
        indent label
  | TmOp (info, op, tm) ->
      Printf.sprintf "%sTmOp<%s>(%s\n%s)" indent (string_of_tm_info info) op
        (desc_string_of_term_indent next_indent tm)
  | TmIdent (info, ident) ->
      Printf.sprintf "%sTmIdent<%s>(%s)" indent (string_of_tm_info info)
        ident

let desc_string_of_term tm = desc_string_of_term_indent "" tm

type toplevel_term =
  | TopTmUnfiorm of tm_info * string * plain_ty
  | TopTmExtern of tm_info * string * ty
  | TopTmTyDeclare of tm_info * ty_declare
  | TopTmLet of tm_info * string * term
  | TopTmEntry of tm_info * string * term

let desc_string_of_toplevel_term = function
  | TopTmUnfiorm (info, name, ty) ->
      Printf.sprintf "Uniform<%s>(%s, %s)" (string_of_tm_info info) name
        (desc_string_of_plain_ty ty)
  | TopTmExtern (info, name, ty) ->
      Printf.sprintf "Extern<%s>(%s, %s)" (string_of_tm_info info) name
        (desc_string_of_type ty)
  | TopTmTyDeclare (info, decl) ->
      Printf.sprintf "TyDeclare<%s>(%s)" (string_of_tm_info info)
        (desc_string_of_ty_declare decl)
  | TopTmLet (info, name, tm) ->
      Printf.sprintf "Let<%s>(%s =\n%s)" (string_of_tm_info info) name
        (desc_string_of_term_indent "  " tm)
  | TopTmEntry (info, name, tm) ->
      Printf.sprintf "Entry<%s>(%s =\n%s)" (string_of_tm_info info) name
        (desc_string_of_term_indent "  " tm)
