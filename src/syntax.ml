type tm_info = { line : int; column : int }

let create_tm_info l c = { line = l; column = c }

let init_tm_info = create_tm_info 1 1

let info_inc_column_n info n = { line = info.line; column = info.column + n }

let info_inc_column info = info_inc_column_n info 1

let info_inc_line info = { line = info.line + 1; column = 1 }

let string_of_tm_info info =
  Printf.sprintf "line: %d, column %d" info.line info.column

type atomic_value =
  | IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool

type plain_ty =
  | TyCustom of string
  | TyTuple of plain_ty list
  | TyFloat
  | TyInt
  | TyBool

let rec desc_string_of_plain_ty = function
  | TyCustom name -> Printf.sprintf "TyCustom(%s)" name
  | TyTuple ts ->
      let ts_desc =
        ts
        |> List.map (fun pt -> desc_string_of_plain_ty pt)
        |> String.concat ", "
      in
      Printf.sprintf "TyTuple[%s]" ts_desc
  | TyFloat -> "TyFloat"
  | TyInt -> "TyInt"
  | TyBool -> "TyBool"

type ty = TyPlain of plain_ty | TyArrow of int * plain_ty * ty

let rec desc_string_of_type = function
  | TyPlain pt -> desc_string_of_plain_ty pt
  | TyArrow (id, pt, dt) ->
      Printf.sprintf "%s <%d>-> %s"
        (desc_string_of_plain_ty pt)
        id (desc_string_of_type dt)

type binary_op = BOpAdd | BOpMinus | BOpDiv | BOpMul

type term =
  | TmAtom of tm_info * atomic_value
  | TmAbs of tm_info * string * ty * term
  | TmApp of tm_info * term * term
  | TmLet of tm_info * string * term * term
  | TmIf of tm_info * term * term * term
  | TmLoop of tm_info * term * term
  | TmTuple of tm_info * term list
  | TmNamedTuple of tm_info * string * term list
  | TmTupleAccess of tm_info * term * int
  | TmRecord of tm_info * string * (string * term) list
  | TmRecordAccess of tm_info * term * string
  | TmMinus of tm_info * term
  | TmBinaryOp of tm_info * term * binary_op * term

let desc_string_of_term _ = "Term"

type ty_declare =
  | TyDeclTuple of string * plain_ty list
  | TyDeclRecord of string * (string * plain_ty) list

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
      Printf.sprintf "Let<%s>(%s, %s)" (string_of_tm_info info) name
        (desc_string_of_term tm)
  | TopTmEntry (info, name, tm) ->
      Printf.sprintf "Entry<%s>(%s, %s)" (string_of_tm_info info) name
        (desc_string_of_term tm)
