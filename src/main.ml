type tm_info = string

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

type ty = TyPlain of plain_ty | TyArrow of int * plain_ty * ty

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

type ty_declare =
  | TyDeclTuple of string * plain_ty list
  | TyDeclRecord of string * (string * plain_ty) list

type top_level_term =
  | TopTmUnfiorm of tm_info * string * plain_ty
  | TopTmExtern of tm_info * string * ty
  | TopTmTyDeclare of tm_info * ty_declare
  | TopTmLet of tm_info * string * term
  | TopTmEntry of tm_info * string * term

let () = print_endline "Hello Ocaml!"
