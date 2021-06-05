type info = string

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

type ty = TyPlain of plain_ty | TyArrow of ty * ty | TyUnit

type binary_op = BOpAdd | BOpMinus | BOpDiv | BOpMul

type term =
  | TmAtom of info * atomic_value
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmLet of info * string * ty option * term * term
  | TmIf of info * term * term * term
  | TmLoop of info * term * term
  | TmTuple of info * term list
  | TmNamedTuple of info * string * term list
  | TmTupleAccess of info * term * int
  | TmRecord of info * string * (string * term) list
  | TmRecordAccess of info * term * string
  | TmUnit of info
  | TmMinus of info * term
  | TmBinaryOp of info * term * binary_op * term

let () = print_endline "Hello Ocaml!"
