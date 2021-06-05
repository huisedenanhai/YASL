type info = string

type atomic_value =
  | IntLiteral of int
  | FloatLiteral of float
  | BoolLiteral of bool

type ty =
  | TyCustom of string
  | TyTuple of ty list
  | TyArrow of ty * ty
  | TyUnit
  | TyFloat
  | TyInt

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
  | TmCast of info * term * ty
  | TmMinus of info * term
  | TmBinaryOp of info * term * binary_op * term

let () = print_endline "Hello Ocaml!"
