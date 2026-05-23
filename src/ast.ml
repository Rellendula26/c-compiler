(* Source Program Meaning *)

type program =
  | Program of function_definition

and function_definition =
  | Function of string * block_item list

and block_item =
  | S of statement
  | D of declaration

and declaration =
  | Declaration of string * exp option

and statement =
  | Return of exp
  | Expression of exp
  | If of exp * statement * statement option
  | Null

and unary_operator =
  | Complement
  | Negate
  | Not

and binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual

and exp =
  | Constant of int
  | Var of string
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp
  | Assignment of exp * exp
  | Conditional of exp * exp * exp