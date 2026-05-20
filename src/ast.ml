type program =
  | Program of function_definition

and function_definition =
  | Function of string * statement

and statement =
  | Return of exp

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
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp