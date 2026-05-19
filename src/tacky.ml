type program =
  | Program of function_definition

and function_definition =
  | Function of string * instruction list

and instruction =
  | Return of value
  | Unary of unary_operator * value * value
  | Binary of binary_operator * value * value * value

and value =
  | Constant of int
  | Var of string

and unary_operator =
  | Complement
  | Negate

and binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder