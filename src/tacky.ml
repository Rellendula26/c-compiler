type program =
  | Program of function_definition

and function_definition =
  | Function of string * instruction list

and instruction =
  | Return of value
  | Unary of unary_operator * value * value
  | Binary of binary_operator * value * value * value
  | Copy of value * value
  | Jump of string
  | JumpIfZero of value * string
  | JumpIfNotZero of value * string
  | Label of string

and value =
  | Constant of int
  | Var of string

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
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual