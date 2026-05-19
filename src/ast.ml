(* Represents the entire parsed source program. *)
type program =
  | Program of function_definition

(* Represents a function definition containing its name and body. *)
and function_definition =
  | Function of string * statement

(* Represents a statement inside a function body. *)
and statement =
  | Return of exp

(* Unary operators *)
and unary_operator =
  | Complement
  | Negate

(* NEW: binary operators *)
and binary_operator =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder

(* Expressions *)
and exp =
  | Constant of int
  | Unary of unary_operator * exp
  | Binary of binary_operator * exp * exp