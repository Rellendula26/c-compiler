(* Represents the entire parsed source program. *)
type program =
  | Program of function_definition

(* Represents a function definition containing its name and body. *)
and function_definition =
  | Function of string * statement

(* Represents a statement inside a function body. *)
and statement =
  | Return of exp

(* Represents an expression. *)
and exp =
  | Constant of int