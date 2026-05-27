(* Source Program Meaning *)

type program =
  | Program of function_declaration list

and function_declaration =
  | FunctionDeclaration of string * string list * block option

and block =
  | Block of block_item list

and block_item =
  | S of statement
  | D of declaration

and declaration =
  | VarDecl of variable_declaration
  | FunDecl of function_declaration

and variable_declaration =
  | VariableDeclaration of string * exp option

and for_init =
  | InitDecl of variable_declaration
  | InitExp of exp option

and statement =
  | Return of exp
  | Expression of exp
  | If of exp * statement * statement option
  | Compound of block
  | Break of string option
  | Continue of string option
  | While of exp * statement * string option
  | DoWhile of statement * exp * string option
  | For of for_init * exp option * exp option * statement * string option
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
  | FunctionCall of string * exp list