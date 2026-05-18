type program =
  | Program of function_definition

and function_definition =
  | Function of string * instruction list

and instruction =
  | Mov of operand * operand
  | Unary of unary_operator * operand
  | AllocateStack of int
  | Ret

and unary_operator =
  | Neg
  | Not

and operand =
  | Imm of int
  | Reg of reg
  | Pseudo of string
  | Stack of int

and reg =
  | AX
  | R10