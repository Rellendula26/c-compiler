type program =
  | Program of function_definition

and function_definition =
  | Function of string * instruction list

and instruction =
  | Mov of operand * operand
  | Ret

and operand =
  | Imm of int
  | Reg of reg

and reg =
  | W0