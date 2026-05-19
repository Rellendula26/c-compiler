type program =
  | Program of function_definition

and function_definition =
  | Function of string * instruction list

and instruction =
  | Mov of operand * operand
  | Unary of unary_operator * operand
  | Binary of binary_operator * operand * operand
  | Cdq
  | Idiv of operand
  | AllocateStack of int
  | Ret

and unary_operator =
  | Neg
  | Not

and binary_operator =
  | Add
  | Sub
  | Mult

and operand =
  | Imm of int
  | Reg of reg
  | Pseudo of string
  | Stack of int

and reg =
  | AX
  | DX
  | R10