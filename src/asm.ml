type program =
  | Program of function_definition list

and function_definition =
  | Function of string * instruction list

and instruction =
  | Mov of operand * operand
  | Unary of unary_operator * operand
  | Binary of binary_operator * operand * operand
  | Cmp of operand * operand
  | Idiv of operand
  | Cdq
  | Jmp of string
  | JmpCC of cond_code * string
  | SetCC of cond_code * operand
  | Label of string
  | Push of operand
  | Call of string
  | AllocateStack of int
  | DeallocateStack of int
  | Ret

and unary_operator =
  | Neg
  | Not

and binary_operator =
  | Add
  | Sub
  | Mult

and cond_code =
  | E
  | NE
  | G
  | GE
  | L
  | LE

and operand =
  | Imm of int
  | Reg of reg
  | Pseudo of string
  | Stack of int

and reg =
  | AX
  | CX
  | DX
  | DI
  | SI
  | R8
  | R9
  | R10
  | R11