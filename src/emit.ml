open Asm

let emit_reg = function
  | AX -> "%eax"
  | DX -> "%edx"
  | R10 -> "%r10d"

let emit_operand = function
  | Imm n -> "$" ^ string_of_int n
  | Reg r -> emit_reg r
  | Pseudo name -> name
  | Stack offset -> string_of_int offset ^ "(%rbp)"

let emit_unop = function
  | Neg -> "negl"
  | Not -> "notl"

let emit_binop = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"

let emit_instruction = function
  | Mov (src, dst) ->
      "    movl " ^ emit_operand src ^ ", " ^ emit_operand dst

  | Unary (op, operand) ->
      "    " ^ emit_unop op ^ " " ^ emit_operand operand

  | Binary (op, src, dst) ->
      "    " ^ emit_binop op ^ " " ^
      emit_operand src ^ ", " ^ emit_operand dst

  | Cdq ->
      "    cdq"

  | Idiv operand ->
      "    idivl " ^ emit_operand operand

  | AllocateStack bytes ->
      "    subq $" ^ string_of_int bytes ^ ", %rsp"

  | Ret ->
      "    movq %rbp, %rsp\n" ^
      "    popq %rbp\n" ^
      "    ret"

let emit_function = function
  | Function (name, instructions) ->
      let label = "_" ^ name in
      let body =
        instructions
        |> List.map emit_instruction
        |> String.concat "\n"
      in
      ".globl " ^ label ^ "\n" ^
      label ^ ":\n" ^
      "    pushq %rbp\n" ^
      "    movq %rsp, %rbp\n" ^
      body ^ "\n"

let emit_program = function
  | Program func ->
      emit_function func