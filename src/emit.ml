open Asm

let emit_reg = function
  | W0 -> "w0"

let emit_operand = function
  | Imm n -> "#" ^ string_of_int n
  | Reg r -> emit_reg r

let emit_instruction = function
  | Mov (src, dst) ->
      "    mov " ^ emit_operand dst ^ ", " ^ emit_operand src
  | Ret ->
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
      body ^ "\n"

let emit_program = function
  | Program func ->
      emit_function func