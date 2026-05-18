open Ast
open Asm

let gen_exp = function
  | Constant n -> Imm n

let gen_statement = function
  | Return expr ->
      let operand = gen_exp expr in
      [Mov (operand, Reg W0); Ret]

let gen_function = function
  | Ast.Function (name, body) ->
      let instructions = gen_statement body in
      Asm.Function (name, instructions)

let gen_program = function
  | Ast.Program func ->
      Asm.Program (gen_function func)