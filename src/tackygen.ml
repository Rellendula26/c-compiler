open Ast
open Tacky

let temp_counter = ref 0

let make_temp () =
  let name = "tmp" ^ string_of_int !temp_counter in
  incr temp_counter;
  name

let convert_unop = function
  | Ast.Complement -> Tacky.Complement
  | Ast.Negate -> Tacky.Negate

let convert_binop = function
  | Ast.Add -> Tacky.Add
  | Ast.Subtract -> Tacky.Subtract
  | Ast.Multiply -> Tacky.Multiply
  | Ast.Divide -> Tacky.Divide
  | Ast.Remainder -> Tacky.Remainder

let rec emit_tacky_exp exp =
  match exp with
  | Ast.Constant n ->
      ([], Tacky.Constant n)

  | Ast.Unary (op, inner) ->
      let inner_instructions, src = emit_tacky_exp inner in
      let dst = Tacky.Var (make_temp ()) in
      let instruction = Tacky.Unary (convert_unop op, src, dst) in
      (inner_instructions @ [instruction], dst)

  | Ast.Binary (op, left, right) ->
      let left_instructions, src1 = emit_tacky_exp left in
      let right_instructions, src2 = emit_tacky_exp right in
      let dst = Tacky.Var (make_temp ()) in
      let instruction =
        Tacky.Binary (convert_binop op, src1, src2, dst)
      in
      (left_instructions @ right_instructions @ [instruction], dst)

let gen_program program =
  match program with
  | Ast.Program (Ast.Function (name, Ast.Return expr)) ->
      let instructions, result = emit_tacky_exp expr in
      Tacky.Program (
        Tacky.Function (name, instructions @ [Tacky.Return result])
      )