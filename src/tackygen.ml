open Ast
open Tacky

let temp_counter = ref 0
let label_counter = ref 0

let make_temp () =
  let name = "tmp" ^ string_of_int !temp_counter in
  incr temp_counter;
  name

let make_label prefix =
  let name = prefix ^ string_of_int !label_counter in
  incr label_counter;
  name

let convert_unop = function
  | Ast.Complement -> Tacky.Complement
  | Ast.Negate -> Tacky.Negate
  | Ast.Not -> Tacky.Not

let convert_binop = function
  | Ast.Add -> Tacky.Add
  | Ast.Subtract -> Tacky.Subtract
  | Ast.Multiply -> Tacky.Multiply
  | Ast.Divide -> Tacky.Divide
  | Ast.Remainder -> Tacky.Remainder
  | Ast.Equal -> Tacky.Equal
  | Ast.NotEqual -> Tacky.NotEqual
  | Ast.LessThan -> Tacky.LessThan
  | Ast.LessOrEqual -> Tacky.LessOrEqual
  | Ast.GreaterThan -> Tacky.GreaterThan
  | Ast.GreaterOrEqual -> Tacky.GreaterOrEqual
  | Ast.And | Ast.Or ->
      failwith "Logical operators handled separately"

let rec emit_tacky_exp exp =
  match exp with
  | Ast.Constant n ->
      ([], Tacky.Constant n)

  | Ast.Unary (op, inner) ->
      let inner_instructions, src = emit_tacky_exp inner in
      let dst = Tacky.Var (make_temp ()) in
      let instruction = Tacky.Unary (convert_unop op, src, dst) in
      (inner_instructions @ [instruction], dst)

  | Ast.Binary (Ast.And, left, right) ->
      let left_instructions, left_val = emit_tacky_exp left in
      let right_instructions, right_val = emit_tacky_exp right in
      let dst = Tacky.Var (make_temp ()) in
      let false_label = make_label "and_false" in
      let end_label = make_label "and_end" in
      (
        left_instructions
        @ [Tacky.JumpIfZero (left_val, false_label)]
        @ right_instructions
        @ [
            Tacky.JumpIfZero (right_val, false_label);
            Tacky.Copy (Tacky.Constant 1, dst);
            Tacky.Jump end_label;
            Tacky.Label false_label;
            Tacky.Copy (Tacky.Constant 0, dst);
            Tacky.Label end_label;
          ],
        dst
      )

  | Ast.Binary (Ast.Or, left, right) ->
      let left_instructions, left_val = emit_tacky_exp left in
      let right_instructions, right_val = emit_tacky_exp right in
      let dst = Tacky.Var (make_temp ()) in
      let true_label = make_label "or_true" in
      let end_label = make_label "or_end" in
      (
        left_instructions
        @ [Tacky.JumpIfNotZero (left_val, true_label)]
        @ right_instructions
        @ [
            Tacky.JumpIfNotZero (right_val, true_label);
            Tacky.Copy (Tacky.Constant 0, dst);
            Tacky.Jump end_label;
            Tacky.Label true_label;
            Tacky.Copy (Tacky.Constant 1, dst);
            Tacky.Label end_label;
          ],
        dst
      )

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