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

let break_label loop_label =
  "break_" ^ loop_label

let continue_label loop_label =
  "continue_" ^ loop_label

let require_label = function
  | Some label -> label
  | None -> failwith "Loop statement should have been labeled"

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

  | Ast.Var name ->
      ([], Tacky.Var name)

  | Ast.FunctionCall (name, args) ->
      let arg_instructions, arg_values =
        List.fold_left
          (fun (all_instrs, values) arg ->
            let instrs, value = emit_tacky_exp arg in
            (all_instrs @ instrs, values @ [value]))
          ([], [])
          args
      in
      let dst = Tacky.Var (make_temp ()) in
      (arg_instructions @ [Tacky.FunCall (name, arg_values, dst)], dst)

  | Ast.Assignment (Ast.Var name, rhs) ->
      let rhs_instructions, rhs_val = emit_tacky_exp rhs in
      let dst = Tacky.Var name in
      (rhs_instructions @ [Tacky.Copy (rhs_val, dst)], dst)

  | Ast.Assignment _ ->
      failwith "Invalid assignment target should have been rejected by resolver"

  | Ast.Conditional (condition, then_exp, else_exp) ->
      let condition_instructions, condition_val = emit_tacky_exp condition in
      let then_instructions, then_val = emit_tacky_exp then_exp in
      let else_instructions, else_val = emit_tacky_exp else_exp in
      let dst = Tacky.Var (make_temp ()) in
      let else_label = make_label "conditional_else" in
      let end_label = make_label "conditional_end" in
      (
        condition_instructions
        @ [Tacky.JumpIfZero (condition_val, else_label)]
        @ then_instructions
        @ [
            Tacky.Copy (then_val, dst);
            Tacky.Jump end_label;
            Tacky.Label else_label;
          ]
        @ else_instructions
        @ [
            Tacky.Copy (else_val, dst);
            Tacky.Label end_label;
          ],
        dst
      )

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

let emit_optional_exp = function
  | None -> []

  | Some exp ->
      let instructions, _ = emit_tacky_exp exp in
      instructions

let rec emit_tacky_block block =
  match block with
  | Ast.Block items ->
      items
      |> List.map emit_tacky_block_item
      |> List.flatten

and emit_tacky_statement stmt =
  match stmt with
  | Ast.Return expr ->
      let instructions, result = emit_tacky_exp expr in
      instructions @ [Tacky.Return result]

  | Ast.Expression expr ->
      let instructions, _ = emit_tacky_exp expr in
      instructions

  | Ast.If (condition, then_stmt, None) ->
      let condition_instructions, condition_val = emit_tacky_exp condition in
      let end_label = make_label "if_end" in
      condition_instructions
      @ [Tacky.JumpIfZero (condition_val, end_label)]
      @ emit_tacky_statement then_stmt
      @ [Tacky.Label end_label]

  | Ast.If (condition, then_stmt, Some else_stmt) ->
      let condition_instructions, condition_val = emit_tacky_exp condition in
      let else_label = make_label "if_else" in
      let end_label = make_label "if_end" in
      condition_instructions
      @ [Tacky.JumpIfZero (condition_val, else_label)]
      @ emit_tacky_statement then_stmt
      @ [Tacky.Jump end_label; Tacky.Label else_label]
      @ emit_tacky_statement else_stmt
      @ [Tacky.Label end_label]

  | Ast.Compound block ->
      emit_tacky_block block

  | Ast.Break label ->
      let label = require_label label in
      [Tacky.Jump (break_label label)]

  | Ast.Continue label ->
      let label = require_label label in
      [Tacky.Jump (continue_label label)]

  | Ast.While (condition, body, label) ->
      let label = require_label label in
      let condition_instructions, condition_val = emit_tacky_exp condition in
      let body_instructions = emit_tacky_statement body in
      let continue_label = continue_label label in
      let break_label = break_label label in
      [Tacky.Label continue_label]
      @ condition_instructions
      @ [Tacky.JumpIfZero (condition_val, break_label)]
      @ body_instructions
      @ [Tacky.Jump continue_label; Tacky.Label break_label]

  | Ast.DoWhile (body, condition, label) ->
      let label = require_label label in
      let start_label = "start_" ^ label in
      let continue_label = continue_label label in
      let break_label = break_label label in
      let body_instructions = emit_tacky_statement body in
      let condition_instructions, condition_val = emit_tacky_exp condition in
      [Tacky.Label start_label]
      @ body_instructions
      @ [Tacky.Label continue_label]
      @ condition_instructions
      @ [Tacky.JumpIfNotZero (condition_val, start_label)]
      @ [Tacky.Label break_label]

  | Ast.For (init, condition, post, body, label) ->
      let label = require_label label in
      let start_label = "start_" ^ label in
      let continue_label = continue_label label in
      let break_label = break_label label in
      let init_instructions = emit_tacky_for_init init in
      let condition_instructions, condition_val_opt =
        match condition with
        | None -> ([], None)
        | Some exp ->
            let instructions, value = emit_tacky_exp exp in
            (instructions, Some value)
      in
      let post_instructions = emit_optional_exp post in
      let body_instructions = emit_tacky_statement body in
      init_instructions
      @ [Tacky.Label start_label]
      @ condition_instructions
      @
      (match condition_val_opt with
       | None -> []
       | Some condition_val ->
           [Tacky.JumpIfZero (condition_val, break_label)])
      @ body_instructions
      @ [Tacky.Label continue_label]
      @ post_instructions
      @ [Tacky.Jump start_label; Tacky.Label break_label]

  | Ast.Null ->
      []

and emit_tacky_variable_declaration decl =
  match decl with
  | Ast.VariableDeclaration (_name, None) ->
      []

  | Ast.VariableDeclaration (name, Some init) ->
      let instructions, value = emit_tacky_exp init in
      instructions @ [Tacky.Copy (value, Tacky.Var name)]

and emit_tacky_for_init init =
  match init with
  | Ast.InitDecl decl ->
      emit_tacky_variable_declaration decl

  | Ast.InitExp None ->
      []

  | Ast.InitExp (Some exp) ->
      let instructions, _ = emit_tacky_exp exp in
      instructions

and emit_tacky_block_item item =
  match item with
  | Ast.S stmt ->
      emit_tacky_statement stmt

  | Ast.D (Ast.VarDecl decl) ->
      emit_tacky_variable_declaration decl

  | Ast.D (Ast.FunDecl _) ->
      []

let emit_tacky_function = function
  | Ast.FunctionDeclaration (name, params, Some body) ->
      let instructions = emit_tacky_block body in
      Tacky.Function (
        name,
        params,
        instructions @ [Tacky.Return (Tacky.Constant 0)]
      )

  | Ast.FunctionDeclaration (_, _, None) ->
      failwith "Function declaration should not emit code"

let gen_program program =
  match program with
  | Ast.Program funcs ->
      let functions =
        funcs
        |> List.filter (function
             | Ast.FunctionDeclaration (_, _, Some _) -> true
             | _ -> false)
        |> List.map emit_tacky_function
      in
      Tacky.Program functions