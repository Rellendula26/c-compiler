open Ast
open Tacky

let temp_counter = ref 0

let make_temp () =
  let name = "tmp." ^ string_of_int !temp_counter in
  incr temp_counter;
  name

let convert_unop = function
  | Ast.Complement -> Tacky.Complement
  | Ast.Negate -> Tacky.Negate

let rec emit_tacky_exp exp =
  match exp with
  | Ast.Constant n ->
      ([], Tacky.Constant n)

  | Ast.Unary (op, inner) ->
      let inner_instructions, src = emit_tacky_exp inner in
      let dst = Tacky.Var (make_temp ()) in
      let instruction = Tacky.Unary (convert_unop op, src, dst) in
      (inner_instructions @ [instruction], dst)

let gen_statement = function
  | Ast.Return expr ->
      let instructions, value = emit_tacky_exp expr in
      instructions @ [Tacky.Return value]

let gen_function = function
  | Ast.Function (name, body) ->
      Tacky.Function (name, gen_statement body)

let gen_program = function
  | Ast.Program func ->
      Tacky.Program (gen_function func)