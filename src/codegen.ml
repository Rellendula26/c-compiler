open Tacky
open Asm

module StringMap = Map.Make(String)

let convert_val = function
  | Tacky.Constant n -> Asm.Imm n
  | Tacky.Var name -> Asm.Pseudo name

let convert_unop = function
  | Tacky.Complement -> Asm.Not
  | Tacky.Negate -> Asm.Neg

let gen_instruction = function
  | Tacky.Return value ->
      [Asm.Mov (convert_val value, Asm.Reg Asm.AX); Asm.Ret]

  | Tacky.Unary (op, src, dst) ->
      let src = convert_val src in
      let dst = convert_val dst in
      [Asm.Mov (src, dst); Asm.Unary (convert_unop op, dst)]

let replace_operand operand state =
  let stack_map, next_offset = state in
  match operand with
  | Pseudo name ->
      if StringMap.mem name stack_map then
        (Stack (StringMap.find name stack_map), state)
      else
        let new_offset = next_offset - 4 in
        let stack_map = StringMap.add name new_offset stack_map in
        (Stack new_offset, (stack_map, new_offset))

  | _ ->
      (operand, state)

let replace_instruction instruction state =
  match instruction with
  | Mov (src, dst) ->
      let src, state = replace_operand src state in
      let dst, state = replace_operand dst state in
      (Mov (src, dst), state)

  | Unary (op, operand) ->
      let operand, state = replace_operand operand state in
      (Unary (op, operand), state)

  | Ret ->
      (Ret, state)

  | AllocateStack _ ->
      (instruction, state)

let replace_pseudos instructions =
  let initial_state = (StringMap.empty, 0) in
  let instructions, (_, final_offset) =
    List.fold_left
      (fun (acc, state) instr ->
        let instr, state = replace_instruction instr state in
        (acc @ [instr], state))
      ([], initial_state)
      instructions
  in
  (instructions, -final_offset)

let fix_instruction = function
  | Mov (Stack src, Stack dst) ->
      [
        Mov (Stack src, Reg R10);
        Mov (Reg R10, Stack dst);
      ]

  | instr ->
      [instr]

let fix_instructions instructions =
  instructions
  |> List.map fix_instruction
  |> List.flatten

let gen_function = function
  | Tacky.Function (name, instructions) ->
      let asm_instructions =
        instructions
        |> List.map gen_instruction
        |> List.flatten
      in
      let asm_instructions, stack_bytes = replace_pseudos asm_instructions in
      let asm_instructions = fix_instructions asm_instructions in
      Asm.Function (name, AllocateStack stack_bytes :: asm_instructions)

let gen_program = function
  | Tacky.Program func ->
      Asm.Program (gen_function func)