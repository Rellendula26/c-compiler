open Tacky
open Asm

module StringMap = Map.Make(String)

let convert_val = function
  | Tacky.Constant n -> Asm.Imm n
  | Tacky.Var name -> Asm.Pseudo name

let convert_unop = function
  | Tacky.Complement -> Asm.Not
  | Tacky.Negate -> Asm.Neg
  | Tacky.Not -> failwith "Tacky.Not handled specially"

let cond_code_of_relop = function
  | Tacky.Equal -> Asm.E
  | Tacky.NotEqual -> Asm.NE
  | Tacky.LessThan -> Asm.L
  | Tacky.LessOrEqual -> Asm.LE
  | Tacky.GreaterThan -> Asm.G
  | Tacky.GreaterOrEqual -> Asm.GE
  | _ -> failwith "Not a relational operator"

let gen_instruction = function
  | Tacky.Return value ->
      [Asm.Mov (convert_val value, Asm.Reg Asm.AX); Asm.Ret]

  | Tacky.Copy (src, dst) ->
      [Asm.Mov (convert_val src, convert_val dst)]

  | Tacky.Jump target ->
      [Asm.Jmp target]

  | Tacky.JumpIfZero (condition, target) ->
      [
        Asm.Cmp (Asm.Imm 0, convert_val condition);
        Asm.JmpCC (Asm.E, target);
      ]

  | Tacky.JumpIfNotZero (condition, target) ->
      [
        Asm.Cmp (Asm.Imm 0, convert_val condition);
        Asm.JmpCC (Asm.NE, target);
      ]

  | Tacky.Label name ->
      [Asm.Label name]

  | Tacky.Unary (Tacky.Not, src, dst) ->
      let src = convert_val src in
      let dst = convert_val dst in
      [
        Asm.Cmp (Asm.Imm 0, src);
        Asm.Mov (Asm.Imm 0, dst);
        Asm.SetCC (Asm.E, dst);
      ]

  | Tacky.Unary (op, src, dst) ->
      let src = convert_val src in
      let dst = convert_val dst in
      [Asm.Mov (src, dst); Asm.Unary (convert_unop op, dst)]

  | Tacky.Binary
      ((Tacky.Equal | Tacky.NotEqual | Tacky.LessThan | Tacky.LessOrEqual
        | Tacky.GreaterThan | Tacky.GreaterOrEqual) as op, src1, src2, dst) ->
      let src1 = convert_val src1 in
      let src2 = convert_val src2 in
      let dst = convert_val dst in
      [
        Asm.Cmp (src2, src1);
        Asm.Mov (Asm.Imm 0, dst);
        Asm.SetCC (cond_code_of_relop op, dst);
      ]

  | Tacky.Binary (op, src1, src2, dst) ->
      let src1 = convert_val src1 in
      let src2 = convert_val src2 in
      let dst = convert_val dst in
      match op with
      | Tacky.Add ->
          [
            Asm.Mov (src1, dst);
            Asm.Binary (Asm.Add, src2, dst);
          ]

      | Tacky.Subtract ->
          [
            Asm.Mov (src1, dst);
            Asm.Binary (Asm.Sub, src2, dst);
          ]

      | Tacky.Multiply ->
          [
            Asm.Mov (src1, dst);
            Asm.Binary (Asm.Mult, src2, dst);
          ]

      | Tacky.Divide ->
          [
            Asm.Mov (src1, Asm.Reg Asm.AX);
            Asm.Cdq;
            Asm.Idiv src2;
            Asm.Mov (Asm.Reg Asm.AX, dst);
          ]

      | Tacky.Remainder ->
          [
            Asm.Mov (src1, Asm.Reg Asm.AX);
            Asm.Cdq;
            Asm.Idiv src2;
            Asm.Mov (Asm.Reg Asm.DX, dst);
          ]

      | _ ->
          failwith "Unexpected binary operator in normal binary codegen"

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

  | Binary (op, src, dst) ->
      let src, state = replace_operand src state in
      let dst, state = replace_operand dst state in
      (Binary (op, src, dst), state)

  | Cmp (src, dst) ->
      let src, state = replace_operand src state in
      let dst, state = replace_operand dst state in
      (Cmp (src, dst), state)

  | Idiv operand ->
      let operand, state = replace_operand operand state in
      (Idiv operand, state)

  | SetCC (cond, operand) ->
      let operand, state = replace_operand operand state in
      (SetCC (cond, operand), state)

  | Cdq
  | Jmp _
  | JmpCC _
  | Label _
  | Ret
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

  | Binary (op, Stack src, Stack dst) ->
      [
        Mov (Stack src, Reg R10);
        Binary (op, Reg R10, Stack dst);
      ]

  | Idiv (Imm n) ->
      [
        Mov (Imm n, Reg R10);
        Idiv (Reg R10);
      ]

  | Cmp (Stack src, Stack dst) ->
      [
        Mov (Stack src, Reg R10);
        Cmp (Reg R10, Stack dst);
      ]

  | Cmp (src, Imm n) ->
      [
        Mov (Imm n, Reg R11);
        Cmp (src, Reg R11);
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