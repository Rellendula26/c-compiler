open Tacky
open Asm

module StringMap = Map.Make(String)

let convert_val = function
  | Tacky.Constant n -> Asm.Imm n
  | Tacky.Var name -> Asm.Pseudo name

let convert_unop = function
  | Tacky.Complement -> Asm.Not
  | Tacky.Negate -> Asm.Neg
  | Tacky.Not -> failwith "Logical not handled specially"

let convert_cond_code = function
  | Tacky.Equal -> Asm.E
  | Tacky.NotEqual -> Asm.NE
  | Tacky.LessThan -> Asm.L
  | Tacky.LessOrEqual -> Asm.LE
  | Tacky.GreaterThan -> Asm.G
  | Tacky.GreaterOrEqual -> Asm.GE
  | _ -> failwith "Expected comparison operator"

let arg_registers =
  [ Asm.DI; Asm.SI; Asm.DX; Asm.CX; Asm.R8; Asm.R9 ]

let rec take n xs =
  if n <= 0 then []
  else match xs with [] -> [] | x :: rest -> x :: take (n - 1) rest

let rec drop n xs =
  if n <= 0 then xs
  else match xs with [] -> [] | _ :: rest -> drop (n - 1) rest

let gen_instruction = function
  | Tacky.Return value ->
      [Asm.Mov (convert_val value, Asm.Reg Asm.AX); Asm.Ret]

  | Tacky.Copy (src, dst) ->
      [Asm.Mov (convert_val src, convert_val dst)]

  | Tacky.Unary (Tacky.Not, src, dst) ->
      [
        Asm.Cmp (Asm.Imm 0, convert_val src);
        Asm.Mov (Asm.Imm 0, convert_val dst);
        Asm.SetCC (Asm.E, convert_val dst);
      ]

  | Tacky.Unary (op, src, dst) ->
      [
        Asm.Mov (convert_val src, convert_val dst);
        Asm.Unary (convert_unop op, convert_val dst);
      ]

  | Tacky.Binary (op, src1, src2, dst) ->
      (match op with
       | Tacky.Add ->
           [Asm.Mov (convert_val src1, convert_val dst);
            Asm.Binary (Asm.Add, convert_val src2, convert_val dst)]

       | Tacky.Subtract ->
           [Asm.Mov (convert_val src1, convert_val dst);
            Asm.Binary (Asm.Sub, convert_val src2, convert_val dst)]

       | Tacky.Multiply ->
           [Asm.Mov (convert_val src1, convert_val dst);
            Asm.Binary (Asm.Mult, convert_val src2, convert_val dst)]

       | Tacky.Divide ->
           [Asm.Mov (convert_val src1, Asm.Reg Asm.AX);
            Asm.Cdq;
            Asm.Idiv (convert_val src2);
            Asm.Mov (Asm.Reg Asm.AX, convert_val dst)]

       | Tacky.Remainder ->
           [Asm.Mov (convert_val src1, Asm.Reg Asm.AX);
            Asm.Cdq;
            Asm.Idiv (convert_val src2);
            Asm.Mov (Asm.Reg Asm.DX, convert_val dst)]

       | Tacky.Equal | Tacky.NotEqual
       | Tacky.LessThan | Tacky.LessOrEqual
       | Tacky.GreaterThan | Tacky.GreaterOrEqual ->
           [Asm.Cmp (convert_val src2, convert_val src1);
            Asm.Mov (Asm.Imm 0, convert_val dst);
            Asm.SetCC (convert_cond_code op, convert_val dst)])

  | Tacky.Jump target -> [Asm.Jmp target]

  | Tacky.JumpIfZero (condition, target) ->
      [Asm.Cmp (Asm.Imm 0, convert_val condition); Asm.JmpCC (Asm.E, target)]

  | Tacky.JumpIfNotZero (condition, target) ->
      [Asm.Cmp (Asm.Imm 0, convert_val condition); Asm.JmpCC (Asm.NE, target)]

  | Tacky.Label label -> [Asm.Label label]

  | Tacky.FunCall (fun_name, args, dst) ->
      let register_args = take 6 args in
      let stack_args = drop 6 args in

      let register_moves =
        List.combine register_args (take (List.length register_args) arg_registers)
        |> List.map (fun (arg, reg) ->
             Asm.Mov (convert_val arg, Asm.Reg reg))
      in

      let stack_pushes =
        stack_args
        |> List.rev
        |> List.map (fun arg -> Asm.Push (convert_val arg))
      in

      let needs_padding = List.length stack_args mod 2 <> 0 in
      let stack_padding =
        if needs_padding then [Asm.AllocateStack 8] else []
      in

      let bytes_to_remove =
        (List.length stack_args * 8) + (if needs_padding then 8 else 0)
      in
      let cleanup =
        if bytes_to_remove = 0 then [] else [Asm.DeallocateStack bytes_to_remove]
      in

      stack_padding
      @ stack_pushes
      @ register_moves
      @ [Asm.Call fun_name]
      @ cleanup
      @ [Asm.Mov (Asm.Reg Asm.AX, convert_val dst)]

let replace_pseudos instructions =
  let stack_offset = ref 0 in
  let table = ref StringMap.empty in

  let replace_operand = function
    | Asm.Pseudo name ->
        if StringMap.mem name !table then
          Asm.Stack (StringMap.find name !table)
        else (
          stack_offset := !stack_offset - 4;
          table := StringMap.add name !stack_offset !table;
          Asm.Stack !stack_offset
        )
    | other -> other
  in

  let replace_instruction = function
    | Asm.Mov (src, dst) -> Asm.Mov (replace_operand src, replace_operand dst)
    | Asm.Unary (op, operand) -> Asm.Unary (op, replace_operand operand)
    | Asm.Binary (op, src, dst) -> Asm.Binary (op, replace_operand src, replace_operand dst)
    | Asm.Cmp (src, dst) -> Asm.Cmp (replace_operand src, replace_operand dst)
    | Asm.Idiv operand -> Asm.Idiv (replace_operand operand)
    | Asm.SetCC (cc, operand) -> Asm.SetCC (cc, replace_operand operand)
    | Asm.Push operand -> Asm.Push (replace_operand operand)
    | other -> other
  in

  let instructions = List.map replace_instruction instructions in
  (instructions, -(!stack_offset))

let fix_instruction = function
  | Asm.Mov (Asm.Stack src, Asm.Stack dst) ->
      [Asm.Mov (Asm.Stack src, Asm.Reg Asm.R10);
       Asm.Mov (Asm.Reg Asm.R10, Asm.Stack dst)]

  | Asm.Binary (op, src, Asm.Stack dst) when op = Asm.Mult ->
      [Asm.Mov (Asm.Stack dst, Asm.Reg Asm.R11);
       Asm.Binary (op, src, Asm.Reg Asm.R11);
       Asm.Mov (Asm.Reg Asm.R11, Asm.Stack dst)]

  | Asm.Binary (op, Asm.Stack src, Asm.Stack dst) ->
      [Asm.Mov (Asm.Stack src, Asm.Reg Asm.R10);
       Asm.Binary (op, Asm.Reg Asm.R10, Asm.Stack dst)]

  | Asm.Cmp (Asm.Stack src, Asm.Stack dst) ->
      [Asm.Mov (Asm.Stack src, Asm.Reg Asm.R10);
       Asm.Cmp (Asm.Reg Asm.R10, Asm.Stack dst)]

  | Asm.Cmp (src, Asm.Imm n) ->
      [Asm.Mov (Asm.Imm n, Asm.Reg Asm.R11);
       Asm.Cmp (src, Asm.Reg Asm.R11)]

  | Asm.Idiv (Asm.Imm n) ->
      [Asm.Mov (Asm.Imm n, Asm.Reg Asm.R10);
       Asm.Idiv (Asm.Reg Asm.R10)]

  | other -> [other]

let fix_instructions instructions =
  List.flatten (List.map fix_instruction instructions)

let gen_function = function
  | Tacky.Function (name, params, instructions) ->
      let param_moves =
        params
        |> List.mapi (fun i param ->
             if i < 6 then
               Asm.Mov (Asm.Reg (List.nth arg_registers i), Asm.Pseudo param)
             else
               let stack_offset = 16 + ((i - 6) * 8) in
               Asm.Mov (Asm.Stack stack_offset, Asm.Pseudo param))
      in

      let instructions =
        param_moves @ List.flatten (List.map gen_instruction instructions)
      in

      let instructions, stack_bytes = replace_pseudos instructions in
      let stack_bytes =
        if stack_bytes mod 16 = 0 then stack_bytes
        else stack_bytes + (16 - (stack_bytes mod 16))
      in

      let instructions =
        Asm.AllocateStack stack_bytes :: fix_instructions instructions
      in

      Asm.Function (name, instructions)

let gen_program = function
  | Tacky.Program functions ->
      Asm.Program (List.map gen_function functions)