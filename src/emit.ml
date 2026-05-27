open Asm

let emit_reg_4byte = function
  | AX -> "%eax"
  | CX -> "%ecx"
  | DX -> "%edx"
  | DI -> "%edi"
  | SI -> "%esi"
  | R8 -> "%r8d"
  | R9 -> "%r9d"
  | R10 -> "%r10d"
  | R11 -> "%r11d"

let emit_reg_1byte = function
  | AX -> "%al"
  | CX -> "%cl"
  | DX -> "%dl"
  | DI -> "%dil"
  | SI -> "%sil"
  | R8 -> "%r8b"
  | R9 -> "%r9b"
  | R10 -> "%r10b"
  | R11 -> "%r11b"

let emit_operand = function
  | Imm n -> "$" ^ string_of_int n
  | Reg r -> emit_reg_4byte r
  | Pseudo name -> name
  | Stack offset -> string_of_int offset ^ "(%rbp)"

let emit_operand_1byte = function
  | Reg r -> emit_reg_1byte r
  | Stack offset -> string_of_int offset ^ "(%rbp)"
  | other -> emit_operand other

let emit_unop = function
  | Neg -> "negl"
  | Not -> "notl"

let emit_binop = function
  | Add -> "addl"
  | Sub -> "subl"
  | Mult -> "imull"

let emit_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"

let emit_label name =
  "L" ^ name

let emit_instruction = function
  | Mov (src, dst) ->
      "    movl " ^ emit_operand src ^ ", " ^ emit_operand dst

  | Unary (op, operand) ->
      "    " ^ emit_unop op ^ " " ^ emit_operand operand

  | Binary (op, src, dst) ->
      "    " ^ emit_binop op ^ " " ^
      emit_operand src ^ ", " ^ emit_operand dst

  | Cmp (src, dst) ->
      "    cmpl " ^ emit_operand src ^ ", " ^ emit_operand dst

  | Idiv operand ->
      "    idivl " ^ emit_operand operand

  | Cdq ->
      "    cdq"

  | Jmp target ->
      "    jmp " ^ emit_label target

  | JmpCC (cond, target) ->
      "    j" ^ emit_cond_code cond ^ " " ^ emit_label target

  | SetCC (cond, operand) ->
      "    set" ^ emit_cond_code cond ^ " " ^ emit_operand_1byte operand

  | Label name ->
      emit_label name ^ ":"

  | Push operand ->
      "    pushq " ^ emit_operand operand

  | Call name ->
      "    call _" ^ name

  | AllocateStack bytes ->
      "    subq $" ^ string_of_int bytes ^ ", %rsp"

  | DeallocateStack bytes ->
      "    addq $" ^ string_of_int bytes ^ ", %rsp"

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
  | Program funcs ->
      funcs
      |> List.map emit_function
      |> String.concat "\n"