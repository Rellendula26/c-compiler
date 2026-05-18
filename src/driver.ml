let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let contents = really_input_string ic len in
  close_in ic;
  contents

let write_file filename contents =
  let oc = open_out filename in
  output_string oc contents;
  close_out oc

let compile filename =
  let source = read_file filename in
  let tokens = Lexer.lex source in
  let ast = Parser.parse tokens in
  let asm = Codegen.gen_program ast in
  let output = Emit.emit_program asm in
  write_file "out.s" output

(*Compiles actualy .c file*)
let () =
  if Array.length Sys.argv <> 2 then
    failwith "Usage: ./mycc <source.c>"
  else
    compile Sys.argv.(1)