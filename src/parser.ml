open Lexer
open Ast

exception ParseError of string

let expect expected tokens =
  match tokens with
  | actual :: rest when actual = expected -> rest
  | actual :: _ ->
      raise (ParseError "Unexpected token")
  | [] ->
      raise (ParseError "Unexpected end of input")

let parse_exp tokens =
  match tokens with
  | IntConst n :: rest ->
      (Constant n, rest)
  | _ ->
      raise (ParseError "Expected integer constant")

(*Parses tokens one by one*)
let parse_statement tokens =
  let tokens = expect ReturnKw tokens in
  let expr, tokens = parse_exp tokens in
  let tokens = expect Semicolon tokens in
  (Return expr, tokens)

(*Grammar rule for function definition*)
let parse_function tokens =
  let tokens = expect IntKw tokens in
  match tokens with
  | Ident name :: rest ->
      let tokens = expect LParen rest in
      let tokens = expect VoidKw tokens in
      let tokens = expect RParen tokens in
      let tokens = expect LBrace tokens in
      let stmt, tokens = parse_statement tokens in
      let tokens = expect RBrace tokens in
      (Function (name, stmt), tokens)
  | _ ->
      raise (ParseError "Expected function name")


let parse tokens =
  let func, tokens = parse_function tokens in
  match tokens with
  | [] -> Program func
  | _ -> raise (ParseError "Unexpected tokens after end of program")