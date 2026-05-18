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

(* Parses a return statement according to: return <exp>; *)
let rec parse_exp tokens =
  match tokens with
  | IntConst n :: rest ->
      (Constant n, rest)

  | Hyphen :: rest ->
      let inner, rest = parse_exp rest in
      (Unary (Negate, inner), rest)

  | Tilde :: rest ->
      let inner, rest = parse_exp rest in
      (Unary (Complement, inner), rest)

  | LParen :: rest ->
      let inner, rest = parse_exp rest in
      let rest = expect RParen rest in
      (inner, rest)

  | Decrement :: _ ->
      raise (ParseError "Decrement operator is not supported")

  | _ ->
      raise (ParseError "Malformed expression")


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