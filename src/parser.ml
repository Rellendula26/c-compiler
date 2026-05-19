open Lexer
open Ast

exception ParseError of string

let expect expected tokens =
  match tokens with
  | actual :: rest when actual = expected -> rest
  | _ -> raise (ParseError "Unexpected token")

let precedence = function
  | Plus | Minus -> 45
  | Star | Slash | Percent -> 50
  | _ -> raise (ParseError "Not a binary operator")

let is_binary_op = function
  | Plus | Minus | Star | Slash | Percent -> true
  | _ -> false

let binop_of_token = function
  | Plus -> Add
  | Minus -> Subtract
  | Star -> Multiply
  | Slash -> Divide
  | Percent -> Remainder
  | _ -> raise (ParseError "Expected binary operator")

(* Parse constants, unary expressions, and parenthesized expressions *)
let rec parse_factor tokens =
  match tokens with
  | IntConst n :: rest ->
      (Constant n, rest)

  | Minus :: rest ->
      let inner, rest = parse_factor rest in
      (Unary (Negate, inner), rest)

  | Tilde :: rest ->
      let inner, rest = parse_factor rest in
      (Unary (Complement, inner), rest)

  | LParen :: rest ->
      let inner, rest = parse_exp rest 0 in
      let rest = expect RParen rest in
      (inner, rest)

  | _ ->
      raise (ParseError "Malformed factor")

(* Precedence climbing parser *)
and parse_exp tokens min_prec =
  let left, tokens = parse_factor tokens in
  parse_exp_loop left tokens min_prec

and parse_exp_loop left tokens min_prec =
  match tokens with
  | tok :: _ when is_binary_op tok && precedence tok >= min_prec ->
      let op = binop_of_token tok in
      let rest = List.tl tokens in
      let right, rest =
        parse_exp rest (precedence tok + 1)
      in
      let new_left = Binary (op, left, right) in
      parse_exp_loop new_left rest min_prec

  | _ ->
      (left, tokens)

let parse_statement tokens =
  let tokens = expect ReturnKw tokens in
  let expr, tokens = parse_exp tokens 0 in
  let tokens = expect Semicolon tokens in
  (Return expr, tokens)

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
  | _ -> raise (ParseError "Unexpected tokens after program")