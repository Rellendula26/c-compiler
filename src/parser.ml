open Lexer
open Ast

exception ParseError of string

let expect expected tokens =
  match tokens with
  | actual :: rest when actual = expected -> rest
  | _ -> raise (ParseError "Unexpected token")

let precedence = function
  | Star | Slash | Percent -> 50
  | Plus | Minus -> 45
  | Less | LessEqual | Greater | GreaterEqual -> 35
  | EqualEqual | BangEqual -> 30
  | And -> 10
  | Or -> 5
  | Question -> 3
  | Assign -> 1
  | _ -> raise (ParseError "Not an expression operator")

let is_expression_op = function
  | Plus | Minus | Star | Slash | Percent
  | And | Or
  | EqualEqual | BangEqual
  | Less | LessEqual | Greater | GreaterEqual
  | Question
  | Assign -> true
  | _ -> false

let binop_of_token = function
  | Plus -> Add
  | Minus -> Subtract
  | Star -> Multiply
  | Slash -> Divide
  | Percent -> Remainder
  | And -> And
  | Or -> Or
  | EqualEqual -> Equal
  | BangEqual -> NotEqual
  | Less -> LessThan
  | LessEqual -> LessOrEqual
  | Greater -> GreaterThan
  | GreaterEqual -> GreaterOrEqual
  | _ -> raise (ParseError "Expected binary operator")

let rec parse_factor tokens =
  match tokens with
  | IntConst n :: rest ->
      (Constant n, rest)

  | Ident name :: rest ->
      (Var name, rest)

  | Minus :: rest ->
      let inner, rest = parse_factor rest in
      (Unary (Negate, inner), rest)

  | Tilde :: rest ->
      let inner, rest = parse_factor rest in
      (Unary (Complement, inner), rest)

  | Bang :: rest ->
      let inner, rest = parse_factor rest in
      (Unary (Not, inner), rest)

  | LParen :: rest ->
      let inner, rest = parse_exp rest 0 in
      let rest = expect RParen rest in
      (inner, rest)

  | _ ->
      raise (ParseError "Malformed factor")

and parse_exp tokens min_prec =
  let left, tokens = parse_factor tokens in
  parse_exp_loop left tokens min_prec

and parse_exp_loop left tokens min_prec =
  match tokens with
  | Assign :: rest when precedence Assign >= min_prec ->
      let right, rest = parse_exp rest (precedence Assign) in
      let new_left = Assignment (left, right) in
      parse_exp_loop new_left rest min_prec

  | Question :: rest when precedence Question >= min_prec ->
      let middle, rest = parse_exp rest 0 in
      let rest = expect Colon rest in
      let right, rest = parse_exp rest (precedence Question) in
      let new_left = Conditional (left, middle, right) in
      parse_exp_loop new_left rest min_prec

  | tok :: rest when is_expression_op tok && precedence tok >= min_prec ->
      let op = binop_of_token tok in
      let right, rest = parse_exp rest (precedence tok + 1) in
      let new_left = Binary (op, left, right) in
      parse_exp_loop new_left rest min_prec

  | _ ->
      (left, tokens)

let parse_declaration tokens =
  let tokens = expect IntKw tokens in
  match tokens with
  | Ident name :: Assign :: rest ->
      let init, rest = parse_exp rest 0 in
      let rest = expect Semicolon rest in
      (Declaration (name, Some init), rest)

  | Ident name :: Semicolon :: rest ->
      (Declaration (name, None), rest)

  | _ ->
      raise (ParseError "Malformed declaration")

let rec parse_statement tokens =
  match tokens with
  | ReturnKw :: rest ->
      let expr, rest = parse_exp rest 0 in
      let rest = expect Semicolon rest in
      (Return expr, rest)

  | IfKw :: rest ->
      let rest = expect LParen rest in
      let condition, rest = parse_exp rest 0 in
      let rest = expect RParen rest in
      let then_stmt, rest = parse_statement rest in
      (match rest with
       | ElseKw :: rest ->
           let else_stmt, rest = parse_statement rest in
           (If (condition, then_stmt, Some else_stmt), rest)
       | _ ->
           (If (condition, then_stmt, None), rest))

  | LBrace :: _ ->
      let block, rest = parse_block tokens in
      (Compound block, rest)

  | Semicolon :: rest ->
      (Null, rest)

  | _ ->
      let expr, rest = parse_exp tokens 0 in
      let rest = expect Semicolon rest in
      (Expression expr, rest)

and parse_block tokens =
  let tokens = expect LBrace tokens in
  let items, tokens = parse_block_items tokens [] in
  (Block items, tokens)

and parse_block_item tokens =
  match tokens with
  | IntKw :: _ ->
      let decl, rest = parse_declaration tokens in
      (D decl, rest)

  | _ ->
      let stmt, rest = parse_statement tokens in
      (S stmt, rest)

and parse_block_items tokens acc =
  match tokens with
  | RBrace :: rest ->
      (List.rev acc, rest)

  | [] ->
      raise (ParseError "Unexpected end of input in block")

  | _ ->
      let item, rest = parse_block_item tokens in
      parse_block_items rest (item :: acc)

let parse_function tokens =
  let tokens = expect IntKw tokens in
  match tokens with
  | Ident name :: rest ->
      let tokens = expect LParen rest in
      let tokens = expect VoidKw tokens in
      let tokens = expect RParen tokens in
      let body, tokens = parse_block tokens in
      (Function (name, body), tokens)

  | _ ->
      raise (ParseError "Expected function name")

let parse tokens =
  let func, tokens = parse_function tokens in
  match tokens with
  | [] -> Program func
  | _ -> raise (ParseError "Unexpected tokens after program")