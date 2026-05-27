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

let rec parse_argument_list tokens acc =
  match tokens with
  | RParen :: rest ->
      (List.rev acc, rest)
  | _ ->
      let arg, rest = parse_exp tokens 0 in
      (match rest with
       | Comma :: rest -> parse_argument_list rest (arg :: acc)
       | RParen :: rest -> (List.rev (arg :: acc), rest)
       | _ -> raise (ParseError "Malformed argument list"))

and parse_factor tokens =
  match tokens with
  | IntConst n :: rest -> (Constant n, rest)

  | Ident name :: LParen :: rest ->
      let args, rest = parse_argument_list rest [] in
      (FunctionCall (name, args), rest)

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

  | _ -> raise (ParseError "Malformed factor")

and parse_exp tokens min_prec =
  let left, tokens = parse_factor tokens in
  parse_exp_loop left tokens min_prec

and parse_exp_loop left tokens min_prec =
  match tokens with
  | Assign :: rest when precedence Assign >= min_prec ->
      let right, rest = parse_exp rest (precedence Assign) in
      parse_exp_loop (Assignment (left, right)) rest min_prec

  | Question :: rest when precedence Question >= min_prec ->
      let middle, rest = parse_exp rest 0 in
      let rest = expect Colon rest in
      let right, rest = parse_exp rest (precedence Question) in
      parse_exp_loop (Conditional (left, middle, right)) rest min_prec

  | tok :: rest when is_expression_op tok && precedence tok >= min_prec ->
      let op = binop_of_token tok in
      let right, rest = parse_exp rest (precedence tok + 1) in
      parse_exp_loop (Binary (op, left, right)) rest min_prec

  | _ -> (left, tokens)

let parse_optional_exp end_token tokens =
  match tokens with
  | tok :: rest when tok = end_token -> (None, rest)
  | _ ->
      let expr, tokens = parse_exp tokens 0 in
      let tokens = expect end_token tokens in
      (Some expr, tokens)

let rec parse_param_list tokens acc =
  match tokens with
  | VoidKw :: RParen :: rest ->
      ([], rest)

  | IntKw :: Ident name :: rest ->
      let acc = name :: acc in
      (match rest with
       | Comma :: rest -> parse_param_list rest acc
       | RParen :: rest -> (List.rev acc, rest)
       | _ -> raise (ParseError "Malformed parameter list"))

  | _ ->
      raise (ParseError "Malformed parameter list")

let parse_variable_declaration_after_name name tokens =
  match tokens with
  | Assign :: rest ->
      let init, rest = parse_exp rest 0 in
      let rest = expect Semicolon rest in
      (VariableDeclaration (name, Some init), rest)

  | Semicolon :: rest ->
      (VariableDeclaration (name, None), rest)

  | _ ->
      raise (ParseError "Malformed variable declaration")

let rec parse_declaration tokens =
  let tokens = expect IntKw tokens in
  match tokens with
  | Ident name :: LParen :: rest ->
      let params, rest = parse_param_list rest [] in
      (match rest with
       | Semicolon :: rest ->
           (FunDecl (FunctionDeclaration (name, params, None)), rest)
       | LBrace :: _ ->
           let body, rest = parse_block rest in
           (FunDecl (FunctionDeclaration (name, params, Some body)), rest)
       | _ ->
           raise (ParseError "Malformed function declaration"))

  | Ident name :: rest ->
      let var_decl, rest = parse_variable_declaration_after_name name rest in
      (VarDecl var_decl, rest)

  | _ ->
      raise (ParseError "Malformed declaration")

and parse_for_init tokens =
  match tokens with
  | IntKw :: Ident name :: LParen :: _ ->
      raise (ParseError "Function declaration not allowed in for initializer")

  | IntKw :: _ ->
      let tokens = expect IntKw tokens in
      (match tokens with
       | Ident name :: rest ->
           let var_decl, rest = parse_variable_declaration_after_name name rest in
           (InitDecl var_decl, rest)
       | _ ->
           raise (ParseError "Malformed for initializer"))

  | Semicolon :: rest ->
      (InitExp None, rest)

  | _ ->
      let expr, rest = parse_exp tokens 0 in
      let rest = expect Semicolon rest in
      (InitExp (Some expr), rest)

and parse_statement tokens =
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

  | WhileKw :: rest ->
      let rest = expect LParen rest in
      let condition, rest = parse_exp rest 0 in
      let rest = expect RParen rest in
      let body, rest = parse_statement rest in
      (While (condition, body, None), rest)

  | DoKw :: rest ->
      let body, rest = parse_statement rest in
      let rest = expect WhileKw rest in
      let rest = expect LParen rest in
      let condition, rest = parse_exp rest 0 in
      let rest = expect RParen rest in
      let rest = expect Semicolon rest in
      (DoWhile (body, condition, None), rest)

  | ForKw :: rest ->
      let rest = expect LParen rest in
      let init, rest = parse_for_init rest in
      let condition, rest = parse_optional_exp Semicolon rest in
      let post, rest = parse_optional_exp RParen rest in
      let body, rest = parse_statement rest in
      (For (init, condition, post, body, None), rest)

  | BreakKw :: rest ->
      let rest = expect Semicolon rest in
      (Break None, rest)

  | ContinueKw :: rest ->
      let rest = expect Semicolon rest in
      (Continue None, rest)

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
  | RBrace :: rest -> (List.rev acc, rest)
  | [] -> raise (ParseError "Unexpected end of input in block")
  | _ ->
      let item, rest = parse_block_item tokens in
      parse_block_items rest (item :: acc)

let parse_function_declaration tokens =
  match parse_declaration tokens with
  | FunDecl func_decl, rest -> (func_decl, rest)
  | VarDecl _, _ -> raise (ParseError "Top-level variable declarations not supported yet")

let rec parse_function_declarations tokens acc =
  match tokens with
  | [] -> Program (List.rev acc)
  | _ ->
      let func_decl, rest = parse_function_declaration tokens in
      parse_function_declarations rest (func_decl :: acc)

let parse tokens =
  parse_function_declarations tokens []