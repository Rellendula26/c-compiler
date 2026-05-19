open Printf

type token =
  | IntKw
  | VoidKw
  | ReturnKw
  | Ident of string
  | IntConst of int

  | Minus
  | Tilde
  | Plus
  | Star
  | Slash
  | Percent

  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semicolon

exception LexError of string

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_word_char c =
  is_letter c || is_digit c

let keyword_of_ident = function
  | "int" -> Some IntKw
  | "void" -> Some VoidKw
  | "return" -> Some ReturnKw
  | _ -> None

let lex input =
  let len = String.length input in

  let rec lex_at i acc =
    if i >= len then
      List.rev acc
    else
      match input.[i] with
      | c when is_whitespace c ->
          lex_at (i + 1) acc

      | '(' ->
          lex_at (i + 1) (LParen :: acc)

      | ')' ->
          lex_at (i + 1) (RParen :: acc)

      | '{' ->
          lex_at (i + 1) (LBrace :: acc)

      | '}' ->
          lex_at (i + 1) (RBrace :: acc)

      | ';' ->
          lex_at (i + 1) (Semicolon :: acc)

      | '~' ->
          lex_at (i + 1) (Tilde :: acc)

      | '-' ->
          lex_at (i + 1) (Minus :: acc)

      | '+' ->
          lex_at (i + 1) (Plus :: acc)

      | '*' ->
          lex_at (i + 1) (Star :: acc)

      | '/' ->
          lex_at (i + 1) (Slash :: acc)

      | '%' ->
          lex_at (i + 1) (Percent :: acc)

      | c when is_letter c ->
          let j = ref (i + 1) in
          while !j < len && is_word_char input.[!j] do
            incr j
          done;
          let name = String.sub input i (!j - i) in
          let token =
            match keyword_of_ident name with
            | Some kw -> kw
            | None -> Ident name
          in
          lex_at !j (token :: acc)

      | c when is_digit c ->
          let j = ref (i + 1) in
          while !j < len && is_digit input.[!j] do
            incr j
          done;
          let num_str = String.sub input i (!j - i) in
          let num = int_of_string num_str in
          lex_at !j (IntConst num :: acc)

      | c ->
          raise (LexError (sprintf "Unexpected character: %c" c))
  in
  lex_at 0 []