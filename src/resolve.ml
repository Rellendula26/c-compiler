open Ast

exception ResolveError of string

module StringMap = Map.Make(String)

let counter = ref 0

let make_unique name =
  let unique = name ^ "." ^ string_of_int !counter in
  incr counter;
  unique

let rec resolve_exp env exp =
  match exp with
  | Constant _ ->
      exp

  | Var name ->
      if StringMap.mem name env then
        Var (StringMap.find name env)
      else
        raise (ResolveError ("Undeclared variable: " ^ name))

  | Unary (op, inner) ->
      Unary (op, resolve_exp env inner)

  | Binary (op, left, right) ->
      Binary (op, resolve_exp env left, resolve_exp env right)

  | Assignment (left, right) ->
      (match left with
       | Var _ ->
           Assignment (resolve_exp env left, resolve_exp env right)
       | _ ->
           raise (ResolveError "Invalid assignment target"))

let resolve_statement env stmt =
  match stmt with
  | Return exp ->
      Return (resolve_exp env exp)

  | Expression exp ->
      Expression (resolve_exp env exp)

  | Null ->
      Null

let resolve_declaration env decl =
  match decl with
  | Declaration (name, init) ->
      if StringMap.mem name env then
        raise (ResolveError ("Duplicate variable declaration: " ^ name))
      else
        let unique_name = make_unique name in
        let env = StringMap.add name unique_name env in
        let init =
          match init with
          | None -> None
          | Some exp -> Some (resolve_exp env exp)
        in
        (env, Declaration (unique_name, init))

let resolve_block_item env item =
  match item with
  | D decl ->
      let env, decl = resolve_declaration env decl in
      (env, D decl)

  | S stmt ->
      let stmt = resolve_statement env stmt in
      (env, S stmt)

let resolve_function = function
  | Function (name, body) ->
      let _, resolved_body =
        List.fold_left
          (fun (env, acc) item ->
            let env, item = resolve_block_item env item in
            (env, acc @ [item]))
          (StringMap.empty, [])
          body
      in
      Function (name, resolved_body)

let resolve_program = function
  | Program func ->
      Program (resolve_function func)