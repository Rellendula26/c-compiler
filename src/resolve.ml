open Ast

exception ResolveError of string

module StringMap = Map.Make(String)

type map_entry = {
  unique_name : string;
  from_current_block : bool;
}

let counter = ref 0

let make_unique name =
  let unique = name ^ "." ^ string_of_int !counter in
  incr counter;
  unique

let copy_env_for_inner_block env =
  StringMap.map
    (fun entry -> { entry with from_current_block = false })
    env

let lookup_var env name =
  if StringMap.mem name env then
    Var ((StringMap.find name env).unique_name)
  else
    raise (ResolveError ("Undeclared variable: " ^ name))

let rec resolve_exp env exp =
  match exp with
  | Constant _ ->
      exp

  | Var name ->
      lookup_var env name

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

  | Conditional (condition, then_exp, else_exp) ->
      Conditional (
        resolve_exp env condition,
        resolve_exp env then_exp,
        resolve_exp env else_exp
      )

let rec resolve_statement env stmt =
  match stmt with
  | Return exp ->
      Return (resolve_exp env exp)

  | Expression exp ->
      Expression (resolve_exp env exp)

  | If (condition, then_stmt, else_stmt) ->
      let condition = resolve_exp env condition in
      let then_stmt = resolve_statement env then_stmt in
      let else_stmt =
        match else_stmt with
        | None -> None
        | Some stmt -> Some (resolve_statement env stmt)
      in
      If (condition, then_stmt, else_stmt)

  | Compound block ->
      let inner_env = copy_env_for_inner_block env in
      Compound (resolve_block inner_env block)

  | Null ->
      Null

and resolve_declaration env decl =
  match decl with
  | Declaration (name, init) ->
      if StringMap.mem name env
         && (StringMap.find name env).from_current_block then
        raise (ResolveError ("Duplicate variable declaration: " ^ name))
      else
        let unique_name = make_unique name in
        let env =
          StringMap.add
            name
            { unique_name; from_current_block = true }
            env
        in
        let init =
          match init with
          | None -> None
          | Some exp -> Some (resolve_exp env exp)
        in
        (env, Declaration (unique_name, init))

and resolve_block_item env item =
  match item with
  | D decl ->
      let env, decl = resolve_declaration env decl in
      (env, D decl)

  | S stmt ->
      let stmt = resolve_statement env stmt in
      (env, S stmt)

and resolve_block env block =
  match block with
  | Block items ->
      let _, resolved_items =
        List.fold_left
          (fun (env, acc) item ->
            let env, item = resolve_block_item env item in
            (env, acc @ [item]))
          (env, [])
          items
      in
      Block resolved_items

let resolve_function = function
  | Function (name, body) ->
      Function (name, resolve_block StringMap.empty body)

let resolve_program = function
  | Program func ->
      Program (resolve_function func)