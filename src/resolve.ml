open Ast

exception ResolveError of string

module StringMap = Map.Make(String)

type map_entry = {
  unique_name : string;
  from_current_scope : bool;
  has_linkage : bool;
}

let counter = ref 0

let make_unique name =
  let unique = name ^ "." ^ string_of_int !counter in
  incr counter;
  unique

let copy_env_for_inner_scope env =
  StringMap.map
    (fun entry -> { entry with from_current_scope = false })
    env

let lookup_ident env name =
  if StringMap.mem name env then
    (StringMap.find name env).unique_name
  else
    raise (ResolveError ("Undeclared identifier: " ^ name))

let rec resolve_optional_exp env = function
  | None -> None
  | Some exp -> Some (resolve_exp env exp)

and resolve_exp env exp =
  match exp with
  | Constant _ -> exp

  | Var name ->
      Var (lookup_ident env name)

  | FunctionCall (name, args) ->
      let name = lookup_ident env name in
      let args = List.map (resolve_exp env) args in
      FunctionCall (name, args)

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

let resolve_local_name env name =
  if StringMap.mem name env
     && (StringMap.find name env).from_current_scope then
    raise (ResolveError ("Duplicate declaration: " ^ name))
  else
    let unique_name = make_unique name in
    let env =
      StringMap.add
        name
        {
          unique_name;
          from_current_scope = true;
          has_linkage = false;
        }
        env
    in
    (env, unique_name)

let rec resolve_variable_declaration env decl =
  match decl with
  | VariableDeclaration (name, init) ->
      let env, unique_name = resolve_local_name env name in
      let init = resolve_optional_exp env init in
      (env, VariableDeclaration (unique_name, init))

and resolve_function_declaration env func_decl =
  match func_decl with
  | FunctionDeclaration (name, params, body) ->
      if StringMap.mem name env then (
        let old = StringMap.find name env in
        if old.from_current_scope && not old.has_linkage then
          raise (ResolveError ("Duplicate declaration: " ^ name))
      );

      let env =
        StringMap.add
          name
          {
            unique_name = name;
            from_current_scope = true;
            has_linkage = true;
          }
          env
      in

      let param_env = copy_env_for_inner_scope env in
      let param_env, params =
        List.fold_left
          (fun (env, acc) param ->
            let env, unique_param = resolve_local_name env param in
            (env, acc @ [unique_param]))
          (param_env, [])
          params
      in

      let body =
        match body with
        | None -> None
        | Some block -> Some (resolve_block param_env block)
      in

      (env, FunctionDeclaration (name, params, body))

and resolve_declaration env decl =
  match decl with
  | VarDecl var_decl ->
      let env, var_decl = resolve_variable_declaration env var_decl in
      (env, VarDecl var_decl)

  | FunDecl func_decl ->
      (match func_decl with
       | FunctionDeclaration (_, _, Some _) ->
           raise (ResolveError "Nested function definitions are not allowed")
       | FunctionDeclaration _ ->
           let env, func_decl = resolve_function_declaration env func_decl in
           (env, FunDecl func_decl))

and resolve_for_init env init =
  match init with
  | InitDecl var_decl ->
      let env, var_decl = resolve_variable_declaration env var_decl in
      (env, InitDecl var_decl)

  | InitExp exp_opt ->
      (env, InitExp (resolve_optional_exp env exp_opt))

and resolve_statement env stmt =
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
      let inner_env = copy_env_for_inner_scope env in
      Compound (resolve_block inner_env block)

  | Break label -> Break label
  | Continue label -> Continue label

  | While (condition, body, label) ->
      While (resolve_exp env condition, resolve_statement env body, label)

  | DoWhile (body, condition, label) ->
      DoWhile (resolve_statement env body, resolve_exp env condition, label)

  | For (init, condition, post, body, label) ->
      let loop_env = copy_env_for_inner_scope env in
      let loop_env, init = resolve_for_init loop_env init in
      let condition = resolve_optional_exp loop_env condition in
      let post = resolve_optional_exp loop_env post in
      let body = resolve_statement loop_env body in
      For (init, condition, post, body, label)

  | Null -> Null

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

let resolve_program = function
  | Program funcs ->
      let _, funcs =
        List.fold_left
          (fun (env, acc) func_decl ->
            let env, func_decl = resolve_function_declaration env func_decl in
            (env, acc @ [func_decl]))
          (StringMap.empty, [])
          funcs
      in
      Program funcs