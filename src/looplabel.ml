open Ast

exception LoopLabelError of string

let counter = ref 0

let make_loop_label () =
  let label = "loop" ^ string_of_int !counter in
  incr counter;
  label

let rec label_statement current_label stmt =
  match stmt with
  | Return _ ->
      stmt

  | Expression _ ->
      stmt

  | Null ->
      stmt

  | Break _ ->
      (match current_label with
       | Some label -> Break (Some label)
       | None -> raise (LoopLabelError "break statement outside of loop"))

  | Continue _ ->
      (match current_label with
       | Some label -> Continue (Some label)
       | None -> raise (LoopLabelError "continue statement outside of loop"))

  | If (condition, then_stmt, else_stmt) ->
      let then_stmt = label_statement current_label then_stmt in
      let else_stmt =
        match else_stmt with
        | None -> None
        | Some stmt -> Some (label_statement current_label stmt)
      in
      If (condition, then_stmt, else_stmt)

  | Compound block ->
      Compound (label_block current_label block)

  | While (condition, body, _) ->
      let label = make_loop_label () in
      let body = label_statement (Some label) body in
      While (condition, body, Some label)

  | DoWhile (body, condition, _) ->
      let label = make_loop_label () in
      let body = label_statement (Some label) body in
      DoWhile (body, condition, Some label)

  | For (init, condition, post, body, _) ->
      let label = make_loop_label () in
      let body = label_statement (Some label) body in
      For (init, condition, post, body, Some label)

and label_block current_label block =
  match block with
  | Block items ->
      Block (List.map (label_block_item current_label) items)

and label_block_item current_label item =
  match item with
  | D _ ->
      item

  | S stmt ->
      S (label_statement current_label stmt)

let label_function = function
  | Function (name, body) ->
      Function (name, label_block None body)

let label_program = function
  | Program func ->
      Program (label_function func)