(* lib/eval.ml *)

open Ast

let rec eval_expr env expr =
  match expr with
  | Int n -> n
  | Bool b -> if b then 1 else 0
  | Var x -> (
      try List.assoc x env
      with Not_found -> failwith ("Variável não definida: " ^ x))
  | BinOp (op, e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match op with
      | "+" -> v1 + v2
      | "-" -> v1 - v2
      | "*" -> v1 * v2
      | "/" -> if v2 <> 0 then v1 / v2 else failwith "Divisão por zero"
      | "==" -> if v1 = v2 then 1 else 0
      | "<" -> if v1 < v2 then 1 else 0
      | ">" -> if v1 > v2 then 1 else 0
      | _ -> failwith ("Operador desconhecido: " ^ op))
  | If (cond, then_expr, else_expr) ->
      if eval_expr env cond <> 0 then eval_expr env then_expr else eval_expr env else_expr
  | FunCall (_name, _args) ->
      failwith "Chamada de função ainda não implementada"

let eval_stmt env stmt =
  match stmt with
  | Assign (var, expr) ->
      let value = eval_expr env expr in
      (var, value) :: env
  | Expr expr ->
      let result = eval_expr env expr in
      Printf.printf "Result: %d\n" result;
      env
  | Print expr ->
      Printf.printf "%d\n" (eval_expr env expr); env
  | FunDef (_name, _params, _body) ->
      failwith "Definição de função ainda não implementada"

  | IfStmt (cond, then_expr, else_expr) ->
    let _ = if eval_expr env cond <> 0 then eval_expr env then_expr
            else eval_expr env else_expr
    in env

