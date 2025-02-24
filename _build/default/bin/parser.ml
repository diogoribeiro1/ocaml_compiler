(* lib/parser.ml *)

open Ast
open Lexer

(* Função auxiliar para imprimir tokens *)
let string_of_token = function
  | INT n -> Printf.sprintf "INT(%d)" n
  | VAR x -> Printf.sprintf "VAR(%s)" x
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | MULT -> "MULT"
  | DIV -> "DIV"
  | ASSIGN -> "ASSIGN"
  | SEMICOLON -> "SEMICOLON"
  | IF -> "IF"
  | LT -> "LT"
  | GT -> "GT"
  | EQ -> "EQ"
  | ELSE -> "ELSE"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACE -> "LBRACE"
  | RBRACE -> "RBRACE"
  | FUN -> "FUN"
  | PRINT -> "PRINT"
  | _ -> "Token não tratado"

(* Parsing de expressões *)

(* Dispatch: if the token is IF, parse an if-expression; otherwise, parse a comparison. *)
let rec parse_expr tokens =
  match tokens with
  | IF :: rest -> parse_if_expr rest
  | _ -> parse_comparison tokens

and parse_if_expr tokens =
  (* Parses an if-expression: if <cond> then <expr> else <expr> *)
  let (cond, tokens_after_cond) = parse_expr tokens in
  (match tokens_after_cond with
   | VAR "then" :: rest1 ->
       let (then_expr, tokens_after_then) = parse_expr rest1 in
       (match tokens_after_then with
        | ELSE :: rest2 | VAR "else" :: rest2 ->
            let (else_expr, tokens_after_else) = parse_expr rest2 in
            (If (cond, then_expr, else_expr), tokens_after_else)
        | _ -> failwith "Erro de parsing: esperado 'else' após a expressão then.")
   | _ -> failwith "Erro de parsing: esperado 'then' após a condição if.")

and parse_comparison tokens =
  let (left, tokens) = parse_additive tokens in
  match tokens with
  | LT :: rest ->
      let (right, tokens_after) = parse_additive rest in
      (BinOp ("<", left, right), tokens_after)
  | GT :: rest ->
      let (right, tokens_after) = parse_additive rest in
      (BinOp (">", left, right), tokens_after)
  | EQ :: rest ->
      let (right, tokens_after) = parse_additive rest in
      (BinOp ("==", left, right), tokens_after)
  | _ -> (left, tokens)

and parse_additive tokens =
  let (left, tokens) = parse_multiplicative tokens in
  let rec aux left tokens =
    match tokens with
    | PLUS :: rest ->
        let (right, tokens_after) = parse_multiplicative rest in
        aux (BinOp ("+", left, right)) tokens_after
    | MINUS :: rest ->
        let (right, tokens_after) = parse_multiplicative rest in
        aux (BinOp ("-", left, right)) tokens_after
    | _ -> (left, tokens)
  in
  aux left tokens

and parse_multiplicative tokens =
  let (left, tokens) = parse_term tokens in
  let rec aux left tokens =
    match tokens with
    | MULT :: rest ->
        let (right, tokens_after) = parse_term rest in
        aux (BinOp ("*", left, right)) tokens_after
    | DIV :: rest ->
        let (right, tokens_after) = parse_term rest in
        aux (BinOp ("/", left, right)) tokens_after
    | _ -> (left, tokens)
  in
  aux left tokens

and parse_term tokens =
  match tokens with
  | INT n :: rest -> (Int n, rest)
  | VAR x :: rest -> (Var x, rest)
  | LPAREN :: rest ->
      let (expr, tokens_after_expr) = parse_expr rest in
      (match tokens_after_expr with
       | RPAREN :: rest' -> (expr, rest')
       | _ -> failwith "Erro de parsing: esperado RPAREN")
  | token :: _ ->
      Printf.printf "Erro: esperado número ou variável, mas encontrado %s\n" (string_of_token token);
      failwith "Erro de parsing: esperado número ou variável."
  | [] -> failwith "Erro de parsing: expressão incompleta."

(* Parsing de declarações *)

let rec parse_stmt tokens =
  match tokens with
  | [] -> (Expr (Int 0), [])
  | SEMICOLON :: rest -> parse_stmt rest
  | VAR var_name :: ASSIGN :: rest ->
      let (expr, tokens_after_expr) = parse_expr rest in
      (match tokens_after_expr with
       | SEMICOLON :: rest_tokens ->
           Printf.printf "Successfully parsed: %s = <expr>\n" var_name;
           (Assign (var_name, expr), rest_tokens)
       | token :: _ ->
           Printf.printf "Erro: token inesperado após expressão: %s\n" (string_of_token token);
           failwith "Esperado ';' após declaração."
       | [] -> failwith "Esperado ';' após declaração.")
  | PRINT :: LPAREN :: rest ->
      let (expr, tokens_after_expr) = parse_expr rest in
      (match tokens_after_expr with
       | RPAREN :: SEMICOLON :: rest_tokens -> (Print expr, rest_tokens)
       | _ -> failwith "Erro de sintaxe em print.")
  | IF :: rest -> parse_if_stmt rest
  | _ -> failwith "Token inesperado na declaração."

and parse_if_stmt tokens =
  (* Parses a top-level if-statement: if <cond> then <expr> else <expr> *)
  let (cond, tokens_after_cond) = parse_expr tokens in
  (match tokens_after_cond with
   | VAR "then" :: rest1 ->
       let (then_expr, tokens_after_then) = parse_expr rest1 in
       (match tokens_after_then with
        | ELSE :: rest2 | VAR "else" :: rest2 ->
            let (else_expr, tokens_after_else) = parse_expr rest2 in
            (IfStmt (cond, then_expr, else_expr), tokens_after_else)
        | _ -> failwith "Erro de parsing: esperado 'else' após a expressão then.")
   | _ -> failwith "Erro de parsing: esperado 'then' após a condição if.")

