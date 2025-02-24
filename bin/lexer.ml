(* lib/lexer.ml *)

type token =
  | INT of int
  | VAR of string
  | BOOL of bool
  | PLUS | MINUS | MULT | DIV
  | ASSIGN | SEMICOLON
  | LPAREN | RPAREN | LBRACE | RBRACE
  | IF | ELSE | FUN | PRINT
  | EQ | LT | GT

let tokenize input =
  let len = String.length input in
  let rec aux i acc =
    if i >= len then List.rev acc
    else
      match input.[i] with
      | ' ' | '\t' | '\n' -> aux (i + 1) acc
      | '+' -> aux (i + 1) (PLUS :: acc)
      | '-' -> aux (i + 1) (MINUS :: acc)
      | '*' -> aux (i + 1) (MULT :: acc)
      | '/' -> aux (i + 1) (DIV :: acc)
      | '=' ->
          if i + 1 < len && input.[i + 1] = '=' then aux (i + 2) (EQ :: acc)
          else aux (i + 1) (ASSIGN :: acc)
      | '<' -> aux (i + 1) (LT :: acc)
      | '>' -> aux (i + 1) (GT :: acc)
      | ';' -> aux (i + 1) (SEMICOLON :: acc)
      | '(' -> aux (i + 1) (LPAREN :: acc)
      | ')' -> aux (i + 1) (RPAREN :: acc)
      | '{' -> aux (i + 1) (LBRACE :: acc)
      | '}' -> aux (i + 1) (RBRACE :: acc)
      | c when '0' <= c && c <= '9' -> (
          let rec read_number j n =
            if j < len && '0' <= input.[j] && input.[j] <= '9' then
              read_number (j + 1) (n * 10 + (Char.code input.[j] - Char.code '0'))
            else (n, j)
          in
          let (num, next_i) = read_number i 0 in
          aux next_i (INT num :: acc))
      | c when ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') -> (
          let rec read_var j =
            if j < len && (('a' <= input.[j] && input.[j] <= 'z') || ('A' <= input.[j] && input.[j] <= 'Z')) then
              read_var (j + 1)
            else j
          in
          let var_name = String.sub input i (read_var i - i) in
          let token = match var_name with
            | "if" -> IF
            | "else" -> ELSE
            | "fun" -> FUN
            | "print" -> PRINT
            | "true" -> BOOL true
            | "false" -> BOOL false
            | _ -> VAR var_name
          in
          aux (read_var i) (token :: acc))
      | _ -> failwith ("Caractere inesperado: " ^ String.make 1 input.[i])
  in
  aux 0 []
