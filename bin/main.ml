(* bin/main.ml *)

open Lexer
open Parser
open Eval

let () =
  let input = "x = 11; print(if x < 10 then 100 else 200);" in
  Printf.printf "Entrada: %s\n" input;

  (* Tokenização *)
  let tokens = tokenize input in

  (* Parsing e Avaliação *)
  let rec process_tokens env tokens =
    match tokens with
    | [] -> ()
    | _ ->
        let (stmt, rest) = parse_stmt tokens in
        let env = eval_stmt env stmt in
        (* Exibir o ambiente atual *)
        Printf.printf "Environment: ";
        List.iter (fun (var, value) -> Printf.printf "%s = %d; " var value) env;
        Printf.printf "\n";
        process_tokens env rest
  in

  process_tokens [] tokens

