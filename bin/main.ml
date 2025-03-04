(* bin/main.ml *)

open Lexer
open Parser
open Eval

let () =
  (* Nome do arquivo de entrada *)
  let filename = "input.txt" in

  (* Abrir e ler o conteúdo do arquivo *)
  let input =
    let ic = open_in filename in
    try
      let line = really_input_string ic (in_channel_length ic) in
      close_in ic;
      line
    with e ->
      close_in_noerr ic;
      raise e
  in

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
        List.iter (fun (var, value) -> Printf.printf "%s = %d; " var value) env;
        process_tokens env rest
  in

  process_tokens [] tokens
