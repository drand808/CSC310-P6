open Project6.Utils

let print_usage () : unit =
  print_string "\nThis file functions as a driver for interfacing with the lexer and parser.\n\n";
  print_string "Usage:\n";
  print_string "\tdune exec bin/main.exe <mode> <filename>: Run your parser and lexer on standard input, or a file if one is provided.\n";
  print_string "Modes:\n";
  print_string "\tlex: Run in lex mode to show the tokens output by your lexer\n";
  print_string "\tparse: Parse a full program, including the function header, using parse_main\n\n";
  exit 1

let () =
  if Array.length Sys.argv < 2 then (print_usage (); exit 1)
  else
    let ch = if Array.length Sys.argv > 2 then open_in Sys.argv.(2) else stdin in
    let toks = tokenize_from_channel ch in

    (* This is intentionally physical equality *)
    if ch != stdin then close_in ch;
    print_string "\n";
    match Sys.argv.(1) with
    | "lex" ->
      print_string @@ string_of_list ~newline:true string_of_token toks
    | "parse" ->
      print_string @@ string_of_stmt @@ Project6.Parser.parse_main toks;
      print_newline ()
    | x -> print_usage (); failwith ("Unknown mode '" ^ x ^ "'")

