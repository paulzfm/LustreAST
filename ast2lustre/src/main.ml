(* File main.ml *)

open Tree

let _ =
	try
		let lexbuf = Lexing.from_channel stdin in
		Parser.programY Lexer.token lexbuf;
		flush stdout;
		exit 0
	with Parsing.Parse_error ->
		exit 2
;;

