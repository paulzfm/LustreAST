(* File main.ml *)

open Tree

(* to Lustre *)

let toLustre = function
    Program (_) -> "OK"
;;

let _ =
	try
		let lexbuf = Lexing.from_channel stdin in
		let result = Parser.programY Lexer.token lexbuf in
            print_endline (toLustre result);
			flush stdout;
			exit 0
	with Parsing.Parse_error ->
		exit 2
;;
