(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
	[' ' '\t' '\n' '\r']	{ token lexbuf }     (* skip blanks *)
	| "TopLevel"	{ TOPLEVEL }
	| "main"	{ MAIN }
	| "program"	{ PROGRAM }
	| "node"	{ NODE }
	
	| "NullComment"	{ NULLCOMMENT }
	| "int"			{ INT }
	| "real"		{ REAL }
	| "vars" 		{ VARS }
	| "uint" 		{ UINT }
	| "short" 		{ SHORT }
	| "ushort" 		{ USHORT }
	| "float" 		{ FLOAT }
	| "body" 		{ BODY }
	| "var_decls" 	{ VAR_DECLS }
	| "uint" 		{ UINT }
	| "body"		{ BODY }
	| "lvalue"		{ LVALUE }
	| "ID"			{ ID }
	| "binop_add"	{ BINOP_ADD }
	| "INT"			{ INT }
	| "UINT"		{ UINT }
	| "NOCALL"		{ NOCALL }
	| "NOGUID"		{ NOGUID }
	| "NOIMPORT"	{ NOIMPORT }
	| "SHORT"		{ SHORT }
	| "NOIMPORT"	{ NOIMPORT }


	| "returns"	{ RETURNS }
	| "let"		{ LET }
	| "tel"		{ TEL }

	| "params"	{ params }
	| "clock"	{ CLOCK }
	| "var"		{ VAR }
	| "pre"		{ PRE }
	| "fby"		{ FBY }

	| "if"		{ IF }
	| "then" 	{ THEN }
	| "else"	{ ELSE }
	| "case" 	{ CASE }
	| "of"		{ OF }

	| "->"		{ ARROW }
	| '+'		{ PLUS }
	| '-'		{ MINUS }
	| '*'		{ TIMES }
	| '/'		{ DIVIDE }
	| "mod"		{ MOD }
	| "div"		{ DIV }

	| "and"		{ AND }
	| "or"		{ OR }
	| "xor"		{ XOR }
	| "not"		{ NOT }

	| '='		{ EQUAL }
	| "<>"		{ NEQUAL }
	| '<'		{ SMALL }
	| '>'		{ LARGE }
	| "<="		{ ESMALL }
	| ">="		{ ELARGE }

	| '('		{ LPAREN }
	| ')'		{ RPAREN }
	| '['		{ LBRACK }
	| ']'		{ RBRACK }
	| '{'		{ LBRACE }
	| '}'		{ RBRACE }

	| ':'		{ COLON }
	| ';'		{ SEMICO }
	| ','		{ COMMA }
	| '^'		{ CARET }
	| '_'		{ UNDERLINE }
	
	| '|'		{ VERBAR }
	| '.'		{ DOT }

	| ['0'-'9']+ as lxm	{ CONST_INT lxm }
	| "true" 			{ TRUE }
	| "false"			{ FALSE }
	(*************definition of float***************) 
	| ['0'-'9']+ '.' ['0'-'9']+ as lxm	{ CONST_FLO lxm }
	| '\''['a'-'z''A'-'Z']'\'' as lxm	{ CONST_CHAR (String.sub lxm 1 1) }
	| ['a'-'z''A'-'Z''_''$']['a'-'z''A'-'Z''0'-'9''_''$']* as lxm	{ IDENT (lxm) }	

  	| eof            { EOF }
	
	(*error |		 {} *)
