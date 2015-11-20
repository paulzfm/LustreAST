(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
	[' ' '\t' '\n' '\r']	{ token lexbuf }     (* skip blanks *)
	| "TopLevel"	{ TOPLEVEL}
	| "main"	{ MAIN }
	| "program"	{ PROGRAM }
	| "node"	{ NODE }

	| "NullComment"	{ NULLCOMMENT }
	| "int"			{ INT }
	| "vars" 		{ VARS }
	| "short" 		{ SHORT }
	| "SHORT"		{ SHORT }
	| "ushort" 		{ USHORT }
	| "USHORT" 		{ USHORT }
	| "INT"			{ INT }
	| "UINT"		{ UINT }
	| "uint" 		{ UINT }
	| "REAL" 		{ REAL }
	| "real"		{ REAL }
	| "FLOAT" 		{ FLOAT }
	| "float" 		{ FLOAT }

	| "body" 		{ BODY }
	| "var_decls" 	{ VAR_DECLS }
	| "body"		{ BODY }
	| "lvalue"		{ LVALUE }
	| "ID"			{ ID }
	| "binop_add"	{ BINOP_ADD }
	| "NOCALL"		{ NOCALL }
	| "NOGUID"		{ NOGUID }
	| "NOIMPORT"	{ NOIMPORT }
	| "NOIMPORT"	{ NOIMPORT }


	| "returns"	{ RETURNS }

	| "params"	{ PARAMS }


	| '('		{ LPAREN }
	| ')'		{ RPAREN }
    | ','       { COMMA  }


	| ['0'-'9']+ as lxm	{ CONST_INT lxm }
	| "true" 			{ TRUE }
	| "false"			{ FALSE }
	(*************definition of float***************)
	| ['0'-'9']+ '.' ['0'-'9']+ as lxm	{ CONST_FLO lxm }
	| '\''['a'-'z''A'-'Z']'\'' as lxm	{ CONST_CHAR (String.sub lxm 1 1) }
	| ['a'-'z''A'-'Z''_''$']['a'-'z''A'-'Z''0'-'9''_''$']* as lxm	{ IDENT (lxm) }

  	| eof            { EOF }
    | _              { print_string "unexpected token"; token lexbuf}

	(*error |		 {} *)
