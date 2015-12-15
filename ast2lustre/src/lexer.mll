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

	| "NullComment"	{ NULLCOMMENT }
	| "#type_block"	{ TYPE_BLOCK }
	| "type"		{ TYPE }
	| "field"		{ FIELD }
	| "#construct_enum" { CONSTRUCT_ENUM }
	| "array"		{ ARRAY }
	| "typename"	{ TYPENAME }
	| "bool"		{ BOOL }
	| "int"			{ INT }
	| "vars" 		{ VARS }
	| "char"		{ CHAR }
	| "short" 		{ SHORT }
	| "ushort" 		{ USHORT }
	| "uint" 		{ UINT }
	| "real"		{ REAL }
	| "float" 		{ FLOAT }

	| "const"		{ CONST }
	| "true"		{ TRUE }
	| "false"		{ FALSE }
	| "ID"			{ ID }
	| "BOOL"		{ BOOL }
	| "CHAR"		{ CHAR }
	| "SHORT"		{ SHORT }
	| "USHORT"		{ USHORT }
	| "INT"			{ INT }
	| "UINT"		{ UINT }
	| "REAL" 		{ REAL }
	| "construct"	{ CONSTRUCT }
	| "#label_const"	{ LABEL_CONST }
	| "#construct_array"	{ CONSTRUCT_ARRAY }
	| "FLOAT" 		{ FLOAT }

	| "body" 		{ BODY }
	| "#var_decls" 	{ VAR_DECLS }
	| "body"		{ BODY }
	| "lvalue"		{ LVALUE }
	| "ID"			{ ID }
	| "#binop_add"	{ BINOP_ADD }

	| "node"		{ NODE }
	| "function"	{ FUNCTION }
	| "params"		{ PARAMS }
	| "returns"		{ RETURNS }
	| "localvars"	{ LOCALVARS }
	| "NOCALL"		{ NOCALL }
	| "NOGUID"		{ NOGUID }
	| "IMPORTED"	{ IMPORTED }
	| "NOIMPORT"	{ NOIMPORT }

	| "#binop_substract"	{ BINOP_SUBSTRACT }
	| "#binop_multiply"	{ BINOP_MULTIPLY }
	| "#binop_divide"	{ BINOP_DIVIDE }
	| "#binop_div"	{ BINOP_DIV }
	| "#binop_mod"	{ BINOP_MOD }
	| "#binop_and"	{ BINOP_AND }
	| "#binop_or"	{ BINOP_OR }
	| "#binop_xor"	{ BINOP_XOR }
	| "#binop_gt"	{ BINOP_GT }
	| "#binop_lt"	{ BINOP_LT }
	| "#binop_ge"	{ BINOP_GE }
	| "#binop_le"	{ BINOP_LE }
	| "#binop_eq"	{ BINOP_EQ }
	| "#binop_neq"	{ BINOP_NEQ }
	| "#unop_shortcast"	{ UNOP_SHORTCAST }
	| "#unop_intcast"	{ UNOP_INTCAST }
	| "#unop_floatcast"	{ UNOP_FLOATCAST }
	| "#unop_realcast"	{ UNOP_REALCAST }
	| "#unop_not"	{ UNOP_NOT }
	| "#unop_pos"	{ UNOP_POS }
	| "#unop_neg"	{ UNOP_NEG }
	| "#if_expr"	{ IF_EXPR }
	| "#switch_expr"	{ SWITCH_EXPR }
	| "case"	{ CASE }
	| "#tempo_pre"	{ TEMPO_PRE }
	| "#tempo_arrow"	{ TEMPO_ARROW }
	| "#tempo_fby"	{ TEMPO_FBY }
	| "#mixed_constructor"	{ MIXED_CONSTRUCTOR }
	| "#struct_iterm"	{ STRUCT_ITERM }
	| "#field_access"	{ FIELD_ACCESS }
	| "#list_expr"	{ LIST_EXPR }
	| "#apply_expr"	{ APPLY_EXPR }
	| "make"	{ MAKE }
	| "flatten"	{ FLATTEN }
	| "#high_order"	{ HIGH_ORDER }
	| "prefix"	{ PREFIX }
	| "#highorder_map"	{ HIGHORDER_MAP }
	| "#highorder_fold"	{ HIGHORDER_FOLD }
	| "#highorder_mapfold"	{ HIGHORDER_MAPFOLD }
	| "#highorder_mapapi"	{ HIGHORDER_MAPI }
	| "#highorder_foldi"	{ HIGHORDER_FOLDI }
	| "#mapwi_default"	{ MAPWI_DEFAULT }
	| "#mapw_default"	{ MAPW_DEFAULT }
	| "#foldw_if"	{ FOLDW_IF }
	| "foldwi"	{ FOLDWI }
	| "short$"	{ SHORTSSS }
	| "int$"	{ INTSSS }
	| "float$"	{ FLOATSSS }
	| "real$"	{ REALSSS }
	| "not$"	{ NOTSSS }
	| "+$"	{ ADDSSS }
	| "-$"	{ MINUSSSS }
	| "$+$"	{ SSSADDSSS }
	| "$-$"	{ SSSMINUSSSS }
	| "$*$"	{ SSSMULSSS }
	| "$/$"	{ SSSDIVDIVSSS }
	| "$div$"	{ SSSDIVSSS }
	| "$mod$"	{ SSSMODSSS }
	| "$and$"	{ SSSANDSSS }
	| "$or$"	{ SSSORSSS }
	| "$xor$"	{ SSSXORSSS }
	| "$=$"	{ SSSEQSSS }
	| "$⟨⟩$"	{ SSSMIDSSS }
	| "$>$"	{ SSSGRESSS }
	| "$>=$"	{ SSSGREEQSSS }
	| "$<$"	{ SSSLESSSS }
	| "$<=$"	{ SSSLESEQSSS }
	| "dynamic_project"	{ DYNAMIC_PROJECT }
	| "typename"	{ TYPENAME }
	| "array_dim"	{ ARRAY_DIM }
	| "array_index"	{ ARRAY_INDEX }
	| "array_slice"	{ ARRAY_SLICE }


	| '('		{ LPAREN }
	| ')'		{ RPAREN }
    | ','       { COMMA  }
    | '='		{ EQUAL  }


	| ['0'-'9']+ as lxm	{ CONST_INT lxm }
	| "true" 			{ TRUE }
	| "false"			{ FALSE }
	| ['0'-'9''a'-'z']* as lxm	{ GUID lxm }
	(*************definition of float***************)
	| ['0'-'9']+ '.' ['0'-'9']+ as lxm	{ CONST_FLO lxm }
	| '\''['a'-'z''A'-'Z']'\'' as lxm	{ CONST_CHAR (String.sub lxm 1 1) }
	| ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']* as lxm	{ IDENT (lxm) }

  	| eof            { EOF }
    | _              { print_string "unexpected token"; token lexbuf}

	(*error |		 {} *)
