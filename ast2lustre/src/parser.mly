/* File parser.mly */

/* header */
%{
	let parse_error s =
		prerr_endline "Systax Error"
	;;
	open Tree
%}

/* tokens */
/* keywords */
%token MAIN TOPLEVEL NODE PROGRAM FUNCTION
%token PARAMS RETURNS BODY LOCALVARS
%token ID
%token COMMA
%token LBRACKET RBRACKET
%token EOF

%token EQUAL LVALUE CLOCK

%token UNIMPORT IMPORTED

%token VAR_DECLS VARS

%token BINOP_ADD
/* const */
%token INTC UINTC SHORTC USHORTC REALC FLOATC
/* kind */
%token INTK UINTK SHORTK USHORTK REALK FLOATK

/* value */
%token CONST_INT CONST_FLO CONST_CHAR TRUEV FALSEV
%token <string> IDENT GUID COMMENT


%start programY			/* the entry point */
%type <Tree.tree> programY
%%

programY:
	TOPLEVEL LBRACKET mainY COMMA programBlkY RBRACKET
		{tree ($3, $5)}
;

mainY:
	MAIN LBRACKET IDENT RBRACKET
		{mainBlk $3}
;

programBlkY:
		stmtBlkYs	{[$1]}
	|				{[]}
;

stmtBlkYs:
		stmtBlkY COMMA stmtBlkYs	{$1::$3}
	|	stmtBlkY 					{[$1]}
;

stmtBlkY:
		typeStmtY	{$1}
	|	constStmtY	{$1}
	|	nodeStmtY	{$1}
;

nodeStmtY:
	NODE LBRACKET nodeKindY COMMA GUID COMMA IDENT COMMA commentY COMMA paramBlkY COMMA returnBlkY COMMA bodyBlkY
		{nodeStmt ($3, $5, $7, $9, $11, $13, $15)}
;

nodeKindY:
		NODE {Node}
	|	FUNCTION {Function}
;

paramBlkY:
	PARAMS LBRACKET declStmtYs RBRACKET
		{paramBlk($3)}
;

returnBlkY:
	RETURNS LBRACKET declStmtYs RBRACKET
		{returnBlk($3)}
;

bodyBlkY:
	BODY LBRACKET localVarYs assignStmtYs RBRACKET
		{bodyBlk($3,$5)}
;

localVarYs:
		LOCALVARS LBRACKET declStmtYs RBRACKET COMMA	{$3}
	|													{[]}
;

assignStmtYs:
		assignStmtY COMMA assignStmtYs	{$1::$3}
	|									{[]}
;

assignStmtY:
	EQUAL LBRACKET LVALUE LBRACKET lhsY RBRACKET COMMA exprY COMMA guidOpY COMMA guidValY COMMA importedY COMMA importCodeY
		{assignStmtY($5, %8, $10, $12, $14, $16)}
;

lhsY:
		ID LBRACKET IDENT COMMA kindY COMMA clockY RBRACKET	{lhs($3, $5, $7)}
	|														{lhs()}
;

exprY:
		BinOpExprY		{$1}
/*	|	ConstructExprY	{$1} */
	|	valueY			{$1}
/* todo */
;

BinOpExprY:
		binOpY LBRACKET kindY COMMA clockY COMMA exprY COMMA exprY RBRACKET
			{BinOpExpr($1, $3, $5, $7, $9)}
;

binOpY:
		BINOP_ADD	{ADD}
/* todo */
;

valueY:
		lhsY		{$1}
	|	INTC LBRACKET CONST_INT RBRACKET	{Int($3)}
	|	UINTC LBRACKET CONST_INT RBRACKET	{UInt($3)}
	|	SHORTC LBRACKET CONST_INT RBRACKET	{Short($3)}
	|	USHORTC LBRACKET CONST_INT RBRACKET	{UShort($3)}
	|	REALC LBRACKET CONST_FLO RBRACKET	{Real($3)}
	|	FLOATC LBRACKET CONST_FLO RBRACKET	{Float($3)}
/* todo*/
;

guidOpY:
		IDENT 	{$1}
	|			{[]}
;

guidValY:
		GUID 	{$1}
	|			{[]}
;

importedY:
		UNIMPORT	{false}
	|	IMPORTED	{true}
;

importCodeY:
	CONST_INT	{$1}
;

clockY:
		LBRACKET CLOCK RBRACKET	{clock($2)}
	|	LBRACKET RBRACKET		{clock()}
;


declStmtYs:
		declStmtY COMMA declStmtYs	{$1::$3}
	|								{[]}
;

declStmtY:
	VAR_DECLS LBRACKET VARS LBRACKET IDENT RBRACKET COMMA kindY COMMA commentY
		{declStmt($5, $8, $10)}
;

kindY:
		INTK	{Int}
	|	UINTK	{UInt}
	|	SHORTK	{Short}
	|	USHORTK	{UShort}
	|	REALK	{Real}
	|	FLOATK	{Float}
/* todo */
;

commentY:
		COMMENT						{$1}
	|	LBRACKET commentY RBRACKET	{$2}
;

/* todo */
constStmtY:
		{[]}
;
typeStmtY:
		{[]}
;
	