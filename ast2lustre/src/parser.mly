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
%token LPAREN RPAREN
%token EOF

%token EQUAL LVALUE CLOCK

%token NOIMPORT IMPORTED

%token VAR_DECLS VARS

%token BINOP_ADD

%token INT UINT SHORT USHORT REAL FLOAT

%token NOCALL NOGUID

/* value */
%token TRUE FALSE
%token <string> IDENT GUID COMMENT CONST_INT CONST_FLO CONST_CHAR
%token NULLCOMMENT


%start programY			/* the entry point */
%type <Tree.topLevel> programY
%%

programY:
	TOPLEVEL LPAREN mainY COMMA programBlkY RPAREN
		{TopLevel ($3, $5)}
;

mainY:
	MAIN LPAREN IDENT RPAREN
		{MainBlk $3}
;

programBlkY:
		stmtBlkYs	{ProgramBlk $1}
	|				{ProgramBlk []}
;

stmtBlkYs:
		stmtBlkY COMMA stmtBlkYs	{$1::$3}
	|	stmtBlkY 					{[$1]}
;

stmtBlkY:
		typeBlkY	{$1}
	|	constBlkY	{$1}
	|	nodeBlkY	{$1}
;

nodeBlkY:
	NODE LPAREN nodeKindY COMMA GUID COMMA IDENT COMMA commentY COMMA paramBlkY COMMA returnBlkY COMMA bodyBlkY RPAREN
		{NodeBlk ($3, $5, $7, $9, $11, $13, $15)}
;

nodeKindY:
		NODE {Node}
	|	FUNCTION {Function}
;

paramBlkY:
	PARAMS LPAREN declStmtYs RPAREN
		{ParamBlk $3}
;

returnBlkY:
	RETURNS LPAREN declStmtYs RPAREN
		{ReturnBlk $3}
;

bodyBlkY:
	BODY LPAREN localVarYs assignStmtYs RPAREN
		{BodyBlk($3,$4)}
;

localVarYs:
		LOCALVARS LPAREN declStmtYs RPAREN COMMA	{$3}
	|													{[]}
;

assignStmtYs:
		assignStmtY COMMA assignStmtYs	{$1::$3}
	|									{[]}
;

assignStmtY:
	EQUAL LPAREN LVALUE LPAREN lhsY RPAREN COMMA exprY COMMA guidOpY COMMA guidValY COMMA importedY COMMA importCodeY
		{AssignStmt($5, $8, $10, $12, $14, $16)}
;

lhsY:
		ID LPAREN IDENT COMMA kindY COMMA clockY RPAREN	{ID($3, $5, $7)}
/*	|													{ID("", BOOL, NOCLOCK)} 
Q:annoymous_id */

;

exprY:
		BinOpExprY			{$1}
	|	atomExprY			{AtomExpr $1}
/* todo */
;

BinOpExprY:
		binOpY LPAREN kindY COMMA clockY COMMA exprY COMMA exprY RPAREN
			{BinOpExpr($1, $3, $5, $7, $9)}
;

binOpY:
		BINOP_ADD	{ADD}
/* todo */
;

atomExprY:
		ID LPAREN IDENT COMMA kindY COMMA clockY RPAREN	{EID($3, $5, $7)}
	|	ID LPAREN IDENT RPAREN							{EIdent($3)}
	|	INT LPAREN CONST_INT RPAREN						{EInt($3)}
	|	UINT LPAREN CONST_INT RPAREN					{EUInt($3)}
	|	SHORT LPAREN CONST_INT RPAREN					{EShort($3)}
	|	USHORT LPAREN CONST_INT RPAREN					{EUShort($3)}
	|	REAL LPAREN CONST_FLO RPAREN					{EReal($3)}
	|	FLOAT LPAREN CONST_FLO RPAREN					{EFloat($3)}
/* todo*/
;

guidOpY:
		IDENT 	{GUIDOp $1}
	|			{NOCALL}
;

guidValY:
		GUID 	{GUIDVal $1}
	|			{NOGUID}
;

importedY:
		NOIMPORT	{NOIMPORT}
	|	IMPORTED	{IMPORTED}
;

importCodeY:
	CONST_INT	{ImportCode $1}
;

clockY:
		LPAREN clockY RPAREN	{$2}
	|	LPAREN RPAREN		{NOCLOCK}
;


declStmtYs:
		declStmtY COMMA declStmtYs	{$1::$3}
	|								{[]}
;

declStmtY:
	VAR_DECLS LPAREN VARS LPAREN IDENT RPAREN COMMA kindY COMMA commentY
		{DeclStmt($5, $8, $10)}
;

kindY:
		INT	{Int}
	|	UINT	{UInt}
	|	SHORT	{Short}
	|	USHORT	{UShort}
	|	REAL	{Real}
	|	FLOAT	{Float}
/* todo */
;

commentY:
		COMMENT						{Comment $1}
	|	LPAREN commentY RPAREN	{$2}
	|	NULLCOMMENT					{NULL_COMMENT}
;

/* todo */
constBlkY:
		{ConstBlk[]}
;
typeBlkY:
		{TypeBlk[]}
;
