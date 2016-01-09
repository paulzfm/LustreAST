/* File parser.mly */

/* header */
%{
	let parse_error s =
		print_string s;
	;;
	open Tree
%}

/* ? for xu: I can't compile lexer.mll. Please use make to have a try. */
/* ? for xu: There is no filed. It's field. */

/* ? for zhu: check integer and Int */

/* tokens */
/* keywords */
%token TYPE PRIVATE PUBLIC PROTECTED FUNCTION NODE RETURNS LET TEL
%token VAR CONST PRE FBY IF THEN ELSE WHEN CASE OF DEFAULTPATTERN ARROW SEG ENUM MAKE FLATTEN WITH
%token SHORTSSS INTSSS FLOATSSS REALSSS NOTSSS ADDSSS MINUSSSS
%token SSSADDSSS SSSMINUSSSS SSSMULSSS SSSDIVFSSS SSSDIVSSS SSSMODSSS SSSANDSSS SSSORSSS SSSXORSSS
%token SSSEQSSS SSSNESSS SSSGRESSS SSSGREEQSSS SSSLESSSS SSSLESEQSSS
%token MAPI MAP FOLDI FOLD MAPFOLD MAPWI FOLDWI MAPW FOLDW DEFAULT
%token CHAR BOOL SHORT USHORT INT UINT FLOAT REAL

%token NOT ADD MINUS MUL DIVF DIV MOD AND OR XOR LESEQ GREEQ NE EQ LES GRE


%left OR
%left XOR
%left AND
%left LES LESEQ GRE GREEQ
%left EQ NE
%left ADD MINUS
%left MUL DIVF DIV MOD
%right NOT
%left CARET

%token TRUE FALSE
%token <string> CONST_USINT CONST_SINT CONST_UINT CONST_INT CONST_FLO CONST_REAL IDENT CONST_CHAR

%token LPAREN RPAREN COMMA COLON SEMICOLON CARET LBRACKET RBRACKET LBRACE RBRACE DOT

%token EOF


%start programY			/* the entry point */
%type <Tree.program> programY
%%

programY: nodeBlksY EOF
	{Program $1}
;

nodeBlksY:
		nodeBlkY nodeBlksY	{$1::$2}
	|						{[]}

nodeBlkY:
		typeBlkY	{$1}
	|	constBlkY	{$1}
	|	funcBlkY	{$1}
;

typeBlkY:
	TYPE typeStmtsY	{TypeBlk $2}
;

typeStmtsY:
		typeStmtY typeStmtsY	{$1::$2}
	|						{[]}
;

typeStmtY:
	modifierY IDENT EQ kindY SEMICOLON
		{TypeStmt($1, $2, $4)}
;

modifierY:
		PRIVATE		{Private}
	|	PUBLIC		{Public}
	|	PROTECTED	{Protected}
	|				{NOMODIFIER}
;

kindY:
		atomTypeY			{AtomType $1}
	|	structTypeY			{$1}
	|	kindY CARET exprY	{Array($1, $3)}
	|	IDENT				{IDENT $1}
	|	enumTypeY			{$1}
;

enumTypeY:
	ENUM LBRACE identsY RBRACE
		{EnumType $3}
;

structTypeY:
	LBRACE fieldsY RBRACE
		{Struct $2}
;

fieldsY:
		fieldY COMMA fieldsY	{$1::$3}
	|	fieldY					{[$1]}
;

fieldY:
	identsY COLON kindY
		{Field($1, $3)}
;

atomTypeY:
		CHAR	{Char}
	|	BOOL	{Bool}
	|	INT		{Int}
	|	UINT	{UInt}
	|	SHORT	{Short}
	|	USHORT	{UShort}
	|	FLOAT	{Float}
	|	REAL	{Real}
;

constBlkY:
	CONST constStmtsY
		{ConstBlk $2}
;

constStmtsY:
		constStmtY constStmtsY	{$1::$2}
	|							{[]}
;

constStmtY:
	modifierY IDENT COLON kindY EQ exprY SEMICOLON
		{ConstStmt($1, $2, $4, $6)}
;

funcBlkY:
	funcTypeY modifierY IDENT paramBlkY returnBlkY funcBodyY
		{FuncBlk($1, $2, $3, $4, $5, $6)}
;

funcTypeY:
		FUNCTION	{Function}
	|	NODE		{Node}
;

paramBlkY:
	LPAREN fields2Y RPAREN
		{ParamBlk($2)}
;

returnBlkY:
	RETURNS LPAREN fields2Y RPAREN
		{ReturnBlk($3)}
;

fields2Y:
		fieldY SEMICOLON fields2Y	{$1::$3}
	|	fieldY						{[$1]}
	|								{[]}
;

funcBodyY:
		varBlkY LET eqStmtsY TEL	{BodyBlk($1, $3)}
	|	SEMICOLON					{NOBODYBLK}
;

varBlkY:
		VAR fields3Y	{VarList $2}
	|				{NOVARBLK}
;

fields3Y:
		fieldY SEMICOLON fields3Y	{$1::$3}
	|								{[]}
;

eqStmtsY:
		eqStmtY eqStmtsY	{$1::$2}
	|					{[]}
;

eqStmtY:
	lhsLY EQ exprY SEMICOLON
		{EqStmt($1, $3)}
;

lhsLY:
		lhs COMMA lhsLY	{$1::$3}
	|	lhs				{[$1]}
;

lhs:
		IDENT			{ID $1}
	|	DEFAULTPATTERN	{ANNOYMITY}

identsY:
		IDENT COMMA identsY	{$1::$3}
	|	IDENT				{[$1]}
;

atomExprY:
		IDENT		{EIdent $1}
	|	new_USINT	{EUShort $1}
	|	new_SINT	{EShort $1}
	|	new_INT		{EInt $1}
	|	new_UINT	{EUInt $1}
	|	new_REAL	{EReal $1}
	|	new_FLO		{EFloat $1}
	|	CONST_CHAR	{EChar $1}
	|	TRUE		{EBool "true"}
	|	FALSE		{EBool "false"}
;

unOpExprY:
	unOpY exprY
		{UnOpExpr($1, $2)}
;

unOpY:
		atomTypeY	{AtomTypeOp $1}
	|	NOT			{NOT}
	|	ADD			{POS}
	|	MINUS		{NEG}
;

binOpExprY:
		exprY OR exprY
			{BinOpExpr(OR, $1, $3)}
	|	exprY XOR exprY
			{BinOpExpr(XOR, $1, $3)}
	|	exprY AND exprY
			{BinOpExpr(AND, $1, $3)}
	|	exprY LES exprY
			{BinOpExpr(LT, $1, $3)}
	|	exprY GRE exprY
			{BinOpExpr(GT, $1, $3)}
	|	exprY GREEQ exprY
			{BinOpExpr(GE, $1, $3)}
	|	exprY LESEQ exprY
			{BinOpExpr(LE, $1, $3)}
	|	exprY NE exprY
			{BinOpExpr(NE, $1, $3)}
	|	exprY EQ exprY
			{BinOpExpr(EQ, $1, $3)}
	|	exprY ADD exprY
			{BinOpExpr(ADD, $1, $3)}
	|	exprY MINUS exprY
			{BinOpExpr(SUB, $1, $3)}
	|	exprY MUL exprY
			{BinOpExpr(MUL, $1, $3)}
	|	exprY DIVF exprY
			{BinOpExpr(DIVF, $1, $3)}
	|	exprY DIV exprY
			{BinOpExpr(DIV, $1, $3)}
	|	exprY MOD exprY
			{BinOpExpr(MOD, $1, $3)}
;

fieldExprY:
	exprY DOT IDENT
		{FieldExpr($1, $3)}
;

structExprY:
	LBRACE exprsY RBRACE
		{StructExpr $2}
;

exprsY:
		exprY COMMA exprsY	{$1::$3}
	|	exprY				{[$1]}
	|						{[]}
;

dynamicProjectExprY:
	LPAREN exprY DOT bracketExprsY DEFAULT exprY RPAREN
		{DynamicProjectExpr($2, $4, $6)}
;

bracketExprsY:
		bracketExprY bracketExprsY	{$1::$2}
	|								{[]}
;

bracketExprY:
	LBRACKET exprY RBRACKET
		{$2}
;

arrAccessExprY:
	exprY LBRACKET exprY RBRACKET
		{ArrAccessExpr($1, $3)}
;

arrInitExprY:
	exprY CARET exprY
		{ArrInitExpr($1, $3)}
;

arrConstructExprY:
	LBRACKET exprsY RBRACKET
		{ArrConstructExpr $2}
;

arrNameConstructExprY:
	LBRACE nameArrItemsY RBRACE
		{ArrNameConstructExpr $2}
;

nameArrItemsY:
		nameArrItemY COMMA nameArrItemsY	{$1::$3}
	|	nameArrItemY						{[$1]}
;

nameArrItemY:
	IDENT COLON exprY
		{NameArrItem($1, $3)}
;

preExprY:
	PRE exprY
		{PreExpr $2}
;

fbyExprY:
	FBY LPAREN exprsY SEMICOLON new_INT SEMICOLON exprsY RPAREN
		{FbyExpr($3, $5, $7)}
;

arrowExprY:
	exprY ARROW exprY
		{ArrowExpr($1, $3)}
;

whenExprY:
	exprY WHEN IDENT
		{WhenExpr($1, $3)}
;

ifExprY:
	IF exprY THEN exprY ELSE exprY
		{IfExpr($2, $4, $6)}
;

caseExprY:
	LPAREN CASE exprY OF casesY RPAREN
		{CaseExpr($3, $5)}
;

casesY:
		caseItemY casesY	{$1::$2}
	|						{[]}
;

caseItemY:
	SEG patternY COLON exprY
		{CaseItem($2, $4)}
;

withExprY:
	LPAREN IDENT WITH withItemsY EQ exprY RPAREN
		{WithExpr($2, $4, $6)}
;

withItemsY:
		withItemY withItemsY	{$1::$2}
	|						{[]}
;

withItemY:
		DOT IDENT				{FieldItem $2}
	|	LBRACKET exprY RBRACKET	{AccessItem $2}
;

patternY:
		IDENT				{PIdent $1}
	|	new_USINT			{PUShort $1}
	|	new_SINT			{PShort $1}
	|	new_INT				{PInt $1}
	|	new_UINT			{PUInt $1}
	|	new_REAL			{PReal $1}
	|	new_FLO				{PFloat $1}
	|	CONST_CHAR			{PChar $1}
	|	TRUE				{PBool "true"}
	|	FALSE				{PBool "false"}
	|	DEFAULTPATTERN		{DefaultPattern}
;

exprListY:
	exprsY
		{ExprList $1}
;

prefixExprY:
	prefixOpY LPAREN exprsY RPAREN
		{PrefixExpr($1, $3)}
;

prefixOpY:
		IDENT 						{Ident $1}
	|	prefixUnOpY					{UnOp $1}
	|	prefixBinOpY				{BinOp $1}
	|	LPAREN MAKE IDENT RPAREN	{Make $3}
	|	LPAREN FLATTEN IDENT RPAREN	{Flatten $3}
;

prefixUnOpY:
		SHORTSSS	{PSHORT}
	|	INTSSS		{PINT}
	|	FLOATSSS	{PFLOAT}
	|	REALSSS		{PREAL}
	|	NOTSSS		{PNOT}
	|	ADDSSS		{PPOS}
	|	MINUSSSS	{PNEG}
;

prefixBinOpY:
		SSSADDSSS	{PADD}
	|	SSSMINUSSSS	{PSUB}
	|	SSSMULSSS	{PMUL}
	|	SSSDIVFSSS	{PDIVF}
	|	SSSDIVSSS	{PDIV}
	|	SSSMODSSS	{PMOD}
	|	SSSANDSSS	{PAND}
	|	SSSORSSS	{POR}
	|	SSSXORSSS	{PXOR}
	|	SSSGRESSS	{PGT}
	|	SSSGREEQSSS	{PGE}
	|	SSSLESSSS	{PLT}
	|	SSSLESEQSSS	{PLE}
	|	SSSEQSSS	{PEQ}
	|	SSSNESSS	{PNE}
;

highOrderExprY:
		LPAREN highOrderOpY prefixOpY LES LES new_INT GRE GRE RPAREN LPAREN exprsY RPAREN
			{HighOrderExpr($2, $3, $6, $11)}
	|	LPAREN MAPWI prefixOpY LES LES new_INT GRE GRE IF exprY DEFAULT exprY RPAREN LPAREN exprsY RPAREN
			{MapwiExpr($3, $6, $10, $12, $15)}
	|	LPAREN MAPW prefixOpY LES LES new_INT GRE GRE IF exprY DEFAULT exprY RPAREN LPAREN exprsY RPAREN
			{MapwExpr($3, $6, $10, $12, $15)}
	|	LPAREN FOLDWI prefixOpY LES LES new_INT GRE GRE IF exprY RPAREN LPAREN exprsY RPAREN
			{FoldwiExpr($3, $6, $10, $13)}
	|	LPAREN FOLDW prefixOpY LES LES new_INT GRE GRE IF exprY RPAREN LPAREN exprsY RPAREN
			{FoldwExpr($3, $6, $10, $13)}
;

highOrderOpY:
		MAP		{MAP}
	|	FOLD	{FOLD}
	|	MAPI	{MAPI}
	|	FOLDI	{FOLDI}
	|	MAPFOLD	{MAPFOLD}
;

exprY:
		atomExprY				{AtomExpr $1}
	|	unOpExprY				{$1}
	|	binOpExprY				{$1}
	|	fieldExprY				{$1}
	|	structExprY				{$1}
	|	dynamicProjectExprY		{$1}
	|	arrAccessExprY			{$1}
	|	arrInitExprY			{$1}
	|	arrConstructExprY		{$1}
	|	arrNameConstructExprY	{$1}
	|	preExprY				{$1}
	|	fbyExprY				{$1}
	|	arrowExprY				{$1}
	|	whenExprY				{$1}
	|	ifExprY					{$1}
	|	caseExprY				{$1}
	|	withExprY				{$1}
	|	LPAREN exprListY RPAREN	{$2}
	|	LPAREN exprY RPAREN		{$2}
	|	prefixExprY				{$1}
	|	highOrderExprY			{$1}
;


new_INT:
		CONST_INT	{$1}
	|	ADD CONST_INT	{$2}
	|	MINUS CONST_INT {"-" ^ $2}
;

new_SINT:
		CONST_SINT	{$1}
	|	ADD CONST_SINT	{$2}
	|	MINUS CONST_SINT {"-" ^ $2}
;

new_USINT:
		CONST_USINT	{$1}
	|	ADD CONST_USINT	{$2}
	|	MINUS CONST_USINT {"-" ^ $2}
;

new_UINT:
		CONST_UINT	{$1}
	|	ADD CONST_UINT	{$2}
	|	MINUS CONST_UINT {"-" ^ $2}
;

new_REAL:
		CONST_REAL	{$1}
	|	ADD CONST_REAL	{$2}
	|	MINUS CONST_REAL {"-" ^ $2}
;

new_FLO:
		CONST_FLO	{$1}
	|	ADD CONST_FLO	{$2}
	|	MINUS CONST_FLO {"-" ^ $2}
;
