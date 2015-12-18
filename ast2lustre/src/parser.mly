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
%token MAIN TOPLEVEL NODE PROGRAM FUNCTION
%token PARAMS RETURNS BODY LOCALVARS
%token ID
%token COMMA
%token LPAREN RPAREN
%token EOF

%token EQUAL LVALUE CLOCK

%token NOIMPORT IMPORTED

%token VAR_DECLS VARS

%token BINOP_ADD BINOP_SUBTRACT  BINOP_MULTIPLY  BINOP_DIVIDE  BINOP_DIV  BINOP_MOD  BINOP_AND  BINOP_OR  BINOP_XOR  BINOP_GT  BINOP_LT  BINOP_GE  BINOP_LE  BINOP_EQ  BINOP_NEQ

%token UNOP_SHORTCAST UNOP_INTCAST UNOP_FLOATCAST UNOP_REALCAST UNOP_NOT UNOP_POS UNOP_NEG

%token INT UINT SHORT USHORT REAL FLOAT BOOL CHAR

%token NOCALL NOGUID

%token TYPENAME ARRAY CONSTRUCT_ENUM FIELD CONSTRUCT LABEL_EXPR

%token IF_EXPR SWITCH_EXPR CASE LABEL_CONST CONSTRUCT_ARRAY LIST_EXPR TEMPO_FBY TEMPO_ARROW TEMPO_PRE FIELD_ACCESS
%token STRUCT_ITERM MIXED_CONSTRUCTOR CONSTRUCT_ARRAY
%token ARRAY_DIM ARRAY_INDEX ARRAY_SLICE DYNAMIC_PROJECT

%token APPLY_EXPR MAKE FLATTEN HIGH_ORDER MAPW_DEFAULT MAPWI_DEFAULT FOLDW_IF FOLDWI

%token PREFIX PARAM_TYPES RET_TYPES

%token HIGHORDER_MAP HIGHORDER_FOLD HIGHORDER_MAPFOLD HIGHORDER_MAPI HIGHORDER_FOLDI

%token CONST CONST_BLOCK TYPE TYPE_BLOCK

%token ADDSSS MINUSSSS SSSADDSSS SSSMINUSSSS SSSMULSSS SSSDIVDIVSSS SSSDIVSSS SSSMODSSS SSSANDSSS SSSORSSS SSSXORSSS
%token SSSEQSSS SSSMIDSSS SSSGRESSS SSSGREEQSSS SSSLESSSS SSSLESEQSSS
%token SHORTSSS INTSSS FLOATSSS REALSSS NOTSSS
%token ANONYMOUS_ID PATTERN_ANY

/* value */
%token TRUE FALSE
%token <string> IDENT COMMENT CONST_INT CONST_FLO
%token NULLCOMMENT


%start programY			/* the entry point */
%type <Tree.topLevel> programY
%%

programY:
	TOPLEVEL LPAREN mainY COMMA programBlkY RPAREN EOF
		{TopLevel ($3, $5)}
;

mainY:
	MAIN LPAREN IDENT RPAREN
		{MainBlk $3}
;

programBlkY:
	PROGRAM LPAREN stmtBlkYs RPAREN
		{ProgramBlk $3}
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
		NODE LPAREN nodeKindY COMMA IDENT COMMA IDENT COMMA commentY COMMA paramBlkY COMMA returnBlkY COMMA bodyBlkY RPAREN
			{NodeBlk ($3, $5, $7, $9, $11, $13, $15)}
	|	NODE LPAREN nodeKindY COMMA COMMA IDENT COMMA commentY COMMA paramBlkY COMMA returnBlkY COMMA bodyBlkY RPAREN
			{NodeBlk ($3, "", $6, $8, $10, $12, $14)}
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
	|	LOCALVARS LPAREN RPAREN COMMA				{[]}
	|												{[]}
;

assignStmtYs:
		assignStmtY COMMA assignStmtYs	{$1::$3}
	|	assignStmtY						{[$1]}
	|									{[]}
;

assignStmtY:
	EQUAL LPAREN LVALUE LPAREN lhsLY RPAREN COMMA exprY COMMA guidOpY COMMA guidValY COMMA importedY COMMA importCodeY RPAREN
		{AssignStmt($5, $8, $10, $12, $14, $16)}
;

lhsLY:
		lhsY COMMA lhsLY	{$1}
	|	lhsY				{$1}
;

lhsY:
		ID LPAREN IDENT COMMA kindY COMMA clockY RPAREN	{ID($3, $5, $7)}
	|	ANONYMOUS_ID									{ANONYMOUS_ID}
;

exprY:
		binOpExprY				{$1}
	|	atomExprY				{AtomExpr $1}
	|	unOpExprY				{$1}
	|	ifExprY					{$1}
	|	switchExprY				{$1}
	|	tempoPreExprY			{$1}
	|	tempoArrowExprY			{$1}
	|	tempoFbyExprY			{$1}
	|	fieldAccessExprY		{$1}
	|	constructExprY			{$1}
	|	constructArrExprY		{$1}
	|	mixedConstructorExprY	{$1}
	|	applyExprY				{$1}
	|	arrDimExprY				{$1}
	|	arrIdxExprY				{$1}
	|	arrSliceExprY			{$1}
	|	dynamicProjExprY		{$1}
	|	listExprY				{ListExpr($1)}
;

arrDimExprY:
	ARRAY_DIM LPAREN kindY COMMA clockY COMMA exprY COMMA intValueY RPAREN
		{ArrDimExpr($3,$5,$7,$9)}
;

arrIdxExprY:
	ARRAY_INDEX LPAREN kindY COMMA clockY COMMA exprY COMMA intValueY RPAREN
		{ArrIdxExpr($3,$5,$7,$9)}
;

arrSliceExprY:
	ARRAY_SLICE LPAREN kindY COMMA clockY COMMA exprY COMMA exprY COMMA exprY RPAREN
		{ArrSliceExpr($3,$5,$7,$9,$11)}
;

dynamicProjExprY:
	DYNAMIC_PROJECT LPAREN kindY COMMA clockY COMMA exprY COMMA listExprY COMMA exprY RPAREN
		{DynamicProjExpr($3,$5,$7,$9,$11)}
;

applyExprY:
	APPLY_EXPR LPAREN kindY COMMA clockY COMMA applyBlkY COMMA listExprY RPAREN
		{ApplyExpr($3,$5,$7,$9)}
;

applyBlkY:
		makeStmtY			{$1}
	|	flattenStmtY		{$1}
	|	highOrderStmtY		{$1}
	|	prefixStmtY			{PrefixStmt($1)}
	|	mapwDefaultStmtY	{$1}
	|	mapwiDefaultStmtY	{$1}
	/* ? for zhu: the last parameter of constructor for mapw(i)DefaultStmt should be expr, no list */
	/* ask TA */
	|	foldwIfStmtY		{$1}
	|	foldwiStmtY			{$1}
	/* ? for zhu: It's fold, not flod. */
;

mapwDefaultStmtY:
	MAPW_DEFAULT LPAREN prefixStmtY COMMA intValueY COMMA exprY COMMA exprY RPAREN
		{MapwDefaultStmt($3,$5,$7,$9)}
;

mapwiDefaultStmtY:
	MAPWI_DEFAULT LPAREN prefixStmtY COMMA intValueY COMMA exprY COMMA exprY RPAREN
		{MapwiDefaultStmt($3,$5,$7,$9)}
;

foldwIfStmtY:
	FOLDW_IF LPAREN prefixStmtY COMMA intValueY COMMA exprY RPAREN
		{FoldwIfStmt($3,$5,$7)}
;

foldwiStmtY:
	FOLDWI LPAREN prefixStmtY COMMA intValueY COMMA exprY RPAREN
		{FoldwiStmt($3,$5,$7)}
;

flattenStmtY:
	FLATTEN LPAREN IDENT COMMA kindY RPAREN
		{FlattenStmt($3,$5)}
;

highOrderStmtY:
	HIGH_ORDER LPAREN highOrderOpY COMMA prefixStmtY COMMA intValueY RPAREN
		{HighOrderStmt($3,$5,$7)}
;

highOrderOpY:
		HIGHORDER_MAP		{MAP}
	|	HIGHORDER_FOLD		{FOLD}
	|	HIGHORDER_MAPFOLD	{MAPFOLD}
	|	HIGHORDER_MAPI		{MAPI}
	|	HIGHORDER_FOLDI		{FOLDI}
;

prefixStmtY:
		PREFIX LPAREN IDENT COMMA PARAM_TYPES LPAREN kindsY RPAREN COMMA RET_TYPES LPAREN kindsY RPAREN RPAREN
			{FuncStmt($3,$7,$12)}
	|	PREFIX LPAREN prefixBinOpY RPAREN	{BinOpStmt($3)}
	|	PREFIX LPAREN prefixUnOpY RPAREN	{UnOpStmt($3)}
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
		SSSADDSSS		{PADD}
	|	SSSMINUSSSS		{PSUB}
	|	SSSMULSSS		{PMUL}
	|	SSSDIVDIVSSS 	{PDIVF}
	|	SSSDIVSSS		{PDIV}
	|	SSSMODSSS		{PMOD}
	|	SSSANDSSS		{PAND}
	|	SSSORSSS		{POR}
	|	SSSXORSSS		{PXOR}
	|	SSSEQSSS		{PEQ}
	|	SSSMIDSSS		{PNE}
	|	SSSGRESSS		{PGT}
	|	SSSGREEQSSS		{PGE}
	|	SSSLESSSS		{PLT}
	|	SSSLESEQSSS		{PLE}
;

kindsY:
		kindY COMMA kindsY	{$1::$3}
	|	kindY				{[$1]}
;

makeStmtY:
	MAKE LPAREN IDENT COMMA kindY RPAREN
		{MakeStmt($3,$5)}
;

constructExprY:
	CONSTRUCT LPAREN kindY COMMA clockY COMMA labelExprsY RPAREN
		{ConstructExpr($3,$5,$7)}
;

labelExprsY:
		labelExprY COMMA labelExprsY	{$1::$3}
	|	labelExprY COMMA				{[$1]}
;

labelExprY:
	LABEL_EXPR LPAREN IDENT COMMA exprY RPAREN
		{($3,$5)}
;

constructArrExprY:
	CONSTRUCT_ARRAY LPAREN kindY COMMA clockY COMMA listExprY RPAREN
		{ConstructArrExpr($3,$5,$7)}
;

mixedConstructorExprY:
	MIXED_CONSTRUCTOR LPAREN kindY COMMA clockY COMMA exprY COMMA labelIdxListY COMMA exprY RPAREN
		{MixedConstructorExpr($3,$5,$7,$9,$11)}
;

labelIdxListY:
	LPAREN labelIdxesY RPAREN
		{$2}
;

labelIdxesY:
		labelIdxY COMMA labelIdxesY {$1::$3}
	|	labelIdxY					{[$1]}
;

labelIdxY:
		STRUCT_ITERM LPAREN IDENT RPAREN	{Ident $3}
	|	exprY								{Expr $1}
;

fieldAccessExprY:
	FIELD_ACCESS LPAREN kindY COMMA clockY COMMA exprY COMMA IDENT RPAREN
		{FieldAccessExpr($3,$5,$7,$9)}
;

tempoArrowExprY:
	TEMPO_ARROW LPAREN kindY COMMA clockY COMMA exprY COMMA exprY RPAREN
		{TempoArrowExpr($3,$5,$7,$9)}
;

tempoFbyExprY:
	TEMPO_FBY LPAREN kindY COMMA clockY COMMA listExprY COMMA exprY COMMA listExprY RPAREN
		{TempoFbyExpr($3,$5,$7,$9,$11)}
;

listExprY:
	LIST_EXPR LPAREN exprsY RPAREN
		{$3}
;

exprsY:
		exprY COMMA exprsY	{$1::$3}
	|	exprY				{[$1]}
	|						{[]}
;

tempoPreExprY:
	TEMPO_PRE LPAREN kindY COMMA clockY COMMA exprY RPAREN
		{TempoPreExpr($3,$5,$7)}
;

switchExprY:
	SWITCH_EXPR LPAREN kindY COMMA clockY COMMA exprY COMMA caseStmtsY RPAREN
		{SwitchExpr($3,$5,$7,$9)}
;

caseStmtsY:
		caseStmtY COMMA caseStmtsY	{$1::$3}
	|	caseStmtY					{[$1]}
;

caseStmtY:
	CASE LPAREN valueY COMMA exprY RPAREN
		{($3,$5)}
;

valueY:
		ID LPAREN IDENT COMMA kindY RPAREN	{VIdent($3, $5)}
	|	BOOL LPAREN boolY RPAREN			{VBool($3)}
	|	CHAR LPAREN CONST_INT RPAREN		{VChar($3)}
	|	SHORT LPAREN CONST_INT RPAREN		{VShort($3)}
	|	USHORT LPAREN CONST_INT RPAREN		{VUShort($3)}
	|	INT LPAREN CONST_INT RPAREN			{VInt($3)}
	|	UINT LPAREN CONST_INT RPAREN		{VUInt($3)}
	|	FLOAT LPAREN CONST_FLO RPAREN		{VFloat($3)}
	|	REAL LPAREN CONST_FLO RPAREN		{VReal($3)}
	|	constructValueY						{$1}
	|	constructArrValueY					{$1}
	|	PATTERN_ANY							{VPatternAny}
;

constructValueY:
	CONSTRUCT LPAREN fieldValuesY RPAREN
		{VConstructor($3)}
;

constructArrValueY:
	CONSTRUCT_ARRAY LPAREN valuesY RPAREN
		{VArray($3)}
;
/* ? for zhu: VArray should be 'value list' */

fieldValuesY:
		fieldValueY COMMA fieldValuesY	{$1::$3}
	|	fieldValueY						{[$1]}
;

fieldValueY:
	LABEL_CONST LPAREN IDENT COMMA valueY RPAREN
		{($3,$5)}
;

valuesY:
		valueY COMMA valuesY	{$1::$3}
	|	valueY				{[$1]}
;

ifExprY:
	IF_EXPR LPAREN kindY COMMA clockY COMMA exprY COMMA exprY COMMA exprY RPAREN
		{IfExpr($3,$5,$7,$9,$11)}
;

unOpExprY:
	unopY LPAREN kindY COMMA clockY COMMA exprY RPAREN
		{UnOpExpr($1,$3,$5,$7)}
;

unopY:
		UNOP_SHORTCAST	{SHORT}
	|	UNOP_INTCAST	{INT}
	|	UNOP_FLOATCAST	{FLOAT}
	|	UNOP_REALCAST	{REAL}
	|	UNOP_NOT		{NOT}
	|	UNOP_POS		{POS}
	|	UNOP_NEG		{NEG}
;

binOpExprY:
		binOpY LPAREN kindY COMMA clockY COMMA exprY COMMA exprY RPAREN
			{BinOpExpr($1, $3, $5, $7, $9)}
;

binOpY:
		BINOP_ADD		{ADD}
	|	BINOP_SUBTRACT	{SUB}
	|	BINOP_MULTIPLY	{MUL}
	|	BINOP_DIVIDE	{DIVF}
	|	BINOP_DIV		{DIV}
	|	BINOP_MOD		{MOD}
	|	BINOP_AND		{AND}
	|	BINOP_OR		{OR}
	|	BINOP_XOR		{XOR}
	|	BINOP_GT		{GT}
	|	BINOP_LT		{LT}
	|	BINOP_GE		{GE}
	|	BINOP_LE		{LE}
	|	BINOP_EQ		{EQ}
	|	BINOP_NEQ		{NE}
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
	|	CHAR LPAREN CONST_INT RPAREN					{EChar($3)}
	|	BOOL LPAREN boolY RPAREN					{EBool($3)}
;

boolY:
		FALSE	{"FALSE"}
	|	TRUE	{"TRUE"}
;

guidOpY:
		IDENT 	{GUIDOp $1}
	|	NOCALL	{NOCALL}
;

guidValY:
		IDENT 	{GUIDVal $1}
	|	NOGUID	{NOGUID}
;

importedY:
		NOIMPORT	{NOIMPORT}
	|	IMPORTED	{IMPORTED}
;

importCodeY:
	CONST_INT	{ImportCode $1}
;

clockY:
		LPAREN clockLY RPAREN	{$2}
	|	LPAREN RPAREN		{NOCLOCK}
;

clockLY:
		clockY COMMA clockLY	{$1}
	|	clockY					{$1}
;

declStmtYs:
		declStmtY COMMA declStmtYs	{$1::$3}
	|	declStmtY					{[$1]}
	|								{[]}
;

declStmtY:
	VAR_DECLS LPAREN VARS LPAREN identsY RPAREN COMMA kindY COMMA commentY RPAREN
		{DeclStmt($5, $8, $10)}
;

kindY:
		INT						{Int}
	|	UINT					{UInt}
	|	SHORT					{Short}
	|	USHORT					{UShort}
	|	REAL					{Real}
	|	FLOAT					{Float}
	|	BOOL					{Bool}
	|	CHAR					{Char}
	|	LPAREN kindLY RPAREN	{$2}
	|	constructY				{Construct($1)}
	|	constructEnumY			{$1}
	|	arrayY					{$1}
	|	typenameY				{$1}
;

kindLY:
		kindY COMMA kindLY	{$1}
	|	kindY				{$1}
;

constructY:
	CONSTRUCT LPAREN constructFieldsY RPAREN
		{$3}
;

constructFieldsY:
		constructFieldY COMMA constructFieldsY	{$1::$3}
	|	constructFieldY							{[$1]}
;

constructFieldY:
	FIELD LPAREN IDENT COMMA kindY RPAREN
		{($3,$5)}
;

constructEnumY:
	CONSTRUCT_ENUM LPAREN identsY RPAREN
		{Enum($3)}
;

identsY:
		IDENT COMMA identsY	{$1::$3}
	|	IDENT				{[$1]}
;

arrayY:
	ARRAY LPAREN kindY COMMA intValueY RPAREN
		{Array($3,$5)}
;

intValueY:
	INT LPAREN CONST_INT RPAREN
		{$3}
;

typenameY:
	TYPENAME LPAREN IDENT RPAREN
		{TypeName($3)}
;

commentY:
		COMMENT						{Comment $1}
	|	LPAREN commentY RPAREN	{$2}
	|	NULLCOMMENT					{NULL_COMMENT}
;

constBlkY:
	CONST_BLOCK LPAREN constStmtsY RPAREN
		{ConstBlk($3)}
;

constStmtsY:
		constStmtY COMMA constStmtsY	{$1::$3}
	|	constStmtY						{[$1]}
;

constStmtY:
	CONST LPAREN IDENT COMMA kindY COMMA valueY COMMA commentY RPAREN
		{ConstStmt($3,$5,$7,$9)}
;

typeBlkY:
	TYPE_BLOCK LPAREN typeStmtsY RPAREN
		{TypeBlk($3)}
;

typeStmtsY:
		typeStmtY COMMA typeStmtsY	{$1::$3}
	|	typeStmtY					{[$1]}
;

typeStmtY:
	TYPE LPAREN IDENT COMMA kindY COMMA commentY RPAREN
		{TypeStmt($3,$5,$7)}
;
