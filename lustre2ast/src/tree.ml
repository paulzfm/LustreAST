(* Definition for the lustre AST. *)

type ident = string

type comment =
    | Comment of ident
    | NULL_COMMENT

type clock =
    | Clock of ident
    | NOCLOCK

type integer = string

type modifier =
    | Private
    | Public
    | Protected
    | NOMODIFIER

type funcType =
    | Function
    | Node

type lhs =
    | ID of ident
    | ANNOYMITY

type lhsL = lhs list

type atomType =
    | Bool
    | Short
    | UShort
    | Int
    | UInt
    | Float
    | Real
    | Char

type unOp =
    | AtomTypeOp of atomType
    | NOT
    | POS
    | NEG

type binOp = ADD | SUB | MUL | DIVF | DIV | MOD | AND | OR | XOR | GT | LT | GE | LE | EQ | NE
type prefixUnOp = PSHORT | PINT | PFLOAT | PREAL | PNOT | PPOS | PNEG
type prefixBinOp = PADD | PSUB | PMUL | PDIVF | PDIV | PMOD | PAND | POR | PXOR | PGT | PLT | PGE | PLE | PEQ | PNE
type highOrderOp = MAP | FOLD | MAPFOLD | MAPI | FOLDI

type prefixOp =
    | Ident of ident
    | UnOp of prefixUnOp
    | BinOp of prefixBinOp
    | Flatten of ident
    | Make of ident

type atomExpr =
    | EIdent of ident
    | EBool of ident
    | EChar of ident
    | EShort of ident
    | EUShort of ident
    | EInt of ident
    | EUInt of ident
    | EFloat of ident
    | EReal of ident

type pattern =
    | PIdent of ident
    | PBool of ident
    | PChar of ident
    | PShort of ident
    | PUShort of ident
    | PInt of ident
    | PUInt of ident
    | PFloat of ident
    | PReal of ident
    | DefaultPattern

type kind =
    | AtomType of atomType
    | Struct of field list
    | Array of kind * expr
    | IDENT of ident
    | EnumType of ident list

and field = Field of ident list * kind

and expr =
    | AtomExpr of atomExpr
    | UnOpExpr of unOp * expr
    | BinOpExpr of binOp * expr * expr
    | FieldExpr of expr * ident
    | StructExpr of expr list
    | DynamicProjectExpr of expr * expr list * expr
    | ArrAccessExpr of expr * expr
    | ArrInitExpr of expr * expr
    | ArrConstructExpr of expr list
    | ArrNameConstructExpr of nameArrItem list
    | PreExpr of expr
    | FbyExpr of expr list * integer * expr list
    | ArrowExpr of expr * expr
    | WhenExpr of expr * ident
    | IfExpr of expr * expr * expr
    | CaseExpr of expr * caseItem list
    | WithExpr of ident * withItem list * expr
    | ExprList of expr list
    | PrefixExpr of prefixOp * expr list
    | HighOrderExpr of highOrderOp * prefixOp * integer * expr list
    | MapwiExpr of prefixOp * integer * expr * expr * expr list
    | MapwExpr of prefixOp * integer * expr * expr * expr list
    | FoldwiExpr of prefixOp * integer * expr * expr list
    | FoldwExpr of prefixOp * integer * expr * expr list

and caseItem = CaseItem of pattern * expr

and nameArrItem = NameArrItem of ident * expr

and withItem =
    | FieldItem of ident
    | AccessItem of expr

type eqStmt = EqStmt of lhsL * expr

type varBlk =
    | VarList of field list
    | NOVARBLK

type paramBlk = ParamBlk of field list
type returnBlk = ReturnBlk of field list
type bodyBlk =
    | BodyBlk of varBlk * eqStmt list
    | NOBODYBLK

type typeStmt = TypeStmt of modifier * ident * kind
type constStmt = ConstStmt of modifier * ident * kind * expr

type nodeBlk =
    | TypeBlk of typeStmt list
    | ConstBlk of constStmt list
    | FuncBlk of funcType * modifier * ident * paramBlk * returnBlk * bodyBlk

type program = Program of nodeBlk list
;;

(* with types *)

type tKind =
    | TBool
    | TShort
    | TUShort
    | TInt
    | TUInt
    | TFloat
    | TReal
    | TChar
    | TEnum of ident list
    | TConstruct of (ident * tKind) list
    | TArray of tKind * ident
    | TTypeName of ident

type tValue =
    | TVIdent of ident * tKind
    | TVBool of ident
    | TVShort of ident
    | TVUShort of ident
    | TVInt of ident
    | TVUInt of ident
    | TVFloat of ident
    | TVReal of ident
    | TVChar of ident
    | TVConstructor of (ident * tValue) list
    | TVArray of tValue list
    | TVPatternAny

type tPrefixStmt =
    | TFuncStmt of ident * tKind list * tKind list
    | TUnOpStmt of prefixUnOp
    | TBinOpStmt of prefixBinOp

type tAtomExpr =
    | TEID of ident * tKind * clock
    | TEIdent of ident
    | TEBool of ident
    | TEChar of ident
    | TEShort of ident
    | TEUShort of ident
    | TEInt of ident
    | TEUInt of ident
    | TEFloat of ident
    | TEReal of ident

type tExpr =
    | TAtomExpr of tAtomExpr
    | TBinOpExpr of binOp * tKind * clock * tExpr * tExpr
    | TUnOpExpr of unOp * tKind * clock * tExpr
    | TIfExpr of tKind * clock * tExpr * tExpr * tExpr
    | TSwitchExpr of tKind * clock * tExpr * (tValue * tExpr) list
    | TTempoPreExpr of tKind list * clock list * tExpr
    | TTempoArrowExpr of tKind list * clock list * tExpr * tExpr
    | TTempoFbyExpr of tKind list * clock list * tExpr list * tExpr * tExpr list
    | TFieldAccessExpr of tKind * clock * tExpr * ident
    | TConstructExpr of tKind * clock * (ident * tExpr) list
    | TConstructArrExpr of tKind * clock * tExpr list
    | TMixedConstructorExpr of tKind * clock * tExpr * labelIdx list * tExpr
    | TArrDimExpr of tKind * clock * tExpr * integer
    | TArrIdxExpr of tKind * clock * tExpr * integer
    | TArrSliceExpr of tKind * clock * tExpr * tExpr * tExpr
    | TApplyExpr of tKind list * clock list * applyBlk * tExpr list
    | TDynamicProjExpr of tKind * clock * tExpr * tExpr list * tExpr
    | TListExpr of tExpr list

and labelIdx =
    | TIdent of ident
    | TExpr of tExpr

and applyBlk =
    | TMakeStmt of ident * tKind
    | TFlattenStmt of ident * tKind
    | THighOrderStmt of highOrderOp * tPrefixStmt * integer
    | TPrefixStmt of tPrefixStmt
    | TMapwDefaultStmt of tPrefixStmt * integer * tExpr * tExpr
    | TMapwiDefaultStmt of tPrefixStmt * integer * tExpr * tExpr
    | TFoldwIfStmt of tPrefixStmt * integer * tExpr
    | TFoldwiStmt of tPrefixStmt * integer * tExpr

type tLHS =
    | TID of (ident * tKind * clock)
    | TANONYMOUS_ID

type tGuidOp =
    | TGUIDOp of ident
    | TNOCALL

type tDeclStmt = TDeclStmt of ident list * tKind * comment
type tAssignStmt = TAssignStmt of tLHS list * tExpr * tGuidOp

type tParamBlk = TParamBlk of tDeclStmt list
type tReturnBlk = TReturnBlk of tDeclStmt list
type tBodyBlk = TBodyBlk of tDeclStmt list * tAssignStmt list

type tTypeStmt = TTypeStmt of ident * tKind * comment
type tConstStmt = TConstStmt of ident * tKind * tValue * comment

type tStmtBlk =
    | TTypeBlk of tTypeStmt list
    | TConstBlk of tConstStmt list
    | TNodeBlk of funcType * ident * comment * tParamBlk * tReturnBlk * tBodyBlk

type tMainBlk = TMainBlk of ident
type tProgramBlk = TProgramBlk of tStmtBlk list
type tTopLevel = TTopLevel of tMainBlk * tProgramBlk
;;
