(* Definition for the lustre AST. *)

type ident = string

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
    | Ident of ident
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
    | AtomType of atomType
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
