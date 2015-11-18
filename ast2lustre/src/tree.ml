(* Definition for the lustre AST. *)

type ident = Ident of string
type comment = Comment of string option
type clock = Clock of string option

type kind =
    | Bool
    | Short
    | UShort
    | Int
    | UInt
    | Float
    | Real
    | Char
    | Enum of ident list
    | Construct of (ident * kind) list
    | Array of kind * int
    | TypeName of ident

type construct = (ident * kind) list

type value =
    | VIdent of string
    | VBool of string
    | VShort of string
    | VUShort of string
    | VInt of string
    | VUInt of string
    | VFloat of string
    | VReal of string
    | VChar of string
    | VConstructor of (ident * value) list
    | VArray of value

type unOp = SHORT | INT | FLOAT | REAL | NOT | POS | NEG
type binOp = ADD | SUB | MUL | DIVF | DIV | MOD | AND | OR | XOR | GT | LT | GE | LE | EQ | NE
type prefixUnOp = PSHORT | PINT | PFLOAT | PREAL | PNOT | PPOS | PNEG
type prefixBinOp = PADD | PSUB | PMUL | PDIVF | PDIV | PMOD | PAND | POR | PXOR | PGT | PLT | PGE | PLE | PEQ | PNE
type highOrderOp = MAP | FLOD | MAPFLOD | MAPI | FLODI

type prefixStmt =
    | FuncStmt of ident * kind list * kind list
    | UnOpStmt of prefixUnOp
    | BinOpStmt of prefixBinOp

type expr =
    | BinOpExpr of binOp * kind * clock * expr * expr
    | UnOpExpr of unOp * kind * clock * expr
    | IfExpr of kind * clock * expr * expr * expr
    | SwitchExpr of kind * clock * (value * expr) list
    | TempoPreExpr of kind * clock * expr
    | TempoArrowExpr of kind * clock * expr * expr
    | TempoFbyExpr of kind * clock * expr list * expr * expr list
    | FieldAccessExpr of kind * clock * expr * ident
    | ConstructExpr of ident * construct * clock
    | ConstructArrExpr of kind * clock * expr list
    | MixedConstructorExpr of kind * clock * expr * labelIdx list * expr
    | ArrDimExpr of kind * clock * expr * int
    | ArrIdxExpr of kind * clock * expr * int
    | ArrSliceExpr of kind * clock * expr * expr * expr
    | ApplyExpr of kind * clock * applyBlk * expr list
    | DynamicProjExpr of kind * clock * expr * expr list * expr

and labelIdx =
    | Ident of ident
    | Expr of expr

and applyBlk =
    | MakeStmt of ident * kind
    | FlattenStmt of ident * kind
    | HighOrderStmt of highOrderOp * prefixStmt * int
    | MapStmt of prefixStmt * mapwDefaultStmt
    | MapwiDefaultStmt of prefixStmt * int * expr * expr
    | FlodwIfStmt of prefixStmt * int * expr
    | FlodwiStmt of prefixStmt * int * expr

and mapwDefaultStmt = MapwDefaultStmt of prefixStmt * int * expr * expr

type nodeKind = Node | Function
type guid = GUID of string
type lhs = LHS of (ident * kind * clock) option
type guidOp = GUIDOp of ident option
type guidVal = GUIDVal of guid option
type imported = NOIMPORT | IMPORTED
type importCode = ImportCode of string

type declStmt = DeclStmt of ident * kind * comment
type assignStmt = AssignStmt of lhs * expr * guidOp * guidVal * imported * importCode

type paramBlk = ParamBlk of declStmt list
type returnBlk = ReturnBlk of declStmt list
type bodyBlk = BodyBlk of declStmt list * assignStmt list

type typeStmt = TypeStmt of ident * kind * comment
type constStmt = ConstStmt of ident * kind * value * comment
type nodeStmt = NodeStmt of nodeKind * guid * ident * comment * paramBlk * returnBlk * bodyBlk

type stmtBlk =
    | TypeBlk of typeStmt list
    | ConstBlk of constStmt list
    | NodeBlk of nodeStmt list

type mainBlk = MainBlk of ident
type programBlk = ProgramBlk of stmtBlk list
type topLevel = TopLevel of mainBlk * programBlk
;;

let () =
    Printf.printf "Test ok!\n"
;;
