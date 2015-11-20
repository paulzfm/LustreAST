(* Definition for the lustre AST. *)

(* module Tree = struct *)

type ident = string

type comment =
    | Comment of ident
    | NULL_COMMENT

type clock =
    | Clock of ident
    | NOCLOCK

type integer = string

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
    | Array of kind * integer
    | TypeName of ident

type construct = (ident * kind) list

type value =
    | VIdent of ident
    | VBool of ident
    | VShort of ident
    | VUShort of ident
    | VInt of ident
    | VUInt of ident
    | VFloat of ident
    | VReal of ident
    | VChar of ident
    | VConstructor of (ident * value) list
    | VArray of value

type unOp = SHORT | INT | FLOAT | REAL | NOT | POS | NEG
type binOp = ADD | SUB | MUL | DIVF | DIV | MOD | AND | OR | XOR | GT | LT | GE | LE | EQ | NE
type prefixUnOp = PSHORT | PINT | PFLOAT | PREAL | PNOT | PPOS | PNEG
type prefixBinOp = PADD | PSUB | PMUL | PDIVF | PDIV | PMOD | PAND | POR | PXOR | PGT | PLT | PGE | PLE | PEQ | PNE
type highOrderOp = MAP | FOLD | MAPFOLD | MAPI | FOLDI

type prefixStmt =
    | FuncStmt of ident * kind list * kind list
    | UnOpStmt of prefixUnOp
    | BinOpStmt of prefixBinOp

type atomExpr =
    | EID of ident * kind * clock
    | EIdent of ident
    | EBool of ident
    | EChar of ident
    | EShort of ident
    | EUShort of ident
    | EInt of ident
    | EUInt of ident
    | EFloat of ident
    | EReal of ident

type expr =
    | AtomExpr of atomExpr
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
type guid = string
type lhs = ID of (ident * kind * clock)
type guidOp =
    | GUIDOp of ident
    | NOCALL
type guidVal =
    | GUIDVal of guid
    | NOGUID
type imported = NOIMPORT | IMPORTED
type importCode = ImportCode of ident

type declStmt = DeclStmt of ident * kind * comment
type assignStmt = AssignStmt of lhs * expr * guidOp * guidVal * imported * importCode

type paramBlk = ParamBlk of declStmt list
type returnBlk = ReturnBlk of declStmt list
type bodyBlk = BodyBlk of declStmt list * assignStmt list

type typeStmt = TypeStmt of ident * kind * comment
type constStmt = ConstStmt of ident * kind * value * comment

type stmtBlk =
    | TypeBlk of typeStmt list
    | ConstBlk of constStmt list
    | NodeBlk of nodeKind * guid * ident * comment * paramBlk * returnBlk * bodyBlk

type mainBlk = MainBlk of ident
type programBlk = ProgramBlk of stmtBlk list
type topLevel = TopLevel of mainBlk * programBlk


(* to lustre *)
(* let nodeStmtToLustre = function
    NodeStmt (k, g, i, c, p, r, b) ->

let stmtBlkToLustre = function
    | TypeBlk _ -> []
    | ConstBlk _ -> []
    | NodeBlk [] -> []
    | NodeBlk (stmt :: stmts) ->

let programBlkToLustre = function
    program -> String.concat "," (map stmtBlkToLustre )

let toLustre = function
    TopLevel (m, p) -> programBlkToLustre p *)

(* end;; *)

let sampleTree =
    TopLevel (
        MainBlk "fun1",
        ProgramBlk [NodeBlk (
            Node,
            "",
            "fun1",
            NULL_COMMENT,
            ParamBlk [
                DeclStmt ("var1", Int, NULL_COMMENT);
                DeclStmt ("var2", UInt, NULL_COMMENT)
            ],
            ReturnBlk [
                DeclStmt ("y1", Int, NULL_COMMENT);
                DeclStmt ("y2", UInt, NULL_COMMENT)
            ],
            BodyBlk (
                [],
                [
                    AssignStmt (
                        ID ("y1", Int, NOCLOCK),
                        BinOpExpr (ADD, Int, NOCLOCK, AtomExpr (EID ("var1", Int, NOCLOCK)), AtomExpr (EInt "1")),
                        NOCALL,
                        NOGUID,
                        NOIMPORT,
                        ImportCode "0"
                    );
                ]
            )
        )]
    )
;;
