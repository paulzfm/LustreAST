(* Definition for the lustre AST. *)

(* Hao: I don't think we can save int or float now. Maybe they should be string. *)

type ident = string
type comment = string option
type clock = string option

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
    | Ident of ident
    | Bool of bool
    | Short of int
    | UShort of int
    | Int of int
    | UInt of int
    | Float of float
    | Real of float
    | Char of int
    | Constructor of (ident * value) list
    | Array of value

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
(* Hao: Q1: How to use input value?
        Q2: How to use constant? 
        Guess1: 'value' is a kind of 'expr'.
        Guess2: 'Ident' in 'value' is a 'lhs', which is more complex. Or 'lhs' or something similar is a kind of 'expr'.
                'ConstructExpr' has a similar expression, but I can't find the keyword 'construct' in ast file.
*)

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

and mapwDefaultStmt = prefixStmt * int * expr * expr

type nodeKind = Node | Function
type guid = string
type lhs = (ident * kind * clock) option
type guidOp = ident option
type guidVal = guid option
type imported = bool
type importCode = int

type declStmt = ident * kind * comment
type assignStmt = lhs * expr * guidOp * guidVal * imported * importCode

type paramBlk = declStmt list
type returnBlk = declStmt list
type bodyBlk = declStmt list * assignStmt list

type typeStmt = ident * kind * comment
type constStmt = ident * kind * value * comment
type nodeStmt = nodeKind * guid * ident * comment * paramBlk * returnBlk * bodyBlk

type stmtBlk =
    | TypeBlk of typeStmt list
    | ConstBlk of constStmt list
    | NodeBlk of nodeStmt list

type mainBlk = ident
type programBlk = stmtBlk list
type tree = mainBlk * programBlk
;;

let () =
    Printf.printf "Test ok!\n"
;;
