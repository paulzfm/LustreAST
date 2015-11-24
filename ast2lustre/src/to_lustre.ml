(* File to_lustre.ml *)

open Tree

let indent depth str = Printf.sprintf "%s%s\n" (String.make (depth * 4) ' ') str

let f =
    let commentToLustre = function
        | Comment ident -> ident
        | NULL_COMMENT -> ""
    in let clockToLustre = function
        | Clock ident -> ident
        | NULL_COMMENT -> ""
    in let rec kindToLustre = function
        | Bool -> "bool"
        | Short -> "short"
        | UShort -> "ushort"
        | Int -> "int"
        | UInt -> "uint"
        | Float -> "float"
        | Real -> "real"
        | Char -> "char"
        | Enum idents -> Printf.sprintf "enum { %s }" (String.concat ", " idents)
        | Construct cons -> "cons"
        | Array (kind, size) -> Printf.sprintf "%s[%s]" (kindToLustre kind) size
        | TypeName ident -> ident
    in let rec valueToLustre = function
        | VIdent ident -> ident
        | VBool ident -> ident
        | VShort ident -> ident
        | VUShort ident -> ident
        | VInt ident -> ident
        | VUInt ident -> ident
        | VFloat ident -> ident
        | VReal ident -> ident
        | VChar ident -> ident
        | VConstructor vs -> ""
        | VArray value -> valueToLustre value
    in let unOpToLustre = function
        | SHORT -> "short"
        | INT -> "int"
        | FLOAT -> "float"
        | REAL -> "real"
        | NOT -> "not"
        | POS -> "+"
        | NEG -> "-"
    in let binOpToLustre = function
        | ADD -> "+"
        | SUB -> "-"
        | MUL -> "*"
        | DIVF -> "/"
        | DIV -> "div"
        | MOD -> "mod"
        | AND -> "and"
        | OR -> "or"
        | XOR -> "xor"
        | GT -> ">"
        | LT -> "<"
        | GE -> ">="
        | LE -> "<="
        | EQ -> "="
        | NE -> "!="
    in let prefixUnOpToLustre = function
        | PSHORT -> "short$"
        | PINT -> "int$"
        | PFLOAT -> "float$"
        | PREAL -> "real$"
        | PNOT -> "not$"
        | PPOS -> "+$"
        | PNEG -> "-$"
    in let prefixBinOpToLustre = function
        | PADD -> "$+$"
        | PSUB -> "$-$"
        | PMUL -> "$*$"
        | PDIVF -> "$/$"
        | PDIV -> "$div$"
        | PMOD -> "$mod$"
        | PAND -> "$and$"
        | POR -> "$or$"
        | PXOR -> "$xor$"
        | PGT -> "$>$"
        | PLT -> "$<$"
        | PGE -> "$>=$"
        | PLE -> "$<=$"
        | PEQ -> "$=$"
        | PNE -> "$<>$"
    in let highOrderOpToLustre = function
        | MAP -> "map"
        | FOLD -> "fold"
        | MAPFOLD -> "mapfold"
        | MAPI -> "mapi"
        | FOLDI -> "foldi"
    in let prefixStmtToLustre id stmt = match stmt with
        | _ -> ""
    in let atomExprToLustre = function
        | EID (ident, kind, clock) -> ident
        | EIdent ident -> ident
        | EBool ident -> ident
        | EChar ident -> ident
        | EShort ident -> ident
        | EUShort ident -> ident
        | EInt ident -> ident
        | EUInt ident -> ident
        | EFloat ident -> ident
        | EReal ident -> ident
    in let rec exprToLustre = function
        | AtomExpr expr -> atomExprToLustre expr
        | BinOpExpr (op, kind, clock, exprL, exprR) -> Printf.sprintf "%s %s %s" (exprToLustre exprL) (binOpToLustre op) (exprToLustre exprR)
        | UnOpExpr (op, kind, clock, expr) -> Printf.sprintf "%s %s" (unOpToLustre op) (exprToLustre expr)
        | IfExpr (kind, clock, exprC, exprT, exprF) -> Printf.sprintf "if %s then %s else %s" (exprToLustre exprC) (exprToLustre exprT) (exprToLustre exprF)
        | SwitchExpr (_, _, expr, cases) -> Printf.sprintf "case %s of %s" (exprToLustre expr) (String.concat "\n" (List.map (Printf.sprinf "| %s: %s") (List.map fst cases) (List.map snd cases)))
        | TempoPreExpr (_, _, expr) -> Printf.sprintf "pre %s" (exprToLustre expr)
        | TempoArrowExpr (exprL, exprR) -> Printf.sprintf "%s -> %s" exprL exprR
        | TempoFbyExpr (_, _, exprsL, expr, exprsR) -> Printf.sprintf "fby(%s; %s; %s)" (exprToLustre (List.hd exprsL)) (exprToLustre expr) (exprToLustre (List.hd exprsR))
        | FieldAccessExpr (_, _, expr, ident) -> Printf.sprintf "%s.%s" (exprToLustre expr) ident
        | ConstructExpr (ident, cons, _) ->
        | ConstructArrExpr (kind, _, exprs) ->
        | MixedConstructorExpr of kind * clock * expr * labelIdx list * expr
        | ArrDimExpr of kind * clock * expr * int
        | ArrIdxExpr of (_, _, expr, idx) -> Printf.sprintf "%s[%s]" (exprToLustre expr) (exprToLustre idx)
        | ArrSliceExpr of kind * clock * expr * expr * expr
        | ApplyExpr of kind * clock * applyBlk * expr list
        | DynamicProjExpr of kind * clock * expr * expr list * expr
    in let lhsToLustre = function
        | ID (ident, _, _) -> ident
    in let declStmtToLustre = function
        DeclStmt (ident, kind, comment) -> Printf.sprintf "%s: %s" ident (kindToLustre kind)
    in let assignStmtToLustre stmt depth = match stmt with
        AssignStmt (lhs, expr, _, _, _, _) -> indent depth (Printf.sprintf "%s = %s" (lhsToLustre lhs) (exprToLustre expr))
    in let paramBlkToLustre = function
        ParamBlk (decls) -> Printf.sprintf "(%s)" (String.concat ", " (List.map declStmtToLustre decls))
    in let returnBlkToLustre = function
        ReturnBlk (decls) -> Printf.sprintf "returns(%s)" (String.concat ", " (List.map declStmtToLustre decls))
    in let bodyBlkToLustre depth blk = match blk with
        BodyBlk (decls, eqs) -> ""
    in let typeStmtToLustre depth stmt = match stmt with
        TypeStmt (ident, kind, comment) -> indent depth (Printf.sprintf "%s = %s;" ident (kindToLustre kind))
    in let constStmtToLustre depth stmt = match stmt with ConstStmt (ident, kind, val, comment) -> indent depth (Printf.sprintf "%s: %s = %s;" ident (kindToLustre kind) (valueToLustre val))
    in let stmtBlkToLustre depth blk = match blk with
        | TypeBlk blk -> String.concat "\n" [indent depth "type"; List.map (typeStmtToLustre (depth + 1)) blk]
        | ConstBlk blk -> String.concat "\n" [indent depth "const"; List.map (constStmtToLustre (depth + 1)) blk]
        | NodeBlk (kind, _, ident, comment, paramBlk, returnBlk, bodyBlk) -> indent depth (Printf.sprintf "function %s%s\nreturns%s" ident (paramBlkToLustre paramBlk) (returnBlkToLustre returnBlk) (bodyBlkToLustre (depth + 1) bodyBlk))

    "function"
;;
