(* File main.ml *)

open Tree

(* to Lustre *)

let indent depth str = Printf.sprintf "%s%s\n" (String.make (depth * 4) ' ') str

let commentToLustre = function
    | Comment ident -> ident
    | NULL_COMMENT -> ""

let clockToLustre = function
    | Clock ident -> ident
    | NOCLOCK -> ""

let rec kindToLustre = function
    | Bool -> "bool"
    | Short -> "short"
    | UShort -> "ushort"
    | Int -> "int"
    | UInt -> "uint"
    | Float -> "float"
    | Real -> "real"
    | Char -> "char"
    | Enum idents -> Printf.sprintf "enum { %s }" (String.concat ", " idents)
    | Construct cons -> String.concat ", " (List.map (fun (i, t) -> Printf.sprintf "%s: %s" i (kindToLustre t)) cons)
    | Array (kind, size) -> Printf.sprintf "%s ^ %s" (kindToLustre kind) size
    | TypeName ident -> ident

let rec valueToLustre = function
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
    | VArray vals -> Printf.sprintf "[%s]" (String.concat ", " (List.map valueToLustre vals))

let unOpToLustre = function
    | SHORT -> "short"
    | INT -> "int"
    | FLOAT -> "float"
    | REAL -> "real"
    | NOT -> "not"
    | POS -> "+"
    | NEG -> "-"

let binOpToLustre = function
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

let prefixUnOpToLustre = function
    | PSHORT -> "short$"
    | PINT -> "int$"
    | PFLOAT -> "float$"
    | PREAL -> "real$"
    | PNOT -> "not$"
    | PPOS -> "+$"
    | PNEG -> "-$"

let prefixBinOpToLustre = function
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

let highOrderOpToLustre = function
    | MAP -> "map"
    | FOLD -> "fold"
    | MAPFOLD -> "mapfold"
    | MAPI -> "mapi"
    | FOLDI -> "foldi"

let prefixStmtToLustre = function
    | FuncStmt (ident, _, _) -> ident
    | UnOpStmt op -> prefixUnOpToLustre op
    | BinOpStmt op -> prefixBinOpToLustre op

let atomExprToLustre = function
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

let rec exprToLustre = function
    | AtomExpr expr -> atomExprToLustre expr
    | BinOpExpr (op, kind, clock, exprL, exprR) -> Printf.sprintf "%s %s %s" (exprToLustre exprL) (binOpToLustre op) (exprToLustre exprR)
    | UnOpExpr (op, kind, clock, expr) -> Printf.sprintf "%s %s" (unOpToLustre op) (exprToLustre expr)
    | IfExpr (_, _, exprC, exprT, exprF) -> Printf.sprintf "if %s then %s else %s" (exprToLustre exprC) (exprToLustre exprT) (exprToLustre exprF)
    | SwitchExpr (_, _, expr, cases) -> Printf.sprintf "case %s of" (exprToLustre expr)
    | TempoPreExpr (_, _, expr) -> Printf.sprintf "pre %s" (exprToLustre expr)
    | TempoArrowExpr (_, _, exprL, exprR) -> Printf.sprintf "%s -> %s" (exprToLustre exprL) (exprToLustre exprR)
    | TempoFbyExpr (_, _, exprsL, expr, exprsR) -> Printf.sprintf "fby(%s; %s; %s)" (exprToLustre (List.hd exprsL)) (exprToLustre expr) (exprToLustre (List.hd exprsR))
    | FieldAccessExpr (_, _, expr, ident) -> Printf.sprintf "%s.%s" (exprToLustre expr) ident
    | ConstructExpr (ident, _, _) -> ident
    | ConstructArrExpr (_, _, exprs) -> Printf.sprintf "[%s]" (String.concat ", " (List.map exprToLustre exprs))
    | MixedConstructorExpr (_, _, expr1, labels, expr2) -> Printf.sprintf "%s with %s = %s" (exprToLustre expr1) (String.concat "" (List.map labelIdxToLustre labels)) (exprToLustre expr2)
    | ArrDimExpr (_, _, expr, integer) -> Printf.sprintf "%s ^ %s" (exprToLustre expr) integer
    | ArrIdxExpr (_, _, expr, idx) -> Printf.sprintf "%s[%s]" (exprToLustre expr) idx
    | ArrSliceExpr _ -> ""
    | ApplyExpr (_, _, blk, exprs) -> Printf.sprintf "(%s)(%s)" (applyBlkToLustre blk) (String.concat ", " (List.map exprToLustre exprs))
    | DynamicProjExpr (_, _, expr1, exprs, expr2) -> Printf.sprintf "%s.%s default %s" (exprToLustre expr1) (String.concat "" (List.map (Printf.sprintf "[%s]") (List.map exprToLustre exprs))) (exprToLustre expr2)
    | ListExpr (exprs) -> String.concat ", " (List.map exprToLustre exprs)
and applyBlkToLustre = function
    | MakeStmt (ident, _) -> Printf.sprintf "make %s" ident
    | FlattenStmt (ident, _) -> Printf.sprintf "flatten %s" ident
    | HighOrderStmt (op, stmt, integer) -> Printf.sprintf "%s %s<<%s>>" (highOrderOpToLustre op) (prefixStmtToLustre stmt) integer
    | PrefixStmt stmt -> prefixStmtToLustre stmt
    | MapwDefaultStmt (stmt, integer, expr, exprs) -> Printf.sprintf "mapw %s<<%s>> if %s default (%s)" (prefixStmtToLustre stmt) integer (exprToLustre expr) (exprToLustre exprs)
    | MapwiDefaultStmt (stmt, integer, expr, exprs) -> Printf.sprintf "mapwi %s<<%s>> if %s default (%s)" (prefixStmtToLustre stmt) integer (exprToLustre expr) (exprToLustre exprs)
    | FoldwIfStmt (stmt, integer, expr) -> Printf.sprintf "foldw %s<<%s>> if %s" (prefixStmtToLustre stmt) integer (exprToLustre expr)
    | FoldwiStmt (stmt, integer, expr) -> Printf.sprintf "foldwi %s<<%s>> if %s" (prefixStmtToLustre stmt) integer (exprToLustre expr)
and labelIdxToLustre = function
    | Ident ident -> Printf.sprintf ".%s" ident
    | Expr expr -> Printf.sprintf "[%s]" (exprToLustre expr)

let lhsToLustre = function
    | ID (ident, _, _) -> ident
    | ANONYMOUS_ID -> "_"

let declStmtToLustre = function
    DeclStmt (ident, kind, comment) -> Printf.sprintf "%s: %s" ident (kindToLustre kind)

let assignStmtToLustre depth stmt = match stmt with
    AssignStmt (lhs, expr, _, _, _, _) -> indent depth (Printf.sprintf "%s = %s;" (lhsToLustre lhs) (exprToLustre expr))

let paramBlkToLustre = function
    ParamBlk (decls) -> String.concat "; " (List.map declStmtToLustre decls)

let returnBlkToLustre = function
    ReturnBlk (decls) -> String.concat "; " (List.map declStmtToLustre decls)

let bodyBlkToLustre depth blk = match blk with
    BodyBlk (decls, eqs) -> String.concat "" [
        indent depth "let";
        String.concat "" (List.map (assignStmtToLustre (depth + 1)) eqs);
        indent depth "tel";
    ]

let typeStmtToLustre depth stmt = match stmt with
    TypeStmt (ident, kind, comment) -> indent depth (Printf.sprintf "%s = %s;" ident (kindToLustre kind))

let constStmtToLustre depth stmt = match stmt with
    ConstStmt (ident, kind, value, comment) -> indent depth (Printf.sprintf "%s: %s = %s;" ident (kindToLustre kind) (valueToLustre value))

let stmtBlkToLustre depth blk = match blk with
    | TypeBlk blk -> String.concat "" [
        indent depth "type";
        String.concat "" (List.map (typeStmtToLustre (depth + 1)) blk);
        "\n"
      ]
    | ConstBlk stmt -> String.concat "" [
        indent depth "const";
        constStmtToLustre (depth + 1) stmt;
        "\n"
      ]
    | NodeBlk (kind, _, ident, comment, paramBlk, returnBlk, bodyBlk) -> String.concat "" [
        indent depth (Printf.sprintf "function %s(%s)" ident (paramBlkToLustre paramBlk));
        indent depth (Printf.sprintf "returns(%s)" (returnBlkToLustre returnBlk));
        bodyBlkToLustre (depth + 1) bodyBlk;
      ]

let programBlkToLustre depth blks = match blks with
    ProgramBlk (blks) -> String.concat "" (List.map (stmtBlkToLustre 0) blks)

let toLustre = function
    TopLevel (_, blk) -> programBlkToLustre 0 blk
;;

let _ =
	try
		let lexbuf = Lexing.from_channel stdin in
		let result = Parser.programY Lexer.token lexbuf in
            print_endline (toLustre result);
			flush stdout;
			exit 0
	with Parsing.Parse_error ->
		exit 2
;;
