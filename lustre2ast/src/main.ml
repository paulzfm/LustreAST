(* File main.ml *)

open Tree

let indent depth str = Printf.sprintf "%s%s\n" (String.make (depth * 4) ' ') str

let evalExpr x = "value"

(* to ast *)

let clockToAST = function
    | Clock ident -> Printf.sprintf "(%s)" ident
    | NOCLOCK -> "()"

let nullComment = "NullComment"

let funcTypeToAST = function
    | Function -> "function"
    | Node -> "node"

let lhsToAST = function
    | ID ident -> Printf.sprintf "ID (%s, %s, %s)" ident "" ""
    | ANNOYMITY -> "anonymous_id"

let atomTypeToAST = function
    | Bool -> "bool"
    | Short -> "short"
    | UShort -> "ushort"
    | Int -> "int"
    | UInt -> "uint"
    | Float -> "float"
    | Real -> "real"
    | Char -> "char"

let unOpToAST = function
    | AtomTypeOp Short -> "unop_shortcast"
    | AtomTypeOp Int -> "unop_intcast"
    | AtomTypeOp Float -> "unop_floatcast"
    | AtomTypeOp Real -> "unop_realcast"
    | NOT -> "unop_not"
    | POS -> "unop_pos"
    | NEG -> "unop_neg"
    | _ -> "ERROR"

let binOpToAST = function
    | ADD -> "binop_add"
    | SUB -> "binop_substract"
    | MUL -> "binop_multiply"
    | DIVF -> "binop_divide"
    | DIV -> "binop_div"
    | MOD -> "binop_mod"
    | AND -> "binop_and"
    | OR -> "binop_or"
    | XOR -> "binop_xor"
    | GT -> "binop_gt"
    | LT -> "binop_lt"
    | GE -> "binop_ge"
    | LE -> "binop_le"
    | EQ -> "binop_eq"
    | NE -> "binop_neq"

let prefixUnOpToAST = function
    | PSHORT -> "short$"
    | PINT -> "int$"
    | PFLOAT -> "float$"
    | PREAL -> "real$"
    | PNOT -> "not$"
    | PPOS -> "+$"
    | PNEG -> "-$"

let prefixBinOpToAST = function
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

let highOrderOpToAST = function
    | MAP -> "highorder_map"
    | FOLD -> "highorder_fold"
    | MAPFOLD -> "highorder_mapfold"
    | MAPI -> "highorder_mapi"
    | FOLDI -> "highorder_foldi"

let prefixOpToAST = function
    | Ident ident -> ident
    | UnOp op -> prefixUnOpToAST op
    | BinOp op -> prefixBinOpToAST op
    | Flatten ident -> Printf.sprintf "flatten(%s, %s)" ident ""
    | Make ident -> Printf.sprintf "make(%s, %s)" ident ""

let atomExprToAST = function
    | EIdent ident -> Printf.sprintf "ID(%s)" ident
    | EBool ident -> Printf.sprintf "BOOL(%s)" ident
    | EChar ident -> Printf.sprintf "CHAR(%s)" ident
    | EShort ident -> Printf.sprintf "SHORT(%s)" ident
    | EUShort ident -> Printf.sprintf "USHORT(%s)" ident
    | EInt ident -> Printf.sprintf "INT(%s)" ident
    | EUInt ident -> Printf.sprintf "UINT(%s)" ident
    | EFloat ident -> Printf.sprintf "FLOAT(%s)" ident
    | EReal ident -> Printf.sprintf "REAL(%s)" ident

let patternToAST = function
    | PIdent ident -> Printf.sprintf "ID(%s)" ident
    | PBool ident -> Printf.sprintf "BOOL(%s)" ident
    | PChar ident -> Printf.sprintf "CHAR(%s)" ident
    | PShort ident -> Printf.sprintf "SHORT(%s)" ident
    | PUShort ident -> Printf.sprintf "USHORT(%s)" ident
    | PInt ident -> Printf.sprintf "INT(%s)" ident
    | PUInt ident -> Printf.sprintf "UINT(%s)" ident
    | PFloat ident -> Printf.sprintf "FLOAT(%s)" ident
    | PReal ident -> Printf.sprintf "REAL(%s)" ident
    | DefaultPattern -> "pattern_any"

let rec kindToAST = function
    | AtomType kind -> atomTypeToAST kind
    | Struct fields -> Printf.sprintf "construct(%s)" (String.concat ", " (List.map fieldToAST fields))
    | Array (kind, expr) -> Printf.sprintf "array(%s, %s)" (kindToAST kind) (exprToAST expr)
    | IDENT name -> Printf.sprintf "typename(%s)" name
    | EnumType idents -> Printf.sprintf "construct_enum(%s)" (String.concat ", " idents)

and fieldToAST = function
    Field (idents, kind) -> Printf.sprintf "field(%s, %s)" (String.concat ", " idents) (kindToAST kind)

and exprToAST e = match e with
    | AtomExpr expr -> atomExprToAST expr
    | UnOpExpr (op, expr) -> Printf.sprintf "%s(%s, %s, %s)" (unOpToAST op) (evalType e) (clockToAST NOCLOCK) (exprToAST expr)
    | BinOpExpr (op, exprL, exprR) -> Printf.sprintf "%s(%s, %s, %s, %s)" (binOpToAST op) (evalType e) (clockToAST NOCLOCK) (exprToAST exprL) (exprToAST exprR)
    | FieldExpr (expr, ident) -> ""
    | StructExpr exprs -> ""
    | DynamicProjectExpr (expr1, exprs, expr2) -> Printf.sprintf "dynamic_project(%s, %s, %s, (%s), %s)" (evalType e) (clockToAST NOCLOCK) (exprToAST expr1) (String.concat ", " (List.map exprToAST exprs)) (exprToAST expr2)
    | ArrAccessExpr (expr, idx) -> Printf.sprintf "array_index(%s, %s, %s, %s)" (evalType e) (clockToAST NOCLOCK) (exprToAST expr) (exprToAST idx)
    | ArrInitExpr (expr, dim) -> Printf.sprintf "array_dim(%s, %s, %s, %s)" (evalType e) (clockToAST NOCLOCK) (exprToAST expr) (exprToAST dim)
    | ArrConstructExpr exprs -> Printf.sprintf "construct_array(%s, %s, list_expr(%s))" (evalType e) (clockToAST NOCLOCK) (String.concat ", " (List.map exprToAST exprs))
    | ArrNameConstructExpr items -> ""
    | PreExpr expr -> Printf.sprintf "tempo_pre(%s, %s, %s)" (evalType e) (clockToAST NOCLOCK) (exprToAST expr)
    | FbyExpr (exprs1, integer, exprs2) -> Printf.sprintf "tempo_fby(%s, %s, list_expr(%s), %s, list_expr(%s))" (evalType e) (clockToAST NOCLOCK) (String.concat ", " (List.map exprToAST exprs1)) integer (String.concat ", " (List.map exprToAST exprs2))
    | ArrowExpr (exprL, exprR) -> Printf.sprintf "tempo_arrow(%s, %s, %s, %s)" (evalType e) (clockToAST NOCLOCK) (exprToAST exprL) (exprToAST exprR)
    | WhenExpr (expr, ident) -> ""
    | IfExpr (cond, exprT, exprF) -> Printf.sprintf "if_expr(%s, %s, %s, %s, %s)" (evalType e) (clockToAST NOCLOCK) (exprToAST cond) (exprToAST exprT) (exprToAST exprF)
    | CaseExpr (expr, cases) -> Printf.sprintf "switch_expr(%s, %s, %s, %s)" (evalType e) (clockToAST NOCLOCK) (exprToAST expr) (String.concat ", " (List.map caseItemToAST cases))
    | WithExpr (ident, items, expr) -> ""
    | ExprList (exprs) -> Printf.sprintf "list_expr(%s)" (String.concat ", " (List.map exprToAST exprs))
    | PrefixExpr (op, exprs) -> Printf.sprintf "prefix(%s)" (prefixOpToAST op)
    | HighOrderExpr (hop, op, value, exprs) -> Printf.sprintf "high_order(%s, %s, %s)" (highOrderOpToAST hop) (exprToAST (PrefixExpr (op, exprs))) value
    | MapwiExpr (op, integer, expr1, expr2, exprs) -> Printf.sprintf "mapw_default(%s, %s, %s, %s,)"
    (prefixOpToAST op) integer (exprToAST expr1) (exprToAST (ExprList exprs))
    | MapwExpr (op, integer, expr1, expr2, exprs) -> Printf.sprintf "mapw_default(%s, %s, %s, %s,)"
    (prefixOpToAST op) integer (exprToAST expr1) (exprToAST (ExprList exprs))
    | FoldwiExpr (op, integer, expr, exprs) -> Printf.sprintf "foldwi(%s, %s, %s)"
    (prefixOpToAST op) integer (exprToAST expr)
    | FoldwExpr (op, integer, expr, exprs) -> Printf.sprintf "foldw_if(%s, %s, %s)"
    (prefixOpToAST op) integer (exprToAST expr)

and evalType x = kindToAST (AtomType Bool)

and caseItemToAST = function
    CaseItem (pattern, expr) -> Printf.sprintf "case(%s, %s)" (patternToAST pattern) (exprToAST expr)

and nameArrItemToAST = function
    NameArrItem (ident, expr) -> Printf.sprintf "label_expr(%s, %s)" ident (exprToAST expr)

and withItemToAST = function
    | FieldItem ident -> ident
    | AccessItem expr -> exprToAST expr

let eqStmtToAST depth stmt = match stmt with
    EqStmt (lhss, expr) -> indent depth (Printf.sprintf "=(lvalue(%s), %s, NOCALL, NOGUID, NOIMPORT, 0)" (String.concat ", " (List.map lhsToAST lhss)) (exprToAST expr))

let varBlkToAST depth stmt = match stmt with
    | VarList fields -> String.concat "\n" [
        indent depth "localvars(";
        String.concat ",\n" (List.map fieldToAST fields);
        indent depth "),"
      ]
    | NOVARBLK -> ""

let paramBlkToAST depth stmt = match stmt with
    ParamBlk fields -> String.concat "\n" [
        indent depth "params(";
        String.concat ",\n" (List.map fieldToAST fields);
        indent depth ")"
    ]

let returnBlkToAST depth stmt = match stmt with
    ReturnBlk fields -> String.concat "\n" [
        indent depth "returns(";
        String.concat ",\n" (List.map fieldToAST fields);
        indent depth ")"
    ]

let bodyBlkToAST depth stmt = match stmt with
    | BodyBlk (varBlk, eqs) -> String.concat "\n" [
        indent depth "body(";
        varBlkToAST (depth + 1) varBlk;
        String.concat ",\n" (List.map (eqStmtToAST (depth + 1)) eqs);
        indent depth ")"
      ]
    | NOBODYBLK -> "No body"

let typeStmtToAST depth stmt = match stmt with
    TypeStmt (_, ident, kind) -> indent depth (Printf.sprintf "type(%s, %s, %s)" ident (kindToAST kind) nullComment)

let constStmtToAST depth stmt = match stmt with
    ConstStmt (_, ident, kind, expr) -> indent depth (Printf.sprintf "const(%s, %s, %s, %s)" ident (kindToAST kind) (evalExpr expr) nullComment)

let nodeBlkToAST depth stmt = match stmt with
    | TypeBlk stmts -> String.concat "\n" [
        indent depth "type_block(";
        String.concat ",\n" (List.map (typeStmtToAST (depth + 1)) stmts);
        indent depth ")"
      ]
    | ConstBlk stmts -> String.concat "\n" [
        indent depth "const_block(";
        String.concat ",\n" (List.map (constStmtToAST (depth + 1)) stmts);
        indent depth ")"
      ]
    | FuncBlk (funcType, _, ident, paramBlk, returnBlk, bodyBlk) -> String.concat "\n" [
        indent depth "node(";
        String.concat ",\n" [
            indent (depth + 1) (funcTypeToAST funcType);
            "";
            indent (depth + 1) ident;
            indent (depth + 1) nullComment;
            paramBlkToAST (depth + 1) paramBlk;
            returnBlkToAST (depth + 1) returnBlk;
            bodyBlkToAST (depth + 1) bodyBlk;
        ];
        indent depth ")"
      ]

let searchMain nodes = match (List.hd (List.filter (
    fun node -> match node with
        | FuncBlk _ -> true
        | _ -> false
    ) nodes)) with
        | FuncBlk (_, _, ident, _, _, _) -> ident
        | _ -> ""

let programToAST depth program = match program with
    Program nodes -> String.concat "\n" [
        indent depth "TopLevel(";
        indent (depth + 1) (Printf.sprintf "main(%s)," (searchMain nodes));
        String.concat ",\n" (List.map (nodeBlkToAST (depth + 1)) nodes);
        indent depth ")";
    ]

let toAST program = programToAST 0 program

let toLustre = function
    Program (_) -> "OK"
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
