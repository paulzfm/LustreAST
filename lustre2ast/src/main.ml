(* File main.ml *)

open Tree

let indent depth str = Printf.sprintf "%s%s" (String.make (depth * 4) ' ') str

type value =
    | VBool of bool
    | VInt of int
    | VFloat of float

exception EvalError of string

let rec eval = function
    | AtomExpr (EIdent ident) -> VInt 0
    | AtomExpr (EBool ident) -> VBool (bool_of_string ident)
    | AtomExpr (EChar ident) -> VInt (int_of_char (String.get ident 0))
    | AtomExpr (EShort ident) -> VInt (int_of_string ident)
    | AtomExpr (EUShort ident) -> VInt (int_of_string ident)
    | AtomExpr (EInt ident) -> VInt (int_of_string ident)
    | AtomExpr (EUInt ident) -> VInt (int_of_string ident)
    | AtomExpr (EFloat ident) -> VFloat (float_of_string ident)
    | AtomExpr (EReal ident) -> VFloat (float_of_string ident)
    | UnOpExpr (op, expr) -> (match op with
        | NOT -> (match eval expr with
            | VBool value -> VBool (not value)
            | _ -> raise (EvalError "'not' must be applied to bool values")
        )
        | POS -> (match eval expr with
            | VInt value -> VInt value
            | VFloat value -> VFloat value
            | _ -> raise (EvalError "'+' cannot be applied to bool values")
        )
        | NEG -> (match eval expr with
            | VInt value -> VInt (- value)
            | VFloat value -> VFloat (-. value)
            | _ -> raise (EvalError "'-' cannot be applied to bool values")
        )
        | _ -> raise (EvalError "not supported")
    )
    | BinOpExpr (op, exprL, exprR) -> (match op with
        | ADD -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VInt (v1 + v2)
            | (VFloat v1, VFloat v2) -> VFloat (v1 +. v2)
            | (VInt v1, VFloat v2) -> VFloat ((float_of_int v1) +. v2)
            | (VFloat v1, VInt v2) -> VFloat (v1 +. (float_of_int v2))
            | _ -> raise (EvalError "operands for '+' are incompatible")
        )
        | SUB -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VInt (v1 - v2)
            | (VFloat v1, VFloat v2) -> VFloat (v1 -. v2)
            | (VInt v1, VFloat v2) -> VFloat ((float_of_int v1) -. v2)
            | (VFloat v1, VInt v2) -> VFloat (v1 -. (float_of_int v2))
            | _ -> raise (EvalError "operands for '-' are incompatible")
        )
        | MUL -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VInt (v1 * v2)
            | (VFloat v1, VFloat v2) -> VFloat (v1 *. v2)
            | (VInt v1, VFloat v2) -> VFloat ((float_of_int v1) *. v2)
            | (VFloat v1, VInt v2) -> VFloat (v1 *. (float_of_int v2))
            | _ -> raise (EvalError "operands for '*' are incompatible")
        )
        | DIVF -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VFloat ((float_of_int v1) /. (float_of_int v2))
            | (VFloat v1, VFloat v2) -> VFloat (v1 /. v2)
            | (VInt v1, VFloat v2) -> VFloat ((float_of_int v1) /. v2)
            | (VFloat v1, VInt v2) -> VFloat (v1 /. (float_of_int v2))
            | _ -> raise (EvalError "operands for '/' are incompatible")
        )
        | DIV -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VInt (v1 / v2)
            | _ -> raise (EvalError "operands for 'div' are incompatible")
        )
        | MOD -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VInt (v1 mod v2)
            | _ -> raise (EvalError "operands for 'mod' are incompatible")
        )
        | AND -> (match (eval exprL, eval exprR) with
            | (VBool v1, VBool v2) -> VBool (v1 && v2)
            | _ -> raise (EvalError "operands for 'and' are incompatible")
        )
        | OR -> (match (eval exprL, eval exprR) with
            | (VBool v1, VBool v2) -> VBool (v1 || v2)
            | _ -> raise (EvalError "operands for 'or' are incompatible")
        )
        | XOR -> (match (eval exprL, eval exprR) with
            | (VBool v1, VBool v2) -> VBool (v1 <> v2)
            | _ -> raise (EvalError "operands for 'xor' are incompatible")
        )
        | GT -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VBool (v1 > v2)
            | (VFloat v1, VFloat v2) -> VBool (v1 > v2)
            | (VInt v1, VFloat v2) -> VBool ((float_of_int v1) > v2)
            | (VFloat v1, VInt v2) -> VBool (v1 > (float_of_int v2))
            | _ -> raise (EvalError "operands for '>' are incompatible")
        )
        | LT -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VBool (v1 < v2)
            | (VFloat v1, VFloat v2) -> VBool (v1 < v2)
            | (VInt v1, VFloat v2) -> VBool ((float_of_int v1) < v2)
            | (VFloat v1, VInt v2) -> VBool (v1 < (float_of_int v2))
            | _ -> raise (EvalError "operands for '<' are incompatible")
        )
        | GE -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VBool (v1 >= v2)
            | (VFloat v1, VFloat v2) -> VBool (v1 >= v2)
            | (VInt v1, VFloat v2) -> VBool ((float_of_int v1) >= v2)
            | (VFloat v1, VInt v2) -> VBool (v1 >= (float_of_int v2))
            | _ -> raise (EvalError "operands for '>=' are incompatible")
        )
        | LE -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VBool (v1 <= v2)
            | (VFloat v1, VFloat v2) -> VBool (v1 <= v2)
            | (VInt v1, VFloat v2) -> VBool ((float_of_int v1) <= v2)
            | (VFloat v1, VInt v2) -> VBool (v1 <= (float_of_int v2))
            | _ -> raise (EvalError "operands for '<=' are incompatible")
        )
        | EQ -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VBool (v1 == v2)
            | (VBool v1, VBool v2) -> VBool (v1 == v2)
            | _ -> raise (EvalError "operands for '=' are incompatible")
        )
        | NE -> (match (eval exprL, eval exprR) with
            | (VInt v1, VInt v2) -> VBool (v1 != v2)
            | (VBool v1, VBool v2) -> VBool (v1 != v2)
            | _ -> raise (EvalError "operands for '!=' are incompatible")
        )
    )
    | _ -> raise (EvalError "complex expr not supported")

let evalToAtomExpr kind expr = match eval expr with
    | VBool value -> (match kind with
        | AtomType Bool -> EBool (string_of_bool value)
        | _ -> raise (EvalError "evaluated type 'bool' is incompatible with declared type")
    )
    | VInt value -> (match kind with
        | AtomType Short -> EShort (string_of_int value)
        | AtomType UShort -> EUShort (string_of_int value)
        | AtomType Int -> EInt (string_of_int value)
        | AtomType UInt -> EUInt (string_of_int value)
        | AtomType Char -> EChar (string_of_int value)
        | _ -> raise (EvalError "evaluated type 'int' is incompatible with declared type")
    )
    | VFloat value -> (match kind with
        | AtomType Float -> EFloat (string_of_float value)
        | AtomType Real -> EReal (string_of_float value)
        | _ -> raise (EvalError "evaluated type 'float' is incompatible with declared type")
    )

(* to ast *)

let clockToAST = function
    | Clock ident -> Printf.sprintf "(%s)" ident
    | NOCLOCK -> "()"

let nullComment = "NullComment"

let funcTypeToAST = function
    | Function -> "function"
    | Node -> "node"

let lhsToAST = function
    | ID ident -> Printf.sprintf "ID(%s, %s, %s)" ident "" (clockToAST NOCLOCK)
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
    | EIdent ident -> Printf.sprintf "ID(%s, %s, %s)" ident "" (clockToAST NOCLOCK)
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

let evalExpr kind expr = atomExprToAST (evalToAtomExpr kind expr)

let rec kindToAST = function
    | AtomType kind -> atomTypeToAST kind
    | Struct fields -> Printf.sprintf "construct(%s)" (String.concat ", " (List.map fieldToAST fields))
    | Array (kind, expr) -> Printf.sprintf "array(%s, %s)" (kindToAST kind) (evalExpr kind expr)
    | IDENT name -> Printf.sprintf "typename(%s)" name
    | EnumType idents -> Printf.sprintf "construct_enum(%s)" (String.concat ", " idents)

and fieldToAST = function
    Field (idents, kind) -> Printf.sprintf "var_decls(vars(%s), %s, (%s))" (String.concat ", " idents) (kindToAST kind) nullComment

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
    | PrefixExpr (op, exprs) -> Printf.sprintf "apply_expr(%s, %s, prefix(%s), %s" (evalType e) (clockToAST NOCLOCK) (prefixOpToAST op) (exprToAST (ExprList exprs))
    | HighOrderExpr (hop, op, value, exprs) -> Printf.sprintf "apply_expr(%s, %s, high_order(%s, %s, %s), %s)" (evalType e) (clockToAST NOCLOCK) (highOrderOpToAST hop) (exprToAST (PrefixExpr (op, exprs))) value (exprToAST (ExprList exprs))
    | MapwiExpr (op, integer, expr1, expr2, exprs) -> Printf.sprintf "apply_expr(%s, %s, mapwi_default(%s, %s, %s, %s), %s" (evalType e) (clockToAST NOCLOCK) (prefixOpToAST op) integer (exprToAST expr1) (exprToAST expr2) (exprToAST (ExprList exprs))
    | MapwExpr (op, integer, expr1, expr2, exprs) -> Printf.sprintf "apply_expr(%s, %s, mapw_default(%s, %s, %s, %s), %s" (evalType e) (clockToAST NOCLOCK) (prefixOpToAST op) integer (exprToAST expr1) (exprToAST expr2) (exprToAST (ExprList exprs))
    | FoldwiExpr (op, integer, expr, exprs) -> Printf.sprintf "apply_expr(%s, %s, foldwi(%s, %s, %s), %s)" (evalType e) (clockToAST NOCLOCK) (prefixOpToAST op) integer (exprToAST expr) (exprToAST (ExprList exprs))
    | FoldwExpr (op, integer, expr, exprs) -> Printf.sprintf "apply_expr(%s, %s, foldw_if(%s, %s, %s), %s)" (evalType e) (clockToAST NOCLOCK) (prefixOpToAST op) integer (exprToAST expr) (exprToAST (ExprList exprs))

and evalType x = kindToAST (AtomType Bool)

and caseItemToAST = function
    CaseItem (pattern, expr) -> Printf.sprintf "case(%s, %s)" (patternToAST pattern) (exprToAST expr)

and nameArrItemToAST = function
    NameArrItem (ident, expr) -> Printf.sprintf "label_expr(%s, %s)" ident (exprToAST expr)

and withItemToAST = function
    | FieldItem ident -> ident
    | AccessItem expr -> exprToAST expr

let callFuncName = function
    | PrefixExpr (Ident name, _) -> name
    | _ -> "NOCALL"

let eqStmtToAST depth stmt = match stmt with
    EqStmt (lhss, expr) -> indent depth (Printf.sprintf "=(lvalue(%s), %s, %s, NOGUID, NOIMPORT, 0)" (String.concat ", " (List.map lhsToAST lhss)) (exprToAST expr) (callFuncName expr))

let declStmtToAST depth stmt = match stmt with
    Field (idents, kind) -> indent depth (Printf.sprintf "var_decls(vars(%s), %s, (%s))" (String.concat ", " idents) (kindToAST kind) nullComment)

let varBlkToAST depth stmt = match stmt with
    | VarList fields -> String.concat "\n" [
        indent depth "localvars(";
        String.concat ",\n" (List.map (declStmtToAST (depth + 1)) fields);
        indent depth "),"
      ]
    | NOVARBLK -> ""

let paramBlkToAST depth stmt = match stmt with
    ParamBlk fields -> String.concat "\n" [
        indent depth "params(";
        String.concat ",\n" (List.map (declStmtToAST (depth + 1)) fields);
        indent depth ")"
    ]

let returnBlkToAST depth stmt = match stmt with
    ReturnBlk fields -> String.concat "\n" [
        indent depth "returns(";
        String.concat ",\n" (List.map (declStmtToAST (depth + 1)) fields);
        indent depth ")"
    ]

let bodyBlkToAST depth stmt = match stmt with
    | BodyBlk (NOVARBLK, eqs) ->
    String.concat "\n" [
        indent depth "body(";
        String.concat ",\n" (List.map (eqStmtToAST (depth + 1)) eqs);
        indent depth ")"
      ]
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
    ConstStmt (_, ident, kind, expr) -> indent depth (Printf.sprintf "const(%s, %s, %s, %s)" ident (kindToAST kind) (evalExpr kind expr) nullComment)

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
            indent (depth + 1) "";
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
        indent (depth + 1) "program(";
        String.concat ",\n" (List.map (nodeBlkToAST (depth + 2)) nodes);
        indent (depth + 1) ")";
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
            print_endline (toAST result);
			flush stdout;
			exit 0
	with Parsing.Parse_error ->
		exit 2
;;
