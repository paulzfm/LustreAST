(* File main.ml *)

open Tree

let indent depth str = Printf.sprintf "%s%s" (String.make (depth * 4) ' ') str

let rec makeList times elem =
    if times = 1 then [elem] else elem :: (makeList (times - 1) elem)

exception Error of string

(* symbol table *)

module SymbolTable = struct
    type symbolType =
        | VarSym of kind
        | FuncSym of kind list * kind list
    exception SymbolError of string

    let symbolTable : (string, symbolType) Hashtbl.t array = (Array.make 0 (Hashtbl.create 20))

    let table = ref symbolTable

    let length () =
        Array.length !table

    let enter () =
        let newTable : (string, symbolType) Hashtbl.t = Hashtbl.create 20 in
        table := Array.append !table [|newTable|]

    let exit () =
        table := Array.sub !table 0 (length () - 1)

    let rec insertStruct s ident = match s with
        | VarSym kind -> (match kind with
            | Struct fields -> List.iter (fun f -> match f with         Field (is, k) ->
                List.iter (fun i -> Hashtbl.add !table.(length () - 1) (ident ^ "." ^ i) (VarSym k)) is
            ) fields
            | IDENT name -> insertStruct (search name) ident
            | _ -> ()
        )
        | _ -> ()

    and insert ident value =
        (* Printf.printf "insert: %s\n" ident; *)
        Hashtbl.add !table.(length () - 1) ident value;
        insertStruct value ident

    and recSearch ident i = if i < 0 then
        raise (SymbolError (Printf.sprintf "symbol '%s' not found" ident))
    else
        match Hashtbl.find_all !table.(i) ident with
        | [] -> recSearch ident (i - 1)
        | [value] -> value
        | _ -> raise (SymbolError (Printf.sprintf "more than one symbol '%s' found" ident))

    and search ident = recSearch ident (length () - 1)
end

(* evaluate expr *)

type value =
    | VBool of bool
    | VInt of int
    | VFloat of float
    | VString of string

exception EvalError of string

let rec eval = function
    | AtomExpr (EIdent ident) -> VString ident
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
    (* | _ -> VInt 0 *)
    | _ -> raise (EvalError "complex expr not supported")

(*
let evalToAtomExpr kind expr = match eval expr with
    | VBool value -> (match kind with
        | AtomType Bool -> EBool (string_of_bool value)
        | IDENT ident -> (match SymbolTable.search ident with
            SymbolTable.TypeSym kind -> (match kind with
                | AtomType Bool -> EBool (string_of_bool value)
                | _ -> raise (EvalError "evaluated type 'bool' is incompatible with declared type")
            )
            | _ -> raise (EvalError "evaluated type 'bool' is incompatible with declared type")
        )
        | _ -> raise (EvalError "evaluated type 'bool' is incompatible with declared type")
    )
    | VInt value -> (match kind with
        | AtomType Short -> EShort (string_of_int value)
        | AtomType UShort -> EUShort (string_of_int value)
        | AtomType Int -> EInt (string_of_int value)
        | AtomType UInt -> EUInt (string_of_int value)
        | AtomType Char -> EChar (string_of_int value)
        | IDENT ident -> (match SymbolTable.search ident with
            SymbolTable.TypeSym kind -> (match kind with
                | AtomType Short -> EShort (string_of_int value)
                | AtomType UShort -> EUShort (string_of_int value)
                | AtomType Int -> EInt (string_of_int value)
                | AtomType UInt -> EUInt (string_of_int value)
                | AtomType Char -> EChar (string_of_int value)
                | _ -> raise (EvalError "evaluated type 'int' is incompatible with declared type")
            )
            | _ -> raise (EvalError "evaluated type 'int' is incompatible with declared type")
        )
        | _ -> raise (EvalError "evaluated type 'int' is incompatible with declared type")
    )
    | VFloat value -> (match kind with
        | AtomType Float -> EFloat (string_of_float value)
        | AtomType Real -> EReal (string_of_float value)
        | IDENT ident -> (match SymbolTable.search ident with
            SymbolTable.TypeSym kind -> (match kind with
                | AtomType Float -> EFloat (string_of_float value)
                | AtomType Real -> EReal (string_of_float value)
                | _ -> raise (EvalError "evaluated type 'float' is incompatible with declared type")
            )
            | _ -> raise (EvalError "evaluated type 'float' is incompatible with declared type")
        )
        | _ -> raise (EvalError "evaluated type 'float' is incompatible with declared type")
    )
    | VString value -> EIdent value *)


(* to ast with types *)

type expected =
    | ExpIdent of string
    | ExpKind of tKind
    | NoExp

let rec programToAST = function
    Program nodes -> TTopLevel (
        TMainBlk (
            List.hd (List.concat (List.map (fun n -> match n with
                | FuncBlk (_, _, ident, _, _, _) -> [ident]
                | _ -> []
            ) nodes))
        ),
        TProgramBlk (List.map nodeBlkToAST nodes)
    )

and nodeBlkToAST = function
    | TypeBlk stmts -> TTypeBlk (List.map typeStmtToAST stmts)
    | ConstBlk stmts -> TConstBlk (List.map constStmtToAST stmts)
    | FuncBlk (func, _, ident, par, ret, body) -> TNodeBlk (func, ident, NULL_COMMENT, paramBlkToAST par, returnBlkToAST ret, bodyBlkToAST body)

and typeStmtToAST = function
    TypeStmt (_, ident, kind) -> TTypeStmt (ident, kindToAST kind, NULL_COMMENT)

and constStmtToAST = function
    ConstStmt (_, ident, kind, expr) -> TConstStmt (ident, kindToAST kind, exprToValue (kindToAST kind) expr, NULL_COMMENT)

and paramBlkToAST = function
    ParamBlk fields -> TParamBlk (List.map fieldToAST fields)

and returnBlkToAST = function
    ReturnBlk fields -> TReturnBlk (List.map fieldToAST fields)

and bodyBlkToAST = function
    | BodyBlk (var, eqs) -> TBodyBlk ((match var with
            | VarList fields -> List.map fieldToAST fields
            | NOVARBLK -> []
        ), List.map eqStmtToAST eqs)
    | NOBODYBLK -> TBodyBlk ([], [])

and fieldToAST = function
    Field (idents, kind) -> TDeclStmt (idents, kindToAST kind, NULL_COMMENT)

and eqStmtToAST = function
    EqStmt (lhss, expr) -> let guess = match List.hd lhss with
        | ID ident -> ExpIdent ident
        | ANNOYMITY -> NoExp
    in TAssignStmt (List.map lhsToAST lhss, exprToAST guess expr)

and lhsToAST = function
    | ID ident -> TID (ident, getKind ident, NOCLOCK)
    | ANNOYMITY -> TANONYMOUS_ID

and kindToAST = function
    | AtomType Bool -> TBool
    | AtomType Short -> TShort
    | AtomType UShort -> TUShort
    | AtomType Int -> TInt
    | AtomType UInt -> TUInt
    | AtomType Float -> TFloat
    | AtomType Real -> TReal
    | AtomType Char -> TChar
    | Struct fields -> TConstruct (List.concat (List.map (fun f -> match f with Field (is, k) -> (List.map (fun i -> (i, kindToAST k)) is)) fields))
    | Array (kind, expr) -> TArray (kindToAST kind, exprToInteger expr)
    | IDENT ident -> TTypeName ident
    | EnumType idents -> TEnum idents

and exprToAST expected e =
    let kind = match expected with
        | ExpKind k -> k
        | ExpIdent ident -> getKind ident
        | NoExp -> TBool
    in match e with
        | AtomExpr expr -> TAtomExpr (atomExprToAST expr)
        | UnOpExpr (op, expr) -> TUnOpExpr (op, kind, NOCLOCK, exprToAST (match op with
                | AtomTypeOp _ -> NoExp
                | NOT -> ExpKind TBool
                | POS | NEG -> expected
            ) expr)
        | BinOpExpr (op, exprL, exprR) -> let guess = (match op with
            | AND | OR | XOR -> ExpKind TBool
            | _ -> NoExp
        ) in TBinOpExpr (op, kind, NOCLOCK, exprToAST guess exprL, exprToAST guess exprR)
        | IfExpr (exprC, exprT, exprF) -> TIfExpr (kind, NOCLOCK, exprToAST (ExpKind TBool) exprC, exprToAST expected exprT, exprToAST expected exprF)
        | CaseExpr (sel, cases) -> TSwitchExpr (kind, NOCLOCK, exprToAST NoExp sel, List.map (fun case -> (match case with CaseItem (p, e) -> (match p with
            | PIdent ident -> TVIdent (ident, getKind ident)
            | PBool ident -> TVBool ident
            | PChar ident -> TVChar ident
            | PShort ident -> TVShort ident
            | PUShort ident -> TVUShort ident
            | PInt ident -> TVInt ident
            | PUInt ident -> TVUInt ident
            | PFloat ident -> TVFloat ident
            | PReal ident -> TVReal ident
            | DefaultPattern -> TVPatternAny
        ), exprToAST expected e)) cases)
        | PreExpr expr -> TTempoPreExpr ([kind], [NOCLOCK], exprToAST expected expr)
        | ArrowExpr (exprL, exprR) -> TTempoArrowExpr ([kind], [NOCLOCK], exprToAST expected exprL, exprToAST expected exprR)
        | FbyExpr (exprs1, value, exprs2) -> TTempoFbyExpr ([kind], [NOCLOCK], List.map (exprToAST expected) exprs1, TAtomExpr (TEInt value), List.map (exprToAST expected) exprs2)
        | FieldExpr (expr, ident) -> TFieldAccessExpr (kind, NOCLOCK, exprToAST NoExp expr, ident)
        | ArrNameConstructExpr items -> TConstructExpr (kind, NOCLOCK, List.map (fun x -> match x with NameArrItem (i, e) -> (i, exprToAST (match expected with
            | ExpIdent sym -> ExpKind (getFieldKind sym i)
            | _ -> NoExp
        ) e)) items)
        | ArrConstructExpr exprs -> let guess = match getOriginalKind kind with
            | TArray (k, _) -> ExpKind k
            | _ -> NoExp
        in TConstructArrExpr (kind, NOCLOCK, List.map (exprToAST guess) exprs)
        | WithExpr (ident, items, expr) -> let guess = match getOriginalKind kind with
            | TArray (k, _) -> ExpKind k
            | _ -> NoExp
        in TMixedConstructorExpr (kind, NOCLOCK, TAtomExpr (TEID (ident, getKind ident, NOCLOCK)), List.map (fun i -> match i with
            | FieldItem ident -> TIdent ident
            | AccessItem expr -> TExpr (exprToAST NoExp expr)
        ) items, exprToAST guess expr)
        | ArrAccessExpr (expr, exprV) -> TArrIdxExpr (kind,  NOCLOCK, exprToAST NoExp expr, exprToInteger exprV)
        | ArrInitExpr (expr, exprV) -> TArrDimExpr (kind,  NOCLOCK, exprToAST NoExp expr, exprToInteger exprV)
        | DynamicProjectExpr (expr1, exprs, expr2) -> TDynamicProjExpr (kind, NOCLOCK, exprToAST NoExp expr1, List.map (exprToAST NoExp) exprs, exprToAST expected expr2)
        | PrefixExpr (op, exprs) -> let blk = prefixOpToAST1 op
        in TApplyExpr ([kind], [NOCLOCK], blk, List.map (exprToAST NoExp) exprs)
        | HighOrderExpr (hop, op, value, exprs) -> TApplyExpr ([kind], [NOCLOCK], THighOrderStmt (hop, prefixOpToAST op, value), List.map (exprToAST NoExp) exprs)
        | MapwExpr (op, value, expr1, expr2, exprs) -> TApplyExpr ([kind], [NOCLOCK], TMapwDefaultStmt (prefixOpToAST op, value, exprToAST NoExp expr1, exprToAST NoExp expr2), List.map (exprToAST NoExp) exprs)
        | MapwiExpr (op, value, expr1, expr2, exprs) -> TApplyExpr ([kind], [NOCLOCK], TMapwiDefaultStmt (prefixOpToAST op, value, exprToAST NoExp expr1, exprToAST NoExp expr2), List.map (exprToAST NoExp) exprs)
        | FoldwiExpr (op, value, expr, exprs) -> TApplyExpr ([kind], [NOCLOCK], TFoldwiStmt (prefixOpToAST op, value, exprToAST NoExp expr), List.map (exprToAST NoExp) exprs)
        | FoldwExpr (op, value, expr, exprs) -> TApplyExpr ([kind], [NOCLOCK], TFoldwIfStmt (prefixOpToAST op, value, exprToAST NoExp expr), List.map (exprToAST NoExp) exprs)
        | ExprList exprs -> TListExpr (List.map (exprToAST expected) exprs)
        | _ -> raise (Error "cannot parse this expr")

and prefixOpToAST = function
    | Ident ident -> TFuncStmt (ident, getFuncParams ident, getFuncRets ident)
    | UnOp op ->  TUnOpStmt op
    | BinOp op -> TBinOpStmt op
    | _ -> raise (Error "type error")

and prefixOpToAST1 = function
    | Make ident -> TMakeStmt (ident, getKind ident)
    | Flatten ident -> TFlattenStmt (ident, getKind ident)
    | Ident ident -> TPrefixStmt (TFuncStmt (ident, getFuncParams ident, getFuncRets ident))
    | UnOp op -> TPrefixStmt (TUnOpStmt op)
    | BinOp op -> TPrefixStmt (TBinOpStmt op)

and atomExprToAST = function
    | EIdent ident -> TEID (ident, getKind ident, NOCLOCK) (*TODO: TEID and TEIdent*)
    | EBool ident -> TEBool ident
    | EChar ident -> TEChar ident
    | EShort ident -> TEShort ident
    | EUShort ident -> TEUShort ident
    | EInt ident -> TEInt ident
    | EUInt ident -> TEUInt ident
    | EFloat ident -> TEFloat ident
    | EReal ident -> TEReal ident

and exprToValue kind expr = let str =
    match eval expr with
        | VBool value -> string_of_bool value
        | VInt value -> string_of_int value
        | VFloat value -> string_of_float value
        | VString value -> value
    in match kind with
        | TBool -> TVBool str
        | TShort -> TVShort str
        | TUShort -> TVUShort str
        | TInt -> TVInt str
        | TUInt -> TVUInt str
        | TFloat -> TVFloat str
        | TReal -> TVReal str
        | TChar -> TVChar str
        | _ -> raise (Error "in exprToValue: unexpected type")

and exprToInteger = function
    | AtomExpr (EInt ident) -> ident
    | _ -> raise (Error "expr is not an integer")

and getKind ident = match SymbolTable.search ident with
    | SymbolTable.VarSym kind -> kindToAST kind
    | _ -> raise (Error "symbol doesn't name a variable")

and getFuncParams ident = match SymbolTable.search ident with
    | SymbolTable.FuncSym (ks, _) -> List.map kindToAST ks
    | _ -> raise (Error "symbol doesn't name a function")

and getFuncRets ident = match SymbolTable.search ident with
    | SymbolTable.FuncSym (_, ks) -> List.map kindToAST ks
    | _ -> raise (Error "symbol doesn't name a function")

and getFieldKind ident field = getKind (ident ^ "." ^ field)

and getOriginalKind kind = match kind with
    | TTypeName ident -> getOriginalKind (getKind ident)
    | _ -> kind
;;

(* output *)
let rec output = function
    TTopLevel (main, prog) -> String.concat "\n" [
        indent 0 "TopLevel(";
        mainBlkOut 1 main;
        programBlkOut 1 prog;
        indent 0 ")";
    ]

and programBlkOut depth blk = match blk with
    TProgramBlk blks -> String.concat "\n" [
        indent depth "program(";
        String.concat ",\n" (List.map (stmtBlkOut (depth + 1)) blks);
        indent depth ")"
    ]

and mainBlkOut depth blk = match blk with
    TMainBlk ident -> indent depth (Printf.sprintf "main(%s)" ident);

and stmtBlkOut depth blk = match blk with
    | TTypeBlk stmts -> String.concat "\n" [
        indent depth "type_block(";
        String.concat ",\n" (List.map (typeStmtOut (depth + 1)) stmts);
        indent depth ")"
    ]
    | TConstBlk stmts -> String.concat "\n" [
        indent depth "const_block(";
        String.concat ",\n" (List.map (constStmtOut (depth + 1)) stmts);
        indent depth ")"
    ]
    | TNodeBlk (func, ident, com, paramBlk, returnBlk, bodyBlk) -> String.concat "\n" [
        indent depth "node(";
        String.concat ",\n" [
            indent (depth + 1) (funcTypeOut func);
            indent (depth + 1) "";
            indent (depth + 1) ident;
            indent (depth + 1) (commentOut com);
            paramBlkOut (depth + 1) paramBlk;
            returnBlkOut (depth + 1) returnBlk;
            bodyBlkOut (depth + 1) bodyBlk;
        ];
        indent depth ")"
    ]

and typeStmtOut depth stmt = match stmt with
    TTypeStmt (ident, kind, com) -> indent depth (Printf.sprintf "type(%s, %s, %s)" ident (kindOut kind) (commentOut com))

and constStmtOut depth stmt = match stmt with
    TConstStmt (ident, kind, value, com) -> indent depth (Printf.sprintf "const(%s, %s, %s, %s)" ident (kindOut kind) (valueOut value) (commentOut com))

and funcTypeOut = function
    | Node -> "node"
    | Function -> "function"

and clockOut = function
    | Clock ident -> Printf.sprintf "(%s)" ident
    | NOCLOCK -> "()"

and commentOut = function
    | Comment com -> com
    | NULL_COMMENT -> "NullComment"

and paramBlkOut depth stmt = match stmt with
    TParamBlk stmts -> String.concat "\n" [
        indent depth "params(";
        String.concat ",\n" (List.map (declStmtOut (depth + 1)) stmts);
        indent depth ")"
    ]

and returnBlkOut depth stmt = match stmt with
    TReturnBlk stmts -> String.concat "\n" [
        indent depth "returns(";
        String.concat ",\n" (List.map (declStmtOut (depth + 1)) stmts);
        indent depth ")"
    ]

and declStmtOut depth stmt = match stmt with
    TDeclStmt (idents, kind, com) -> indent depth (Printf.sprintf "var_decls(vars(%s), %s, %s)" (String.concat ", " idents) (kindOut kind) (commentOut com))

and bodyBlkOut depth stmt = match stmt with
    | TBodyBlk ([], eqs) -> String.concat "\n" [
        indent depth "body(";
        String.concat ",\n" (List.map (assignStmtOut (depth + 1)) eqs);
        indent depth ")"
    ]
    | TBodyBlk (vars, eqs) -> String.concat "\n" [
        indent depth "body(";
        indent (depth + 1) "localvars(";
        String.concat ", " (List.map (declStmtOut (depth + 2)) vars);
        indent (depth + 1) ")";
        String.concat ",\n" (List.map (assignStmtOut (depth + 1)) eqs);
        indent depth ")"
    ]

and assignStmtOut depth stmt = match stmt with
    TAssignStmt (lhss, expr) -> indent depth (Printf.sprintf "=(lvalue(%s), %s, NULL)" (String.concat ", " (List.map lhsOut lhss)) (exprOut expr))

and lhsOut = function
    | TID (ident, kind, clk) -> Printf.sprintf "ID(%s, %s, %s)" ident (kindOut kind) (clockOut clk)
    | TANONYMOUS_ID -> "anonymous_id"

and kindOut = function
    | TBool -> "bool"
    | TShort -> "short"
    | TUShort -> "ushort"
    | TInt -> "int"
    | TUInt -> "uint"
    | TFloat -> "float"
    | TReal -> "real"
    | TChar -> "char"
    | TEnum idents -> Printf.sprintf "enum(%s)" (String.concat ", " idents)
    | TConstruct cons -> Printf.sprintf "construct(%s)" (String.concat ", " (List.map (fun (i, k) -> Printf.sprintf "field(%s, %s)" i (kindOut k)) cons))
    | TArray (kind, len) -> Printf.sprintf "array(%s, %s)" (kindOut kind) len
    | TTypeName ident -> Printf.sprintf "typename(%s)" ident

and valueOut = function
    | TVIdent (ident, kind) -> ident
    | TVBool ident -> ident
    | TVShort ident -> ident
    | TVUShort ident -> ident
    | TVInt ident -> ident
    | TVUInt ident -> ident
    | TVFloat ident -> ident
    | TVReal ident -> ident
    | TVChar ident -> ident
    | TVConstructor cons -> Printf.sprintf "construct(%s)" (String.concat ", " (List.map (fun (i, k) -> Printf.sprintf "field(%s, %s)" i (valueOut k)) cons))
    | TVArray vals -> Printf.sprintf "construct_array(%s)" (String.concat ", " (List.map valueOut vals))
    | TVPatternAny -> "pattern_any"

and prefixStmtOut = function
    | TFuncStmt (ident, params, rets) -> Printf.sprintf "prefix(%s, param_types(%s), ret_types(%s))" ident (String.concat ", " (List.map kindOut params)) (String.concat ", " (List.map kindOut rets))
    | TUnOpStmt op -> Printf.sprintf "prefix(%s)" (prefixUnOpOut op)
    | TBinOpStmt op -> Printf.sprintf "prefix(%s)" (prefixBinOpOut op)

and atomExprOut = function
    | TEID (ident, kind, clk) -> Printf.sprintf "ID(%s, %s, %s)" ident (kindOut kind) (clockOut clk)
    | TEIdent ident -> ident
    | TEBool ident -> ident
    | TEChar ident -> ident
    | TEShort ident -> ident
    | TEUShort ident -> ident
    | TEInt ident -> ident
    | TEUInt ident -> ident
    | TEFloat ident -> ident
    | TEReal ident -> ident

and exprOut = function
    | TAtomExpr expr -> atomExprOut expr
    | TBinOpExpr (op, kind, clk, exprL, exprR) -> Printf.sprintf "%s(%s, %s, %s, %s)" (binOpOut op) (kindOut kind) (clockOut clk) (exprOut exprL) (exprOut exprR)
    | TUnOpExpr (op, kind, clk, expr) -> Printf.sprintf "%s(%s, %s, %s)" (unOpOut op) (kindOut kind) (clockOut clk) (exprOut expr)
    | TIfExpr (kind, clk, exprC, exprT, exprF) -> Printf.sprintf "if_expr(%s, %s, %s, %s, %s)" (kindOut kind) (clockOut clk) (exprOut exprC) (exprOut exprT) (exprOut exprF)
    | TSwitchExpr (kind, clk, expr, cases) -> Printf.sprintf "switch_expr(%s, %s, %s, %s)" (kindOut kind) (clockOut clk) (exprOut expr) (String.concat ", " (List.map (fun (v, e) -> Printf.sprintf "case(%s, %s)" (valueOut v) (exprOut e)) cases))
    | TTempoPreExpr (kinds, clks, expr) -> Printf.sprintf "tempo_pre((%s), (%s), %s)" (String.concat ", " (List.map kindOut kinds)) (String.concat ", " (List.map clockOut clks)) (exprOut expr)
    | TTempoArrowExpr (kinds, clks, exprL, exprR) -> Printf.sprintf "tempo_arrow((%s), (%s), %s, %s)" (String.concat ", " (List.map kindOut kinds)) (String.concat ", " (List.map clockOut clks))  (exprOut exprL) (exprOut exprR)
    | TTempoFbyExpr (kinds, clks, exprs1, expr, exprs2) -> Printf.sprintf "tempo_fby((%s), (%s), %s, %s, %s)" (String.concat ", " (List.map kindOut kinds)) (String.concat ", " (List.map clockOut clks)) (exprOut (TListExpr exprs1)) (exprOut expr) (exprOut (TListExpr exprs2))
    | TFieldAccessExpr (kind, clk, expr, ident) -> Printf.sprintf "field_access(%s, %s, %s, %s)" (kindOut kind) (clockOut clk) (exprOut expr) ident
    | TConstructExpr (kind, clk, cons) -> Printf.sprintf "construct(%s, %s, %s)" (kindOut kind) (clockOut clk) (String.concat ", " (List.map (fun (i, e) -> Printf.sprintf "label_expr(%s, %s)" i (exprOut e)) cons))
    | TConstructArrExpr (kind, clk, exprs) -> Printf.sprintf "construct_array(%s, %s, %s)" (kindOut kind) (clockOut clk) (exprOut (TListExpr exprs))
    | TMixedConstructorExpr (kind, clk, expr1, labels, expr2) -> Printf.sprintf "mixed_constructor(%s, %s, %s, (%s), %s)" (kindOut kind) (clockOut clk) (exprOut expr1) (String.concat ", " (List.map labelIdxOut labels)) (exprOut expr2)
    | TArrDimExpr (kind, clk, expr, len) -> Printf.sprintf "array_dim(%s, %s, %s, %s)" (kindOut kind) (clockOut clk) (exprOut expr) len
    | TArrIdxExpr (kind, clk, expr, idx) -> Printf.sprintf "array_index(%s, %s, %s, %s)" (kindOut kind) (clockOut clk) (exprOut expr) idx
    | TArrSliceExpr (kind, clk, expr1, expr2, expr3) -> Printf.sprintf "array_slice(%s, %s, %s, %s, %s)" (kindOut kind) (clockOut clk) (exprOut expr1) (exprOut expr2) (exprOut expr3)
    | TApplyExpr (kinds, clks, blk, exprs) -> Printf.sprintf "apply_expr((%s), (%s), %s, %s)" (String.concat ", " (List.map kindOut kinds)) (String.concat ", " (List.map clockOut clks))
    (applyBlkOut blk) (exprOut (TListExpr exprs))
    | TDynamicProjExpr (kind, clk, expr1, exprs, expr2) -> Printf.sprintf "dynamic_project(%s, %s, %s, (%s), %s)" (kindOut kind) (clockOut clk) (exprOut expr1) (String.concat ", " (List.map exprOut exprs)) (exprOut expr2)
    | TListExpr exprs -> Printf.sprintf "list_expr(%s)" (String.concat ", " (List.map exprOut exprs))

and labelIdxOut = function
    | TIdent ident -> Printf.sprintf "struct_item(%s)" ident
    | TExpr expr -> exprOut expr

and applyBlkOut = function
    | TMakeStmt (ident, kind) -> Printf.sprintf "make(%s, %s)" ident (kindOut kind)
    | TFlattenStmt (ident, kind) -> Printf.sprintf "flatten(%s, %s)" ident (kindOut kind)
    | THighOrderStmt (op, stmt, value) -> Printf.sprintf "high_order(%s, %s, %s)" (highOrderOpOut op) (prefixStmtOut stmt) value
    | TPrefixStmt stmt -> prefixStmtOut stmt
    | TMapwDefaultStmt (stmt, value, expr1, expr2) -> Printf.sprintf "mapw_default(%s, %s, %s, %s)" (prefixStmtOut stmt) value (exprOut expr1) (exprOut expr2)
    | TMapwiDefaultStmt (stmt, value, expr1, expr2) -> Printf.sprintf "mapwi_default(%s, %s, %s, %s)" (prefixStmtOut stmt) value (exprOut expr1) (exprOut expr2)
    | TFoldwIfStmt (stmt, value, expr) -> Printf.sprintf "foldw_if(%s, %s, %s)" (prefixStmtOut stmt) value (exprOut expr)
    | TFoldwiStmt (stmt, value, expr) -> Printf.sprintf "foldwi(%s, %s, %s)" (prefixStmtOut stmt) value (exprOut expr)

and unOpOut = function
    | AtomTypeOp Short -> "unop_shortcast"
    | AtomTypeOp Int -> "unop_intcast"
    | AtomTypeOp Float -> "unop_floatcast"
    | AtomTypeOp Real -> "unop_realcast"
    | NOT -> "unop_not"
    | POS -> "unop_pos"
    | NEG -> "unop_neg"
    | _ -> "!ERROR!"

and binOpOut = function
    | ADD -> "binop_add"
    | SUB -> "binop_subtract"
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

and prefixUnOpOut = function
    | PSHORT -> "short$"
    | PINT -> "int$"
    | PFLOAT -> "float$"
    | PREAL -> "real$"
    | PNOT -> "not$"
    | PPOS -> "+$"
    | PNEG -> "-$"

and prefixBinOpOut = function
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

and highOrderOpOut = function
    | MAP -> "highorder_map"
    | FOLD -> "highorder_fold"
    | MAPFOLD -> "highorder_mapfold"
    | MAPI -> "highorder_mapi"
    | FOLDI -> "highorder_foldi"


(* main *)
let main result =
    output (programToAST result)

let _ =
	try
		let lexbuf = Lexing.from_channel stdin in
		let result = Parser.programY Lexer.token lexbuf in
            print_endline (main result);
			flush stdout;
			exit 0
	with Parsing.Parse_error ->
		exit 2
;;
