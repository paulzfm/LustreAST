(* File main.ml *)

open Tree

exception Error of string

let indent depth str = Printf.sprintf "%s%s" (String.make (depth * 4) ' ') str

let rec makeList times elem =
    if times = 1 then [elem] else elem :: (makeList (times - 1) elem)

let rec findElem x lst =
    match lst with
        | [] -> raise (Error "Not Found")
        | h :: t -> if x = h then 0 else 1 + findElem x t

let rec getElem idx lst = if idx = 0 then List.hd lst else getElem (idx - 1) (List.tl lst)

let rec kindToString = function
    | AtomType Bool -> "bool"
    | AtomType Short -> "short"
    | AtomType UShort -> "ushort"
    | AtomType Int -> "int"
    | AtomType UInt -> "uint"
    | AtomType Float -> "float"
    | AtomType Real -> "real"
    | AtomType Char -> "char"
    | EnumType idents -> Printf.sprintf "enum { %s }" (String.concat ", " idents)
    | Struct cons -> Printf.sprintf "struct { %s }" (String.concat ", " (List.map (fun c -> match c with Field (is, k) -> Printf.sprintf "%s: %s" (String.concat ", " is) (kindToString k)) cons))
    | Array (kind, _) -> Printf.sprintf "[%s]" (kindToString kind)
    | IDENT ident -> Printf.sprintf "ref %s" ident

let rec kindOut1 = function
    | TBool -> "bool"
    | TShort -> "short"
    | TUShort -> "ushort"
    | TInt -> "int"
    | TUInt -> "uint"
    | TFloat -> "float"
    | TReal -> "real"
    | TChar -> "char"
    | TEnum idents -> Printf.sprintf "construct_enum(%s)" (String.concat ", " idents)
    | TConstruct cons -> Printf.sprintf "construct(%s)" (String.concat ", " (List.map (fun (i, k) -> Printf.sprintf "field(%s, %s)" i (kindOut1 k)) cons))
    | TArray (kind, len) -> Printf.sprintf "array(%s, INT(%s))" (kindOut1 kind) len
    | TTypeName ident -> Printf.sprintf "typename(%s)" ident

(* symbol table *)

module SymbolTable = struct
    type symbolType =
        | VarSym of tKind
        | FuncSym of tKind list * tKind list
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

    let rawInsert value ident =
        Hashtbl.replace !table.(length () - 1) ident value

    let insert ident value =
        rawInsert value ident;
        match value with
            | VarSym kind -> (match kind with
                | TEnum idents -> List.iter (rawInsert value) idents
                | _ -> ()
            )
            | _ -> ()

    let rec recSearch ident i = if i < 0 then
        raise (SymbolError (Printf.sprintf "symbol '%s' not found" ident))
    else
        match Hashtbl.find_all !table.(i) ident with
        | [] -> recSearch ident (i - 1)
        | [value] -> value
        | _ -> raise (SymbolError (Printf.sprintf "more than one symbol '%s' found" ident))

    and search ident = recSearch ident (length () - 1)

    let iter func = Hashtbl.iter func !table.(length () - 1)

    let show () =
        Hashtbl.iter (fun k -> fun v ->
            Printf.printf "%s : %s\n" k (match v with
                | VarSym kind -> kindOut1 kind
                | FuncSym (kp, kr) -> String.concat " -> " [
                    String.concat " * " (List.map kindOut1 kp);
                    String.concat " * " (List.map kindOut1 kr)
                ]
            )
        ) !table.(length () - 1)
end

(* constant table *)

module ConstTable = struct
    let constTable : (string, tKind * expr * tValue option) Hashtbl.t = Hashtbl.create 20

    let table = ref constTable

    let insert ident kind expr =
        (* Printf.printf "insert: %s\n" ident; *)
        Hashtbl.replace !table ident (kind, expr, None)

    let update ident value =
        let (kind, expr, str) = Hashtbl.find !table ident in
            Hashtbl.replace !table ident (kind, expr, Some value)

    let search ident =
        match Hashtbl.find_all !table ident with
            | [] -> None
            | [(_, expr, _)] -> Some expr
            | _ -> raise (Error "in const table: multiple keys")

    let get ident =
        let (_, _, str) = Hashtbl.find !table ident in
            str

    let iter func = Hashtbl.iter func !table

    (* let show () =
        Hashtbl.iter (fun k -> fun (_, _, v) ->
            Printf.printf "%s : %s\n" k (match v with
                | Some str -> valueOut str
                | None -> "NULL"
            )
        ) !table *)
end

let rec eval kind e = match e with
    | AtomExpr (EIdent ident) -> (match ConstTable.search ident with
        | None -> TVIdent (ident, kind)
        | Some expr -> eval kind expr
    )
    | AtomExpr (EBool ident) -> TVBool ident
    | AtomExpr (EChar ident) -> TVChar ident
    | AtomExpr (EShort ident) -> TVShort ident
    | AtomExpr (EUShort ident) -> TVUShort ident
    | AtomExpr (EInt ident) -> TVInt ident
    | AtomExpr (EUInt ident) -> TVUInt ident
    | AtomExpr (EFloat ident) -> TVFloat ident
    | AtomExpr (EReal ident) -> TVReal ident
    | UnOpExpr (op, expr) -> (
        let x = eval kind expr
        in (match op with
            | POS -> x
            | NOT -> (match x with
                | TVBool value -> TVBool (string_of_bool (not (bool_of_string value)))
                | _ -> raise (Error "eval: type mismatched for operator 'not'")
            )
            | NEG -> (match x with
                | TVInt a -> TVInt (string_of_int (- (int_of_string a)))
                | TVUInt a -> TVUInt (string_of_int (- (int_of_string a)))
                | TVShort a -> TVShort (string_of_int (- (int_of_string a)))
                | TVUShort a -> TVUShort (string_of_int (- (int_of_string a)))
                | TVFloat a -> TVFloat (string_of_float (-. (float_of_string a)))
                | TVReal a -> TVReal (string_of_float (-. (float_of_string a)))
                | _ -> raise (Error "eval: type mismatched for operator '-'")
            )
            | _ -> raise (Error "not supported")
        )
    )
    | BinOpExpr (op, exprL, exprR) -> (
        let x = eval kind exprL
        in let y = eval kind exprR
        in (match op with
            | ADD | SUB | MUL | DIVF | DIV | MOD ->
                let func = opToFunc op
                in let ffunc = opToFFunc op
                in (match (x, y) with
                    | (TVInt a, TVInt b) -> TVInt (string_of_int (func (int_of_string a) (int_of_string b)))
                    | (TVUInt a, TVUInt b) -> TVUInt (string_of_int (func (int_of_string a) (int_of_string b)))
                    | (TVShort a, TVShort b) -> TVShort (string_of_int (func (int_of_string a) (int_of_string b)))
                    | (TVUShort a, TVUShort b) -> TVUShort (string_of_int (func (int_of_string a) (int_of_string b)))
                    | (TVFloat a, TVFloat b) -> TVFloat (string_of_float (ffunc (float_of_string a) (float_of_string b)))
                    | (TVReal a, TVReal b) -> TVReal (string_of_float (ffunc (float_of_string a) (float_of_string b)))
                    | (_, _) -> raise (Error "eval: type mismatched for numeric operator")
                )
            | GT | LT | GE | LE ->
                let cfunc = opToCmpFunc op
                in let cfuncf = opToCmpFuncF op
                in (match (x, y) with
                    | (TVInt a, TVInt b) -> TVBool (string_of_bool (cfunc (int_of_string a) (int_of_string b)))
                    | (TVUInt a, TVUInt b) -> TVBool (string_of_bool (cfunc (int_of_string a) (int_of_string b)))
                    | (TVShort a, TVShort b) -> TVBool (string_of_bool (cfunc (int_of_string a) (int_of_string b)))
                    | (TVUShort a, TVUShort b) -> TVBool (string_of_bool (cfunc (int_of_string a) (int_of_string b)))
                    | (TVFloat a, TVFloat b) -> TVBool (string_of_bool (cfuncf (float_of_string a) (float_of_string b)))
                    | (TVReal a, TVReal b) -> TVBool (string_of_bool (cfuncf (float_of_string a) (float_of_string b)))
                    | (_, _) -> raise (Error "eval: type mismatched for compare operator")
                )
            | AND | OR | XOR ->
                let bfunc = opToBoolFunc op
                in (match (x, y) with
                    | (TVBool a, TVBool b) -> TVBool (string_of_bool (bfunc (bool_of_string a) (bool_of_string b)))
                    | (_, _) -> raise (Error "eval: type mismatched for boolean operator")
                )
            | EQ | NE ->
                let func = opToFunc op
                in let ffunc = opToFFunc op
                in let bfunc = opToBoolFunc op
                in (match (x, y) with
                    | (TVInt a, TVInt b) -> TVInt (string_of_int (func (int_of_string a) (int_of_string b)))
                    | (TVUInt a, TVUInt b) -> TVUInt (string_of_int (func (int_of_string a) (int_of_string b)))
                    | (TVShort a, TVShort b) -> TVShort (string_of_int (func (int_of_string a) (int_of_string b)))
                    | (TVUShort a, TVUShort b) -> TVUShort (string_of_int (func (int_of_string a) (int_of_string b)))
                    | (TVFloat a, TVFloat b) -> TVFloat (string_of_float (ffunc (float_of_string a) (float_of_string b)))
                    | (TVReal a, TVReal b) -> TVReal (string_of_float (ffunc (float_of_string a) (float_of_string b)))
                    | (TVBool a, TVBool b) -> TVBool (string_of_bool (bfunc (bool_of_string a) (bool_of_string b)))
                    | (_, _) -> raise (Error "eval: type mismatched for equal operator")
                )
            )
        )
    | FieldExpr _ -> raise (Error "eval: 1")
    | StructExpr _ -> raise (Error "eval: 2")
    | ArrConstructExpr exprs -> TVArray (List.map (eval kind) exprs)
    | ArrNameConstructExpr items -> TVConstructor (List.map (fun item -> match item with NameArrItem (i, e) -> (i, eval kind e)) items)
    | ArrInitExpr _ -> raise (Error "eval: 5")
    | ArrAccessExpr _ -> raise (Error "eval: 6")
    | PreExpr _ -> raise (Error "eval: 7")
    | FbyExpr _ -> raise (Error "eval: 8")
    | ArrowExpr _ -> raise (Error "eval: 9")
    | WhenExpr _ -> raise (Error "eval: 10")
    | IfExpr _ -> raise (Error "eval: 11")
    | CaseExpr _ -> raise (Error "eval: 12")
    | WithExpr _ -> raise (Error "eval: 13")
    | ExprList exprs -> eval kind (List.hd exprs)
    | PrefixExpr _ -> raise (Error "eval: 15")
    | HighOrderExpr _ -> raise (Error "eval: 16")
    | MapwiExpr _ -> raise (Error "eval: 17")
    | MapwExpr _ -> raise (Error "eval: 18")
    | FoldwiExpr _ -> raise (Error "eval: 19")
    | FoldwExpr _ -> raise (Error "eval: 20")
    | DynamicProjectExpr _ -> raise (Error "eval: 21")

and opToFunc = function
    | ADD -> (+)
    | SUB -> (-)
    | MUL -> (fun x -> fun y -> x * y)
    | DIV -> (/)
    | MOD -> (mod)
    | _ -> (+)

and opToFFunc = function
    | ADD -> (+.)
    | SUB -> (-.)
    | MUL -> (fun x -> fun y -> x *. y)
    | DIVF -> (/.)
    | _ -> (+.)

and opToBoolFunc = function
    | AND -> (&&)
    | OR -> (||)
    | XOR -> (<>)
    | _ -> (&&)

and opToCmpFunc = function
    | GT -> (>)
    | LT -> (<)
    | GE -> (>=)
    | LE -> (<=)
    | EQ -> (=)
    | NE -> (<>)
    | _ -> (=)

and opToCmpFuncF = function
    | GT -> (>)
    | LT -> (<)
    | GE -> (>=)
    | LE -> (<=)
    | EQ -> (=)
    | NE -> (<>)
    | _ -> (=)

let rec kindToAST = function
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

and exprToInteger expr = match eval TInt expr with
    | TVInt x | TVShort x | TVUInt x | TVUShort x -> x
    | _ -> raise (Error "expr cannot be evaluated to an integer")

(*
let evalToAtomExpr kind expr = match eval expr with
    | VBool value -> (match kind with
        | AtomType Bool -> EBool (string_of_bool value)
        | IDENT ident -> (match SymbolTable.search ident with
            SymbolTable.TypeSym kind -> (match kind with
                | AtomType Bool -> EBool (string_of_bool value)
                | _ -> raise (Error "evaluated type 'bool' is incompatible with declared type")
            )
            | _ -> raise (Error "evaluated type 'bool' is incompatible with declared type")
        )
        | _ -> raise (Error "evaluated type 'bool' is incompatible with declared type")
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
                | _ -> raise (Error "evaluated type 'int' is incompatible with declared type")
            )
            | _ -> raise (Error "evaluated type 'int' is incompatible with declared type")
        )
        | _ -> raise (Error "evaluated type 'int' is incompatible with declared type")
    )
    | VFloat value -> (match kind with
        | AtomType Float -> EFloat (string_of_float value)
        | AtomType Real -> EReal (string_of_float value)
        | IDENT ident -> (match SymbolTable.search ident with
            SymbolTable.TypeSym kind -> (match kind with
                | AtomType Float -> EFloat (string_of_float value)
                | AtomType Real -> EReal (string_of_float value)
                | _ -> raise (Error "evaluated type 'float' is incompatible with declared type")
            )
            | _ -> raise (Error "evaluated type 'float' is incompatible with declared type")
        )
        | _ -> raise (Error "evaluated type 'float' is incompatible with declared type")
    )
    | VString value -> EIdent value *)

type expected =
    | ExpIdent of string
    | ExpKind of tKind
    | NoExp

(* 1. generate symbol table *)

let genSymTable ast =
    SymbolTable.enter ();
    match ast with Program nodes -> List.iter (fun node ->
        match node with
            | TypeBlk stmts -> List.iter (fun stmt ->
                match stmt with TypeStmt (_, i, k) -> SymbolTable.insert i (SymbolTable.VarSym (kindToAST k))
            ) stmts
            | _ -> ()
    ) nodes;
    match ast with Program nodes -> List.iter (fun node ->
        match node with
            | ConstBlk stmts -> List.iter (fun stmt ->
                match stmt with ConstStmt (_, i, k, e) ->
                SymbolTable.insert i (SymbolTable.VarSym (kindToAST k));
                ConstTable.insert i (kindToAST k) e
            ) stmts
            | _ -> ()
    ) nodes;
    match ast with Program nodes -> List.iter (fun node ->
        match node with
            | FuncBlk (_, _, ident, params, rets, _) ->
                let ps = List.concat (match params with ParamBlk fields -> List.map (fun field ->
                    match field with Field (is, k) -> makeList (List.length is) (kindToAST k)) fields)
                in let rs = List.concat (match rets with ReturnBlk fields -> List.map (fun field ->
                    match field with Field (is, k) -> makeList (List.length is) (kindToAST k)) fields)
                in SymbolTable.insert ident (SymbolTable.FuncSym (ps, rs))
            | _ -> ()
    ) nodes
    (* ;SymbolTable.show () *)

(* 2. evaluate const expr *)

let rec evalConstExprs ast =
    ConstTable.iter (fun k -> fun (kind, expr, _) -> ConstTable.update k (eval kind expr))
    (* ConstTable.show () *)

(* 3. transfer to ast with types *)

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
    | FuncBlk (func, _, ident, par, ret, body) ->
        SymbolTable.enter ();
        let par = paramBlkToAST par in
        let ret = returnBlkToAST ret in
        let body = bodyBlkToAST body in
        let tmp = TNodeBlk (func, ident, NULL_COMMENT, par, ret, body) in
            SymbolTable.exit ();
            tmp

and typeStmtToAST = function
    TypeStmt (_, ident, kind) -> TTypeStmt (ident, kindToAST kind, NULL_COMMENT)

and constStmtToAST = function
    ConstStmt (_, ident, kind, expr) -> TConstStmt (ident, kindToAST kind, exprToValue (kindToAST kind) expr, NULL_COMMENT)

and paramBlkToAST = function
    ParamBlk fields -> TParamBlk (List.map fieldToAST fields)

and returnBlkToAST = function
    ReturnBlk fields -> TReturnBlk (List.map fieldToAST fields)

and bodyBlkToAST = function
    | BodyBlk (var, eqs) ->
        let varBlk = (match var with
            | VarList fields -> List.map fieldToAST fields
            | NOVARBLK -> []
        ) in let eqBlk = List.map eqStmtToAST eqs in
            TBodyBlk (varBlk, eqBlk)
    | NOBODYBLK -> TBodyBlk ([], [])

and fieldToAST = function
    Field (idents, kind) ->
        List.iter (fun i -> SymbolTable.insert i (SymbolTable.VarSym (kindToAST kind))) idents;
        TDeclStmt (idents, kindToAST kind, NULL_COMMENT)

and eqStmtToAST = function
    EqStmt (lhss, expr) -> let guess = List.map (fun x -> match x with
        | ID ident -> ExpIdent ident
        | ANNOYMITY -> NoExp) lhss in
    let guidOp = match expr with
        | PrefixExpr (Ident name, _) | MapwiExpr (Ident name, _, _, _, _)
        | MapwExpr (Ident name, _, _, _, _) | FoldwiExpr (Ident name, _, _, _)
        | FoldwExpr (Ident name, _, _, _) | HighOrderExpr (_, Ident name, _, _)
            -> TGUIDOp name
        | _ -> TNOCALL
    in TAssignStmt (List.map lhsToAST lhss, exprToAST 0 guess expr, guidOp)

and lhsToAST = function
    | ID ident -> TID (ident, getKind ident, NOCLOCK)
    | ANNOYMITY -> TANONYMOUS_ID

and kindOut = function
    | TBool -> "bool"
    | TShort -> "short"
    | TUShort -> "ushort"
    | TInt -> "int"
    | TUInt -> "uint"
    | TFloat -> "float"
    | TReal -> "real"
    | TChar -> "char"
    | TEnum idents -> Printf.sprintf "construct_enum(%s)" (String.concat ", " idents)
    | TConstruct cons -> Printf.sprintf "construct(%s)" (String.concat ", " (List.map (fun (i, k) -> Printf.sprintf "field(%s, %s)" i (kindOut k)) cons))
    | TArray (kind, len) -> Printf.sprintf "array(%s, INT(%s))" (kindOut kind) len
    | TTypeName ident -> kindOut (getKind ident)

and inferType order e = match e with
    | AtomExpr (EIdent ident) -> getOriginalKind (getKind ident)
    | AtomExpr (EBool _) -> TBool
    | AtomExpr (EChar _) -> TChar
    | AtomExpr (EShort _) -> TShort
    | AtomExpr (EUShort _) -> TUShort
    | AtomExpr (EInt _) -> TInt
    | AtomExpr (EUInt _) -> TUInt
    | AtomExpr (EFloat _) -> TFloat
    | AtomExpr (EReal _) -> TReal
    | UnOpExpr (_, expr) -> inferType order expr
    | BinOpExpr (_, expr, _) -> inferType order expr
    | IfExpr (_, expr, _) -> inferType order expr
    | ArrowExpr (_, expr) -> inferType order expr
    | ArrAccessExpr (expr, _) -> (match inferType order expr with
        | TArray (kind, _) -> kind
        | _ -> raise (Error "not an array1")
    )
    | FieldExpr (expr, ident) -> (match inferType order expr with
        | TConstruct cons -> (match List.hd (List.filter (fun (i, k) -> i = ident) cons) with
            (_, k) -> k)
        | _ -> raise (Error "not a construct")
    )
    | StructExpr _ -> raise (Error "here1")
    | DynamicProjectExpr _ -> raise (Error "here2")
    | ArrConstructExpr cons -> TArray (inferType order (List.hd cons), string_of_int (List.length cons))
    | ArrNameConstructExpr _ -> raise (Error "here4")
    | WithExpr _ -> raise (Error "here5")
    | ExprList _ -> TBool
    | FbyExpr (exprs, _, _) -> inferType order (List.hd exprs)
    | PreExpr _ -> raise (Error "here7")
    | PrefixExpr (op, _) -> (match op with
        | Ident func -> getElem order (getFuncRets func)
        | _ -> raise (Error "here7 do not know")
    )
    | _ -> raise (Error "unknown type")

and exprToAST order expected e =
    let kinds = List.map (fun x -> match x with
        | ExpKind k -> k
        | ExpIdent ident -> getKind ident
        | NoExp -> inferType (findElem x expected) e) expected in
    let kind = List.hd kinds in
    match e with
        | AtomExpr expr -> TAtomExpr (atomExprToAST expr)
        | UnOpExpr (op, expr) -> TUnOpExpr (op, kind, NOCLOCK, exprToAST 0 [(match op with
                | AtomTypeOp _ -> NoExp
                | NOT -> ExpKind TBool
                | POS | NEG -> List.hd expected
            )] expr)
        | BinOpExpr (op, exprL, exprR) -> let guess = (match op with
            | AND | OR | XOR -> ExpKind TBool
            | _ -> List.hd expected
        ) in TBinOpExpr (op, kind, NOCLOCK, exprToAST 0 [guess] exprL, exprToAST 0 [guess] exprR)
        | IfExpr (exprC, exprT, exprF) -> TIfExpr (kind, NOCLOCK, exprToAST 0 [ExpKind TBool] exprC, exprToAST 0 expected exprT, exprToAST 0 expected exprF)
        | CaseExpr (sel, cases) -> TSwitchExpr (kind, NOCLOCK, exprToAST 0 [NoExp] sel, List.map (fun case -> (match case with CaseItem (p, e) -> (match p with
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
        ), exprToAST 0 expected e)) cases)
        | PreExpr expr -> TTempoPreExpr (kinds, makeList (List.length kinds) NOCLOCK, exprToAST 0 expected expr)
        | ArrowExpr (exprL, exprR) -> TTempoArrowExpr (kinds, makeList (List.length kinds) NOCLOCK, exprToAST 0 expected exprL, exprToAST 0 expected exprR)
        | FbyExpr (exprs1, value, exprs2) ->
            TTempoFbyExpr (kinds, makeList (List.length kinds) NOCLOCK,
                List.map (fun e -> exprToAST (findElem e exprs1) expected e) exprs1,
                TAtomExpr (TEInt value),
                List.map (fun e -> exprToAST (findElem e exprs2) expected e) exprs2
            )
        | FieldExpr (expr, ident) -> TFieldAccessExpr (kind, NOCLOCK, exprToAST 0 [NoExp] expr, ident)
        | ArrNameConstructExpr items ->
            TConstructExpr (kind, NOCLOCK, List.map (
                fun x -> match x with NameArrItem (i, e) -> (i, exprToAST 0 [
                    match getElem order expected with
                        | ExpIdent sym -> ExpKind (getFieldKind sym i)
                        | _ -> NoExp
                ] e)) items)
        | ArrConstructExpr exprs -> let guess = match getOriginalKind kind with
            | TArray (k, _) -> ExpKind k
            | _ -> NoExp
        in TConstructArrExpr (kind, NOCLOCK, List.map (exprToAST 0 [guess]) exprs)
        | WithExpr (ident, items, expr) -> let guess = match getOriginalKind kind with
            | TArray (k, _) -> ExpKind k
            | _ -> NoExp
        in TMixedConstructorExpr (kind, NOCLOCK, TAtomExpr (TEID (ident, getKind ident, NOCLOCK)), List.map (fun i -> match i with
            | FieldItem ident -> TIdent ident
            | AccessItem expr -> TExpr (exprToAST 0 [NoExp] expr)
        ) items, exprToAST 0 [guess] expr)
        | ArrAccessExpr (expr, exprV) ->
            TArrIdxExpr (kind, NOCLOCK, exprToAST 0 [NoExp] expr, exprToInteger exprV)
        | ArrInitExpr (expr, exprV) ->
            let guess = (match getOriginalKind kind with
                | TArray (k, _) -> ExpKind k
                | _ -> raise (Error "nicht glaube")
            ) in
            TArrDimExpr (kind, NOCLOCK, exprToAST 0 [guess] expr, exprToInteger exprV)
        | DynamicProjectExpr (expr1, exprs, expr2) -> TDynamicProjExpr (kind, NOCLOCK, exprToAST 0 [NoExp] expr1, List.map (exprToAST 0 [NoExp]) exprs, exprToAST 0 expected expr2)
        | PrefixExpr (op, exprs) -> let blk = prefixOpToAST1 op
        in TApplyExpr (kinds, makeList (List.length kinds) NOCLOCK, blk, List.map (exprToAST 0 [NoExp]) exprs)
        | HighOrderExpr (hop, op, value, exprs) -> TApplyExpr (kinds, makeList (List.length kinds) NOCLOCK, THighOrderStmt (hop, prefixOpToAST op, value), List.map (exprToAST 0 [NoExp]) exprs)
        | MapwExpr (op, value, expr1, expr2, exprs) -> TApplyExpr (kinds, makeList (List.length kinds) NOCLOCK, TMapwDefaultStmt (prefixOpToAST op, value, exprToAST 0 [NoExp] expr1, exprToAST 0 [NoExp] expr2), List.map (exprToAST 0 [NoExp]) exprs)
        | MapwiExpr (op, value, expr1, expr2, exprs) -> TApplyExpr (kinds, makeList (List.length kinds) NOCLOCK, TMapwiDefaultStmt (prefixOpToAST op, value, exprToAST 0 [NoExp] expr1, exprToAST 0 [NoExp] expr2), List.map (exprToAST 0 [NoExp]) exprs)
        | FoldwiExpr (op, value, expr, exprs) -> TApplyExpr (kinds, makeList (List.length kinds) NOCLOCK, TFoldwiStmt (prefixOpToAST op, value, exprToAST 0 [NoExp] expr), List.map (exprToAST 0 [NoExp]) exprs)
        | FoldwExpr (op, value, expr, exprs) -> TApplyExpr (kinds, makeList (List.length kinds) NOCLOCK, TFoldwIfStmt (prefixOpToAST op, value, exprToAST 0 [NoExp] expr), List.map (exprToAST 0 [NoExp]) exprs)
        | ExprList exprs -> TListExpr (List.map (exprToAST 0 expected) exprs)
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

and exprToValue kind expr = eval kind expr

and getKind ident = match SymbolTable.search ident with
    | SymbolTable.VarSym kind -> kind
    | _ -> raise (Error (Printf.sprintf "symbol '%s' doesn't name a variable" ident))

and getFuncParams ident = match SymbolTable.search ident with
    | SymbolTable.FuncSym (ks, _) -> ks
    | _ -> raise (Error (Printf.sprintf "symbol '%s' doesn't name a function" ident))

and getFuncRets ident = match SymbolTable.search ident with
    | SymbolTable.FuncSym (_, ks) -> ks
    | _ -> raise (Error (Printf.sprintf "symbol '%s' doesn't name a function" ident))

and getFieldKind ident field = match getOriginalKind (getKind ident) with
    | TConstruct cons -> (match List.hd (List.filter (fun (i, k) -> i = field) cons) with
        (_, k) -> k)
    | _ -> raise (Error (Printf.sprintf "symbol '%s.%s' doesn't name a field" ident field))

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
    TMainBlk ident -> indent depth (Printf.sprintf "main(%s)," ident);

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
    TTypeStmt (ident, kind, com) -> indent depth (Printf.sprintf "type(%s, %s, %s)" ident (kindOut1 kind) (commentOut com))

and constStmtOut depth stmt = match stmt with
    TConstStmt (ident, kind, value, com) -> indent depth (Printf.sprintf "const(%s, %s, %s, %s)" ident (kindOut1 kind) (valueOut value) (commentOut com))

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
    TDeclStmt (idents, kind, com) -> indent depth (Printf.sprintf "var_decls(vars(%s), %s, (%s))" (String.concat ", " idents) (kindOut1 kind) (commentOut com))

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
        indent (depth + 1) "),";
        String.concat ",\n" (List.map (assignStmtOut (depth + 1)) eqs);
        indent depth ")"
    ]

and assignStmtOut depth stmt = match stmt with
    TAssignStmt (lhss, expr, op) -> indent depth (Printf.sprintf "=(lvalue(%s), %s, %s, NOGUID, NOIMPORT, 0)" (String.concat ", " (List.map lhsOut lhss)) (exprOut expr) (guidOpOut op))

and guidOpOut = function
    | TGUIDOp ident -> ident
    | TNOCALL -> "NOCALL"

and lhsOut = function
    | TID (ident, kind, clk) -> Printf.sprintf "ID(%s, %s, %s)" ident (kindOut kind) (clockOut clk)
    | TANONYMOUS_ID -> "anonymous_id"

and valueOut = function
    | TVIdent (ident, kind) -> Printf.sprintf "ID(%s, %s)" ident (kindOut1 (getKind ident))
    | TVBool ident -> Printf.sprintf "BOOL(%s)" ident
    | TVShort ident -> Printf.sprintf "SHORT(%s)" ident
    | TVUShort ident -> Printf.sprintf "USHORT(%s)" ident
    | TVInt ident -> Printf.sprintf "INT(%s)" ident
    | TVUInt ident -> Printf.sprintf "UINT(%s)" ident
    | TVFloat ident -> Printf.sprintf "FLOAT(%s)" ident
    | TVReal ident -> Printf.sprintf "REAL(%s)" ident
    | TVChar ident -> Printf.sprintf "CHAR(%s)" (string_of_int (int_of_char (String.get ident 0)))
    | TVConstructor cons -> Printf.sprintf "construct(%s)" (String.concat ", " (List.map (fun (i, k) -> Printf.sprintf "label_const(%s, %s)" i (valueOut k)) cons))
    | TVArray vals -> Printf.sprintf "construct_array(%s)" (String.concat ", " (List.map valueOut vals))
    | TVPatternAny -> "pattern_any"

and prefixStmtOut = function
    | TFuncStmt (ident, params, rets) -> Printf.sprintf "prefix(%s, param_types(%s), ret_types(%s))" ident (String.concat ", " (List.map kindOut1 params)) (String.concat ", " (List.map kindOut1 rets))
    | TUnOpStmt op -> Printf.sprintf "prefix(%s)" (prefixUnOpOut op)
    | TBinOpStmt op -> Printf.sprintf "prefix(%s)" (prefixBinOpOut op)

and atomExprOut = function
    | TEID (ident, kind, clk) -> Printf.sprintf "ID(%s, %s, %s)" ident (kindOut kind) (clockOut clk)
    | TEIdent ident -> ident
    | TEBool ident -> Printf.sprintf "BOOL(%s)" ident
    | TEChar ident -> Printf.sprintf "CHAR(%s)" (string_of_int (int_of_char (String.get ident 0)))
    | TEShort ident -> Printf.sprintf "SHORT(%s)" ident
    | TEUShort ident -> Printf.sprintf "USHORT(%s)" ident
    | TEInt ident -> Printf.sprintf "INT(%s)" ident
    | TEUInt ident -> Printf.sprintf "UINT(%s)" ident
    | TEFloat ident -> Printf.sprintf "FLOAT(%s)" ident
    | TEReal ident -> Printf.sprintf "REAL(%s)" ident

and exprOut = function
    | TAtomExpr expr -> atomExprOut expr
    | TBinOpExpr (op, kind, clk, exprL, exprR) -> Printf.sprintf "%s(%s, %s, %s, %s)" (binOpOut op) (kindOut kind) (clockOut clk) (exprOut exprL) (exprOut exprR)
    | TUnOpExpr (op, kind, clk, expr) -> Printf.sprintf "%s(%s, %s, %s)" (unOpOut op) (kindOut kind) (clockOut clk) (exprOut expr)
    | TIfExpr (kind, clk, exprC, exprT, exprF) -> Printf.sprintf "if_expr((%s), (%s), %s, %s, %s)" (kindOut kind) (clockOut clk) (exprOut exprC) (exprOut exprT) (exprOut exprF)
    | TSwitchExpr (kind, clk, expr, cases) -> Printf.sprintf "switch_expr((%s), (%s), %s, %s)" (kindOut kind) (clockOut clk) (exprOut expr) (String.concat ", " (List.map (fun (v, e) -> Printf.sprintf "case(%s, %s)" (valueOut v) (exprOut e)) cases))
    | TTempoPreExpr (kinds, clks, expr) -> Printf.sprintf "tempo_pre((%s), (%s), %s)" (String.concat ", " (List.map kindOut kinds)) (String.concat ", " (List.map clockOut clks)) (exprOut expr)
    | TTempoArrowExpr (kinds, clks, exprL, exprR) -> Printf.sprintf "tempo_arrow((%s), (%s), %s, %s)" (String.concat ", " (List.map kindOut kinds)) (String.concat ", " (List.map clockOut clks))  (exprOut exprL) (exprOut exprR)
    | TTempoFbyExpr (kinds, clks, exprs1, expr, exprs2) -> Printf.sprintf "tempo_fby((%s), (%s), %s, %s, %s)" (String.concat ", " (List.map kindOut kinds)) (String.concat ", " (List.map clockOut clks)) (exprOut (TListExpr exprs1)) (exprOut expr) (exprOut (TListExpr exprs2))
    | TFieldAccessExpr (kind, clk, expr, ident) -> Printf.sprintf "field_access(%s, %s, %s, %s)" (kindOut kind) (clockOut clk) (exprOut expr) ident
    | TConstructExpr (kind, clk, cons) -> Printf.sprintf "construct(%s, %s, %s)" (kindOut kind) (clockOut clk) (String.concat " " (List.map (fun (i, e) -> Printf.sprintf "label_expr(%s, %s)," i (exprOut e)) cons))
    | TConstructArrExpr (kind, clk, exprs) -> Printf.sprintf "construct_array(%s, %s, %s)" (kindOut kind) (clockOut clk) (exprOut (TListExpr exprs))
    | TMixedConstructorExpr (kind, clk, expr1, labels, expr2) -> Printf.sprintf "mixed_constructor(%s, %s, %s, (%s), %s)" (kindOut kind) (clockOut clk) (exprOut expr1) (String.concat ", " (List.map labelIdxOut labels)) (exprOut expr2)
    | TArrDimExpr (kind, clk, expr, len) -> Printf.sprintf "array_dim((%s), (%s), %s, INT(%s))" (kindOut kind) (clockOut clk) (exprOut expr) len
    | TArrIdxExpr (kind, clk, expr, idx) -> Printf.sprintf "array_index(%s, %s, %s, INT(%s))" (kindOut kind) (clockOut clk) (exprOut expr) idx
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
    | THighOrderStmt (op, stmt, value) -> Printf.sprintf "high_order(%s, %s, INT(%s))" (highOrderOpOut op) (prefixStmtOut stmt) value
    | TPrefixStmt stmt -> prefixStmtOut stmt
    | TMapwDefaultStmt (stmt, value, expr1, expr2) -> Printf.sprintf "mapw_default(%s, INT(%s), %s, %s,)" (prefixStmtOut stmt) value (exprOut expr1) (exprOut expr2)
    | TMapwiDefaultStmt (stmt, value, expr1, expr2) -> Printf.sprintf "mapwi_default(%s, INT(%s), %s, %s,)" (prefixStmtOut stmt) value (exprOut expr1) (exprOut expr2)
    | TFoldwIfStmt (stmt, value, expr) -> Printf.sprintf "foldw_if(%s, INT(%s), %s)" (prefixStmtOut stmt) value (exprOut expr)
    | TFoldwiStmt (stmt, value, expr) -> Printf.sprintf "foldwi(%s, INT(%s), %s)" (prefixStmtOut stmt) value (exprOut expr)

and unOpOut = function
    | AtomTypeOp Bool -> "unop_boolcast"
    | AtomTypeOp Short -> "unop_shortcast"
    | AtomTypeOp UShort -> "unop_intcast"
    | AtomTypeOp Int -> "unop_intcast"
    | AtomTypeOp UInt -> "unop_intcast"
    | AtomTypeOp Float -> "unop_floatcast"
    | AtomTypeOp Real -> "unop_realcast"
    | AtomTypeOp Char -> "unop_charcast"
    | NOT -> "unop_not"
    | POS -> "unop_pos"
    | NEG -> "unop_neg"

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


(* driver *)
let drive result =
    genSymTable result;
    evalConstExprs result;
    print_endline (output (programToAST result))

let _ =
	try
		let lexbuf = Lexing.from_channel stdin in
		let result = Parser.programY Lexer.token lexbuf in
            drive result;
			flush stdout;
			exit 0
	with Parsing.Parse_error ->
		exit 2
;;
