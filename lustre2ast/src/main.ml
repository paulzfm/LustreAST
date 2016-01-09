(* File main.ml *)

open Tree

let indent depth str = Printf.sprintf "%s%s" (String.make (depth * 4) ' ') str

let rec makeList times elem =
    if times = 1 then [elem] else elem :: (makeList (times - 1) elem)

(* symbol table *)

module SymbolTable = struct
    type symbolType =
        | ConstSym of kind
        | VarSym of kind
        | TypeSym of kind
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
        | ConstSym kind -> (match kind with
            | Struct fields -> List.iter (fun f -> match f with         Field (is, k) ->
                List.iter (fun i -> Hashtbl.add !table.(length () - 1) (ident ^ "." ^ i) (ConstSym k)) is
            ) fields
            | IDENT name -> insertStruct (search name) ident
            | _ -> ()
        )
        | VarSym kind -> (match kind with
            | Struct fields -> List.iter (fun f -> match f with         Field (is, k) ->
                List.iter (fun i -> Hashtbl.add !table.(length () - 1) (ident ^ "." ^ i) (VarSym k)) is
            ) fields
            | IDENT name -> insertStruct (search name) ident
            | _ -> ()
        )
        | TypeSym kind -> (match kind with
            | Struct fields -> List.iter (fun f -> match f with         Field (is, k) ->
                List.iter (fun i -> Hashtbl.add !table.(length () - 1) (ident ^ "." ^ i) (VarSym k)) is
            ) fields
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
    | _ -> VInt 0
    (* | _ -> raise (EvalError "complex expr not supported") *)

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
    | VString value -> EIdent value

(* infer types *)

let rec evalToType = function
    | AtomExpr (EIdent ident) -> (match SymbolTable.search ident with
        | SymbolTable.ConstSym kind -> kind
        | SymbolTable.VarSym kind -> kind
        | SymbolTable.TypeSym kind -> kind
    )
    | AtomExpr (EBool _) -> AtomType Bool
    | AtomExpr (EChar _) -> AtomType Char
    | AtomExpr (EShort _) -> AtomType Short
    | AtomExpr (EUShort _) -> AtomType UShort
    | AtomExpr (EInt _) -> AtomType Int
    | AtomExpr (EUInt _) -> AtomType UInt
    | AtomExpr (EFloat _) -> AtomType Float
    | AtomExpr (EReal _) -> AtomType Real
    | UnOpExpr (_, expr) -> evalToType expr
    | BinOpExpr (op, expr, _) -> (match op with
        | DIV -> AtomType Int
        | (AND | OR | XOR | GT | LT | GE | LE | EQ | NE) -> AtomType Bool
        | _ -> evalToType expr
    )
    | FieldExpr (field, expr) -> AtomType Bool
    | StructExpr exprs -> evalToType (List.hd exprs)
    | DynamicProjectExpr (expr, _, _) -> evalToType expr
    | ArrAccessExpr (expr, _) -> evalToType expr
    | ArrInitExpr (expr, _) -> evalToType expr
    | ArrConstructExpr exprs -> evalToType (List.hd exprs)
    | ArrNameConstructExpr items -> AtomType Bool
    | PreExpr expr -> evalToType expr
    | FbyExpr (exprs, _, _) -> evalToType (List.hd exprs)
    | ArrowExpr (expr, _) -> evalToType expr
    | WhenExpr (expr, ident) -> evalToType expr
    | IfExpr (_, expr, _) -> evalToType expr
    | CaseExpr (expr, cases) -> evalToType expr
    | WithExpr (_, _, expr) -> evalToType expr
    | ExprList exprs -> evalToType (List.hd exprs)
    | PrefixExpr (_, exprs) -> evalToType (List.hd exprs)
    | HighOrderExpr (_, _, _, exprs) -> evalToType (List.hd exprs)
    | MapwiExpr _ -> AtomType Bool
    | MapwExpr _ -> AtomType Bool
    | FoldwiExpr _ -> AtomType Bool
    | FoldwExpr _ -> AtomType Bool

(* to ast *)

let isApplyBlock = ref false

let clockToAST = function
    | Clock ident -> Printf.sprintf "(%s)" ident
    | NOCLOCK -> "()"

let nullComment = "NullComment"

let funcTypeToAST = function
    | Function -> "function"
    | Node -> "node"

let binOpToAST = function
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

let rec kindToAST = function
    | AtomType kind -> atomTypeToAST kind
    | Struct fields -> Printf.sprintf "construct(%s)" (String.concat ", " (List.map fieldToAST fields))
    | Array (kind, expr) -> Printf.sprintf "array(%s, %s)" (kindToAST kind) (evalExpr (AtomType Int) expr)
    | IDENT name -> searchIdentType name (* not in decl *)
    | EnumType idents -> Printf.sprintf "construct_enum(%s)" (String.concat ", " idents)

and kindToAST1 = function
    | AtomType kind -> atomTypeToAST kind
    | Struct fields -> Printf.sprintf "construct(%s)" (String.concat ", " (List.map fieldToAST fields))
    | Array (kind, expr) -> Printf.sprintf "array(%s, %s)" (kindToAST kind) (evalExpr (AtomType Int) expr)
    | IDENT name -> Printf.sprintf "typename(%s)" name (* in decl *)
    | EnumType idents -> Printf.sprintf "construct_enum(%s)" (String.concat ", " idents)

and prefixOpToAST = function
    | Ident ident ->
        let x, y = match SymbolTable.search ident with
            | SymbolTable.FuncSym (params, rets) -> (String.concat ", " (List.map kindToAST params)), (String.concat ", " (List.map kindToAST rets))
            | _ -> raise (EvalError "func error")
        in Printf.sprintf "%s, param_types(%s), ret_types(%s)" ident x y
    | UnOp op -> prefixUnOpToAST op
    | BinOp op -> prefixBinOpToAST op
    | Flatten ident -> Printf.sprintf "flatten(%s, %s)" ident ""
    | Make ident -> Printf.sprintf "make(%s, %s)" ident ""

and fieldToAST = function
    Field (idents, kind) -> Printf.sprintf "field(%s, %s)" (List.hd idents) (kindToAST1 kind)

and searchIdentType ident = match SymbolTable.search ident with
    | SymbolTable.ConstSym kind -> kindToAST kind
    | SymbolTable.VarSym kind -> kindToAST kind
    | SymbolTable.TypeSym kind -> kindToAST kind
    | _ -> raise (EvalError "unkon kind")

and atomExprToAST = function
    | EIdent ident -> Printf.sprintf "ID(%s, %s, %s)" ident (try searchIdentType ident with
        | SymbolTable.SymbolError _ -> ident
        | _ -> searchIdentType ident
    ) (clockToAST NOCLOCK)
    | EBool ident -> Printf.sprintf "BOOL(%s)" ident
    | EChar ident -> Printf.sprintf "CHAR(%s)" (string_of_int (int_of_char (String.get ident 0)))
    | EShort ident -> Printf.sprintf "SHORT(%s)" ident
    | EUShort ident -> Printf.sprintf "USHORT(%s)" ident
    | EInt ident -> Printf.sprintf "INT(%s)" ident
    | EUInt ident -> Printf.sprintf "UINT(%s)" ident
    | EFloat ident -> Printf.sprintf "FLOAT(%s)" ident
    | EReal ident -> Printf.sprintf "REAL(%s)" ident

and exprToAST ident e = match e with
    | AtomExpr expr -> atomExprToAST expr
    | UnOpExpr (op, expr) -> Printf.sprintf "%s(%s, %s, %s)" (unOpToAST op) (searchIdentType ident) (clockToAST NOCLOCK) (exprToAST ident expr)
    | BinOpExpr (op, exprL, exprR) -> Printf.sprintf "%s(%s, %s, %s, %s)" (binOpToAST op) (searchIdentType ident) (clockToAST NOCLOCK) (exprToAST ident exprL) (exprToAST ident exprR)
    | FieldExpr (expr, name) -> Printf.sprintf "field_access(%s, %s, %s, %s)" (searchIdentType ident) (clockToAST NOCLOCK) (exprToAST ident expr) name
    | StructExpr exprs -> "!!!2"
    | DynamicProjectExpr (expr1, exprs, expr2) -> Printf.sprintf "dynamic_project(%s, %s, %s, (%s), %s)" "" (clockToAST NOCLOCK) (exprToAST ident expr1) (String.concat ", " (List.map (exprToAST ident) exprs)) (exprToAST ident expr2)
    | ArrAccessExpr (expr, idx) -> Printf.sprintf "array_index(%s, %s, %s, %s)" (searchIdentType ident) (clockToAST NOCLOCK) (exprToAST ident expr) (exprToAST ident idx)
    | ArrInitExpr (expr, dim) -> Printf.sprintf "array_dim(%s, %s, %s, %s)" (searchIdentType ident) (clockToAST NOCLOCK) (exprToAST ident expr) (exprToAST ident dim)
    | ArrConstructExpr exprs -> Printf.sprintf "construct_array(%s, %s, list_expr(%s))" (searchIdentType ident) (clockToAST NOCLOCK) (String.concat ", " (List.map (exprToAST ident) exprs))
    | ArrNameConstructExpr items -> Printf.sprintf "construct(%s, %s, %s)" (searchIdentType ident) (clockToAST NOCLOCK) (String.concat ", " (List.map (nameArrItemToAST ident) items))
    | PreExpr expr -> Printf.sprintf "tempo_pre(%s, %s, %s)" (searchIdentType ident) (clockToAST NOCLOCK) (exprToAST ident expr)
    | FbyExpr (exprs1, integer, exprs2) -> Printf.sprintf "tempo_fby(%s, %s, list_expr(%s), %s, list_expr(%s))" (searchIdentType ident) (clockToAST NOCLOCK) (String.concat ", " (List.map (exprToAST ident) exprs1)) integer (String.concat ", " (List.map (exprToAST ident) exprs2))
    | ArrowExpr (exprL, exprR) -> Printf.sprintf "tempo_arrow(%s, %s, %s, %s)" (searchIdentType ident) (clockToAST NOCLOCK) (exprToAST ident exprL) (exprToAST ident exprR)
    | WhenExpr (expr, ident) -> "!!!4"
    | IfExpr (cond, exprT, exprF) -> Printf.sprintf "if_expr(%s, %s, %s, %s, %s)" (searchIdentType ident) (clockToAST NOCLOCK) (exprToAST ident cond) (exprToAST ident exprT) (exprToAST ident exprF)
    | CaseExpr (expr, cases) -> Printf.sprintf "switch_expr((%s), (%s), %s, %s)" (searchIdentType ident) (clockToAST NOCLOCK) (exprToAST ident expr) (String.concat ", " (List.map (caseItemToAST ident) cases))
    | WithExpr (ident, items, expr) -> ""
    | ExprList (exprs) -> Printf.sprintf "list_expr(%s)" (String.concat ", " (List.map (exprToAST ident) exprs))
    | PrefixExpr (op, exprs) ->
        if !isApplyBlock then
            Printf.sprintf "prefix(%s)" (prefixOpToAST op)
        else begin
            isApplyBlock := true;
            let tmp = Printf.sprintf "apply_expr((%s), (%s), prefix(%s), %s)" (searchIdentType ident) (clockToAST NOCLOCK) (prefixOpToAST op) (exprToAST ident (ExprList exprs)) in
            isApplyBlock := false;
            tmp
        end
    | HighOrderExpr (hop, op, value, exprs) ->
        isApplyBlock := true;
        let tmp = Printf.sprintf "apply_expr((%s), (%s), high_order(%s, %s, INT(%s)), %s)" (searchIdentType ident) (clockToAST NOCLOCK) (highOrderOpToAST hop) (exprToAST ident (PrefixExpr (op, exprs))) value (exprToAST ident (ExprList exprs)) in
        isApplyBlock := false;
        tmp
    | MapwiExpr (op, integer, expr1, expr2, exprs) ->
        isApplyBlock := true;
        let tmp = Printf.sprintf "apply_expr((%s), (%s), mapwi_default(%s, %s, %s, %s), %s" (searchIdentType ident) (clockToAST NOCLOCK) (prefixOpToAST op) integer (exprToAST ident expr1) (exprToAST ident expr2) (exprToAST ident (ExprList exprs)) in
        isApplyBlock := false;
        tmp
    | MapwExpr (op, integer, expr1, expr2, exprs) ->
        isApplyBlock := true;
        let tmp = Printf.sprintf "apply_expr((%s), (%s), mapw_default(%s, %s, %s, %s), %s" (searchIdentType ident) (clockToAST NOCLOCK) (prefixOpToAST op) integer (exprToAST ident expr1) (exprToAST ident expr2) (exprToAST ident (ExprList exprs)) in
        isApplyBlock := false;
        tmp
    | FoldwiExpr (op, integer, expr, exprs) ->
        isApplyBlock := true;
        let tmp = Printf.sprintf "apply_expr((%s), (%s), foldwi(%s, %s, %s), %s)" (searchIdentType ident) (clockToAST NOCLOCK) (prefixOpToAST op) integer (exprToAST ident expr) (exprToAST ident (ExprList exprs)) in
        isApplyBlock := false;
        tmp
    | FoldwExpr (op, integer, expr, exprs) ->
        isApplyBlock := true;
        let tmp = Printf.sprintf "apply_expr((%s), (%s), foldw_if(%s, %s, %s), %s)" (searchIdentType ident) (clockToAST NOCLOCK) (prefixOpToAST op) integer (exprToAST ident expr) (exprToAST ident (ExprList exprs)) in
        isApplyBlock := false;
        tmp

and caseItemToAST ident item = match item with
    CaseItem (pattern, expr) -> Printf.sprintf "case(%s, %s)" (patternToAST ident pattern) (exprToAST "" expr)

and nameArrItemToAST ident e = match e with
    NameArrItem (name, expr) -> Printf.sprintf "label_expr(%s, %s)" name (exprToAST (ident ^ "." ^ name) expr)

and withItemToAST = function
    | FieldItem ident -> ident
    | AccessItem expr -> exprToAST "" expr

and lhsToAST = function
    | ID ident -> Printf.sprintf "ID(%s, %s, %s)" ident (searchIdentType ident) (clockToAST NOCLOCK)
    | ANNOYMITY -> "anonymous_id"

and patternToAST sel pat = match pat with
    | PIdent ident -> Printf.sprintf "ID(%s, %s)" ident (searchIdentType ident)
    | PBool ident -> Printf.sprintf "BOOL(%s)" ident
    | PChar ident -> Printf.sprintf "CHAR(%s)" ident
    | PShort ident -> Printf.sprintf "SHORT(%s)" ident
    | PUShort ident -> Printf.sprintf "USHORT(%s)" ident
    | PInt ident -> Printf.sprintf "INT(%s)" ident
    | PUInt ident -> Printf.sprintf "UINT(%s)" ident
    | PFloat ident -> Printf.sprintf "FLOAT(%s)" ident
    | PReal ident -> Printf.sprintf "REAL(%s)" ident
    | DefaultPattern -> "pattern_any"

and evalExpr kind expr = atomExprToAST (evalToAtomExpr kind expr)

let callFuncName = function
    | PrefixExpr (Ident name, _) -> name
    | _ -> "NOCALL"

let eqStmtToAST depth stmt = match stmt with
    EqStmt (lhss, expr) -> indent depth (Printf.sprintf "=(lvalue(%s), %s, %s, NOGUID, NOIMPORT, 0)" (String.concat ", " (List.map lhsToAST lhss)) (exprToAST (match List.hd lhss with
        | ID ident -> ident
        | _ -> ""
    ) expr) (callFuncName expr))

let declStmtToAST depth stmt = match stmt with
    Field (idents, kind) -> indent depth (Printf.sprintf "var_decls(vars(%s), %s, (%s))" (String.concat ", " idents) (kindToAST1 kind) nullComment)

let varBlkToAST depth stmt = match stmt with
    | VarList fields ->
        List.iter (fun field -> match field with
            Field (idents, kind) -> List.iter (fun ident ->
                SymbolTable.insert ident (SymbolTable.VarSym kind)
            ) idents
        ) fields;
        String.concat "\n" [
            indent depth "localvars(";
            String.concat ",\n" (List.map (declStmtToAST (depth + 1)) fields);
            indent depth "),"
        ]
    | NOVARBLK -> ""

let paramBlkToAST depth stmt = match stmt with
    ParamBlk fields ->
        List.iter (fun field -> match field with
            Field (idents, kind) -> List.iter (fun ident ->
                SymbolTable.insert ident (SymbolTable.VarSym kind)
            ) idents
        ) fields;
        String.concat "\n" [
            indent depth "params(";
            String.concat ",\n" (List.map (declStmtToAST (depth + 1)) fields);
            indent depth ")"
        ]

let returnBlkToAST depth stmt = match stmt with
    ReturnBlk fields ->
        List.iter (fun field -> match field with
            Field (idents, kind) -> List.iter (fun ident ->
                SymbolTable.insert ident (SymbolTable.VarSym kind)
            ) idents
        ) fields;
        String.concat "\n" [
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
    | BodyBlk (varBlk, eqs) -> String.concat "\n" (List.rev [
        indent depth ")";
        String.concat ",\n" (List.map (eqStmtToAST (depth + 1)) eqs);
        varBlkToAST (depth + 1) varBlk;
        indent depth "body("
      ])
    | NOBODYBLK -> "No body"

let typeStmtToAST depth stmt = match stmt with
    TypeStmt (_, ident, kind) -> indent depth (Printf.sprintf "type(%s, %s, %s)" ident (kindToAST1 kind) nullComment)

let constStmtToAST depth stmt = match stmt with
    ConstStmt (_, ident, kind, expr) -> indent depth (Printf.sprintf "const(%s, %s, %s, %s)" ident (kindToAST1 kind) (evalExpr kind expr) nullComment)

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
    | FuncBlk (funcType, _, ident, paramBlk, returnBlk, bodyBlk) ->
        let params = List.concat (match paramBlk with ParamBlk fields -> List.map (fun field -> match field with
            Field (idents, kind) -> makeList (List.length idents) kind
        ) fields) in
        let rets = List.concat (match returnBlk with ReturnBlk fields -> List.map (fun field -> match field with
            Field (idents, kind) -> makeList (List.length idents) kind
        ) fields) in
        SymbolTable.insert ident (SymbolTable.FuncSym (params, rets));
        SymbolTable.enter ();
        let tmp = String.concat "\n" [
            indent depth "node(";
            String.concat ",\n" (List.rev [
                bodyBlkToAST (depth + 1) bodyBlk;
                returnBlkToAST (depth + 1) returnBlk;
                paramBlkToAST (depth + 1) paramBlk;
                indent (depth + 1) nullComment;
                indent (depth + 1) ident;
                indent (depth + 1) "";
                indent (depth + 1) (funcTypeToAST funcType);
            ]);
            indent depth ")"
        ] in
        SymbolTable.exit ();
        tmp


let searchMain nodes = match (List.hd (List.filter (
    fun node -> match node with
        | FuncBlk _ -> true
        | _ -> false
    ) nodes)) with
        | FuncBlk (_, _, ident, _, _, _) -> ident
        | _ -> ""

let programToAST depth program = match program with
    Program nodes ->
        SymbolTable.enter ();
        SymbolTable.insert "#bool" (SymbolTable.VarSym (AtomType Bool));
        (* types *)
        List.iter (fun node -> match node with
            | TypeBlk stmts -> List.iter (
                fun stmt -> match stmt with TypeStmt (_, ident, kind) ->
                    SymbolTable.insert ident (SymbolTable.TypeSym kind);
            ) stmts
            | _ -> ()
        ) nodes;
        (* consts *)
        List.iter (fun node -> match node with
            | ConstBlk stmts -> List.iter (
                fun stmt -> match stmt with ConstStmt (_, ident, kind, _) ->
                    SymbolTable.insert ident (SymbolTable.ConstSym kind);
            ) stmts
            | _ -> ()
        ) nodes;
        let tmp = String.concat "\n" [
            indent depth "TopLevel(";
            indent (depth + 1) (Printf.sprintf "main(%s)," (searchMain nodes));
            indent (depth + 1) "program(";
            String.concat ",\n" (List.map (nodeBlkToAST (depth + 2)) nodes);
            indent (depth + 1) ")";
            indent depth ")";
        ] in
        SymbolTable.exit ();
        tmp

let toAST program = programToAST 0 program

let toLustre = function
    Program (_) -> "OK"
;;

(* output *)
let rec output = function
    TTopLevel (main, prog) -> String.concat "\n" [
        indent 0 "TopLevel(";
        mainBlkOut (depth + 1) main;
        programBlkOut (depth + 1) prog;
        indent 0 ")";
    ]

and programBlkOut depth blk = match blk with
    TMainBlk blks -> String.concat "\n" [
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
    TConstStmt (ident, kind, value, com) -> indent depth (Printf.sprintf "const(%s, %s, %s)" ident (kindOut kind) (valueOut) (commentOut com))

and funcTypeOut = function
    | Node -> "node"
    | Function -> "function"

and clockOut = function
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
    | TBodyBlk (_, eqs) -> String.concat "\n" [
        indent depth "body(";
        String.concat ",\n" (List.map (assignStmtOut (depth + 1)) eqs);
        indent depth ")"
    ]
    | TBodyBlk (vars, eqs) -> String.concat "\n" [
        indent depth "body(";
        indent (depth + 1) "localvars(";
        List.map (declStmtOut (depth + 2)) vars;
        indent (depth + 1) ")";
        String.concat ",\n" (List.map (assignStmtOut (depth + 1)) eqs);
        indent depth ")"
    ]

and assignStmtOut depth stmt = match stmt with
    TAssignStmt (lhss, expr) -> indent depth (Printf.sprintf "=(lvalue(%s), %s, NULL)" (List.map lhsOut lhss) (exprOut expr))

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
    | TVConstructor cons -> Printf.sprintf "construct(%s)" (String.concat ", " (List.map (fun (i, k) -> Printf.sprintf "field(%s, %s)" i (kindOut k)) cons))
    | TVArray vals -> Printf.sprintf "construct_array(%s)" (String.concat ", " (List.map valueOut vals))
    | TVPatternAny -> "pattern_any"

and prefixStmtOut = function
    | TFuncStmt (ident, params, rets) -> Printf.sprintf "prefix(%s, param_types(%s), ret_types(%s))" ident (String.concat ", " (List.map kindOut params)) (String.concat ", " (List.map kindOut rets))
    | TUnOpStmt op -> Printf.sprintf "prefix(%s)" (prefixUnOpOut op)
    | TBinOpStmt op -> Printf.sprint "prefix(%s)" (prefixBinOpOut op)

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
