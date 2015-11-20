(* File main.ml *)

open Tree

(* to AST *)
let commentToAST = function
    | Comment (ident) -> String.concat "" ["comment("; ident; ")"]
    | NULL_COMMENT -> "NullComment"

let clockToAST = function
    | Clock (ident) -> ident
    | NOCLOCK -> "()"

let kindToAST = function
    | Bool -> "bool"
    | Short -> "short"
    | UShort -> "ushort"
    | Int -> "int"
    | UInt -> "uint"
    | Float -> "float"
    | Real -> "real"
    | Char -> "char"

let declStmtToAST = function
    DeclStmt (ident, kind, comment) ->
        String.concat "" ["var_decls("; ident; ","; kindToAST kind; ","; commentToAST comment; ")"]

let declStmtListToAST = function
    xs -> String.concat "," (List.map declStmtToAST xs)

let lhsToAST = function
    ID (ident, kind, clock) -> String.concat "" ["ID("; ident; ","; kindToAST kind; ","; clockToAST clock; ")"]

let guidOpToAST = function
    | GUIDOp (ident) -> ident
    | NOCALL -> "NOCALL"

let guidValToAST = function
    | GUIDVal (guid) -> guid
    | NOGUID -> "NOGUID"

let importedToAST = function
    | NOIMPORT -> "NOIMPORT"
    | IMPORTED -> "IMPORTED"

let importCodeToAST = function
    ImportCode (ident) -> ident

let atomExprToAST = function
    | EID (ident, kind, clock) -> String.concat "" ["ID("; ident; ","; kindToAST kind; ","; clockToAST clock; ")"]
    | EIdent ident -> ident
    | EBool ident -> ident
    | EChar ident -> ident
    | EShort ident -> ident
    | EUShort ident -> ident
    | EInt ident -> ident
    | EUInt ident -> ident
    | EFloat ident -> ident
    | EReal ident -> ident

let binOpToAST = function
    | ADD -> "add"
    | SUB -> "sub"

let rec exprToAST = function
    | AtomExpr (expr) -> atomExprToAST expr
    | BinOpExpr (op, kind, clock, expr1, expr2) -> String.concat "" ["binop_"; binOpToAST op ;"(";
        kindToAST kind; ","; clockToAST clock; ","; exprToAST expr1; ","; exprToAST expr2; ")"]

let assignStmtToAST = function
    AssignStmt (lhs, expr, guidOp, guidVal, imported, importCode) -> String.concat "" ["=("; lhsToAST lhs; ","; exprToAST expr; ","; guidOpToAST guidOp; ",";  guidValToAST guidVal; ","; importedToAST imported; ","; importCodeToAST importCode; ")"]

let paramBlkToAST = function
    ParamBlk xs -> String.concat "" ["params("; declStmtListToAST xs; ")"]

let returnBlkToAST = function
    ReturnBlk xs -> String.concat "" ["returns("; declStmtListToAST xs; ")"]

let bodyBlkToAST = function
    BodyBlk (ds, _as) -> String.concat "" ["body(\n";
        String.concat "," (List.map assignStmtToAST _as);
    ")"]

let nodeKindToAST = function
    | Node -> "node"
    | Function -> "function"

let stmtBlkToAST = function
    NodeBlk (nodeKind, guid, ident, comment, paramBlk, returnBlk, bodyBlk) ->
        String.concat "" ["node(\n"; nodeKindToAST nodeKind; ",\n"; guid; ",\n"; ident; ",\n";
            commentToAST comment; ",\n"; paramBlkToAST paramBlk; ",\n";
            returnBlkToAST returnBlk; ",\n"; bodyBlkToAST bodyBlk; ")\n"]

let stmtListToAST = function
    xs -> String.concat "," (List.map stmtBlkToAST xs)

let mainBlkToAST = function
    MainBlk (ident) -> String.concat "" ["main("; ident; ")"]

let programBlkToAST = function
    ProgramBlk stmts -> String.concat "" ["program(\n"; stmtListToAST stmts; ")"]

let toAST = function
    TopLevel (m, p) -> String.concat "" [
        "=====\nTopLevel("; mainBlkToAST m; ",\n"; programBlkToAST p; ")"
    ]

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
