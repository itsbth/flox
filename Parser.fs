module Parser

open AST
open Scanner

let rec expression tokens = equality tokens

and equality tokens = 
    let inline next lhs op rest = 
        let (rhs, rest) = comparison rest
        (Expr.BinOp(op, lhs, rhs), rest)
    
    let (lhs, rest) = comparison tokens
    match rest with
    | EQUAL_EQUAL :: rest -> next lhs BinOp.Equal rest
    | BANG_EQUAL :: rest -> next lhs BinOp.NotEqual rest
    | _ -> (lhs, rest)

and comparison tokens = 
    let inline next lhs op rest = 
        let (rhs, rest) = addition rest
        (Expr.BinOp(op, lhs, rhs), rest)
    
    let (lhs, rest) = addition tokens
    match rest with
    | LESS :: rest -> next lhs BinOp.LessThan rest
    | LESS_EQUAL :: rest -> next lhs BinOp.LessThanOrEqual rest
    | GREATER :: rest -> next lhs BinOp.GreaterThan rest
    | GREATER_EQUAL :: rest -> next lhs BinOp.LessThanOrEqual rest
    | _ -> (lhs, rest)

and addition tokens = 
    let inline next lhs op rest = 
        let (rhs, rest) = addition rest
        (Expr.BinOp(op, lhs, rhs), rest)
    
    let (lhs, rest) = multiplication tokens
    match rest with
    | PLUS :: rest -> next lhs BinOp.Add rest
    | MINUS :: rest -> next lhs BinOp.Sub rest
    | _ -> (lhs, rest)

and multiplication tokens = 
    let inline next lhs op rest = 
        let (rhs, rest) = multiplication rest
        (Expr.BinOp(op, lhs, rhs), rest)
    
    let (lhs, rest) = unary tokens
    match rest with
    | STAR :: rest -> next lhs BinOp.Mul rest
    | SLASH :: rest -> next lhs BinOp.Div rest
    | _ -> (lhs, rest)

and unary tokens = 
    let inline next op rest = 
        let (rhs, rest) = primary rest
        (Expr.UnOp(op, rhs), rest)
    match tokens with
    | MINUS :: rest -> next UnOp.Negate rest
    | BANG :: rest -> next UnOp.Not rest
    | _ -> primary tokens

and primary tokens = 
    match tokens with
    | NUMBER n :: rest -> (Literal (LNumber n), rest)
    | STRING s :: rest -> (Literal (LString s), rest)
    | IDENTIFIER n :: rest -> (Identifier n, rest)
    | LEFT_PAREN :: rest -> 
        let (expr, rest) = expression rest
        match rest with
        | RIGHT_PAREN :: rest -> (expr, rest)
        | _ -> failwith "unterminated parens"
    | tok :: rest -> failwithf "unexpected token %A, expected literal or expression" tok
    | [] -> failwith "unexpected eof"

let rec statement tokens =
    match tokens with
    | PRINT :: rest ->
        let (expr, rest) = expression rest in (Statement.Print expr, rest)
    | VAR :: IDENTIFIER n :: EQUAL :: rest -> let (expr, rest) = expression rest in Statement.VariableDeclaration (n, expr), rest
    | VAR :: IDENTIFIER n :: rest -> Statement.VariableDeclaration (n, Literal LNil), rest
    | _ -> let (expr, rest) = expression tokens in (Statement.Expression expr, rest)