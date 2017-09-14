module Parser

open AST
open Scanner

// todo: move everything under expression, mutual recursion is not used
let rec expression tokens = assignment tokens

and assignment tokens =
    let (lhs, rest) = equality tokens
    match lhs, rest with
    | Identifier n, EQUAL :: rest ->
        let (rhs, rest) = expression rest
        Expr.Assignment(n, rhs), rest
    | _, _ -> lhs, rest

and equality tokens = 
    let inline next lhs op rest = 
        let (rhs, rest) = comparison rest
        Expr.BinOp(op, lhs, rhs), rest
    
    let (lhs, rest) = comparison tokens
    match rest with
    | EQUAL_EQUAL :: rest -> next lhs BinOp.Equal rest
    | BANG_EQUAL :: rest -> next lhs BinOp.NotEqual rest
    | _ -> (lhs, rest)

and comparison tokens = 
    let inline next lhs op rest = 
        let (rhs, rest) = addition rest
        Expr.BinOp(op, lhs, rhs), rest
    
    let (lhs, rest) = addition tokens
    match rest with
    | LESS :: rest -> next lhs BinOp.LessThan rest
    | LESS_EQUAL :: rest -> next lhs BinOp.LessThanOrEqual rest
    | GREATER :: rest -> next lhs BinOp.GreaterThan rest
    | GREATER_EQUAL :: rest -> next lhs BinOp.LessThanOrEqual rest
    | _ -> (lhs, rest)

and addition tokens = 
    let rec addition' tokens lhs = 
        match tokens with
        | PLUS :: rest -> 
            let (rhs, rest) = multiplication rest
            addition' rest (Expr.BinOp(BinOp.Add, lhs, rhs))
        | MINUS :: rest -> 
            let (rhs, rest) = multiplication rest
            addition' rest (Expr.BinOp(BinOp.Sub, lhs, rhs))
        | _ -> (lhs, tokens)
    
    let (lhs, rest) = multiplication tokens
    addition' rest lhs

and multiplication tokens = 
    let rec multiplication' tokens lhs =
        match tokens with
        | STAR :: rest ->
            let (rhs, rest) = unary rest
            multiplication' rest (Expr.BinOp(BinOp.Mul, lhs, rhs))
        | SLASH :: rest ->
            let (rhs, rest) = unary rest
            multiplication' rest (Expr.BinOp(BinOp.Div, lhs, rhs))
        | _ -> lhs, tokens
    
    let (lhs, rest) = unary tokens
    multiplication' rest lhs

and unary tokens = 
    let inline next op rest = 
        let (rhs, rest) = call rest
        Expr.UnOp(op, rhs), rest
    match tokens with
    | MINUS :: rest -> next UnOp.Negate rest
    | BANG :: rest -> next UnOp.Not rest
    | _ -> call tokens

and call tokens =
    let rec call' lhs tokens args =
        match tokens with
        | RIGHT_PAREN :: rest -> Call(lhs, args |> List.rev), rest
        | _ ->
            let (arg, rest) = expression tokens
            match rest with
            | COMMA :: rest | rest -> call' lhs rest (arg :: args)
    let (lhs, rest) = primary tokens
    match rest with
    | LEFT_PAREN :: rest -> call' lhs rest []
    | _ -> lhs, rest

and primary tokens = 
    match tokens with
    | NUMBER n :: rest -> Literal(LNumber n), rest
    | STRING s :: rest -> Literal(LString s), rest
    | IDENTIFIER n :: rest -> Identifier n, rest
    | TRUE :: rest -> Literal(LBool true), rest
    | FALSE :: rest -> Literal(LBool true), rest
    | LEFT_PAREN :: rest -> 
        let (expr, rest) = expression rest
        match rest with
        | RIGHT_PAREN :: rest -> expr, rest
        | _ -> failwith "unterminated parens"
    | tok :: rest -> failwithf "unexpected token %A, expected literal or expression" (* fool of a *) tok
    | [] -> failwith "unexpected eof"

let rec statement tokens =
    let block tokens =
        let rec block' tokens stmnts =
            match tokens with
            | RIGHT_BRACE :: rest ->
                stmnts, rest
            | _ ->
                let (stmnt, rest) = statement tokens
                match rest with
                | EOF :: [] | [] -> failwith "eof in block"
                | SEMICOLON :: rest | rest -> block' rest (stmnt :: stmnts)
        let (stmnts, rest) = block' tokens []
        stmnts |> List.rev, rest
    
    let ifstmnt tokens =
        match tokens with
        | LEFT_PAREN :: rest ->
            let (condition, rest) = expression rest
            match rest with
            | RIGHT_PAREN :: rest ->
                let (ifTrue, rest) = statement rest
                match rest with
                | ELSE :: rest ->
                    let (ifFalse, rest) = statement rest
                    Statement.IfStatement(condition, ifTrue, Some ifFalse), rest
                | _ ->
                    Statement.IfStatement(condition, ifTrue, None), rest
            | _ -> failwith "unterminated condition in if statement"
        | _ -> failwith "unable to parse if statement"

    match tokens with
    | PRINT :: rest -> 
        let (expr, rest) = expression rest
        Statement.Print expr, rest
    | VAR :: IDENTIFIER n :: EQUAL :: rest -> 
        let (expr, rest) = expression rest
        Statement.VariableDeclaration(n, expr), rest
    | VAR :: IDENTIFIER n :: rest -> Statement.VariableDeclaration(n, Literal LNil), rest
    | LEFT_BRACE :: rest ->
        let (stmnts, rest) = block rest
        Statement.Block(stmnts), rest
    | IF :: rest ->
        ifstmnt rest
    | _ -> 
        let (expr, rest) = expression tokens
        Statement.Expression expr, rest

let rec program tokens =
    // need to decide on seq vs list accumulator, using both is just messy
    seq {
        let (stmnt, rest) = statement tokens
        yield stmnt
        match rest with
        | SEMICOLON :: EOF :: [] | EOF :: [] ->  ()
        | SEMICOLON :: rest | rest -> yield! program rest
    }
