module Evaluator

open AST

type Scope = Map<string, Value>

let rec evaluate expr scope = 
    let unop op rhs = 
        let rhs = evaluate rhs scope
        match op, rhs with
        | UnOp.Negate, LNumber n -> LNumber -n
        | UnOp.Not, LBool b -> LBool(not b)
        | _ -> failwith "Invalid operand type"
    
    let binop op lhs rhs = 
        let (lhs, rhs) = evaluate lhs scope, evaluate rhs scope
        match op, lhs, rhs with
        | BinOp.Add, LNumber lhs, LNumber rhs -> LNumber(lhs + rhs)
        | BinOp.Add, LString lhs, LString rhs -> LString(lhs + rhs)
        | BinOp.Sub, LNumber lhs, LNumber rhs -> LNumber(lhs - rhs)
        | BinOp.Mul, LNumber lhs, LNumber rhs -> LNumber(lhs * rhs)
        | BinOp.Div, LNumber lhs, LNumber rhs -> LNumber(lhs / rhs)
        | BinOp.Equal, a, b -> LBool(a = b)
        | BinOp.NotEqual, a, b -> LBool(a <> b)
        | _ -> failwith "binop fail"
    
    match expr with
    | BinOp(op, lhs, rhs) -> binop op lhs rhs
    | UnOp(op, rhs) -> unop op rhs
    | Literal l -> l
    | Identifier n -> Scope.get scope n
    | Call(expr, args) -> LNil
    | _ -> failwith "missing stuff"

let rec execute stmnt scope = 
    match stmnt with
    | Print expr -> 
        printfn "%A" (evaluate expr scope)
        None, scope
    // XXX: Ugly hack to keep scope modifications out of evaluate, but only supports "root" assignments (Hey, that's a feature)
    | Expression(Expr.Assignment(n, expr)) -> 
        let res = evaluate expr scope
        Some res, Scope.set scope n res
    | Expression expr -> evaluate expr scope |> Some, scope
    | VariableDeclaration(name, expr) -> 
        let rhs = evaluate expr scope
        Some rhs, Scope.add scope name rhs
    | Block stmnts ->
        let scope = Scope.nest scope
        let (ret, scope) = List.fold (fun (_, scope) stmnt -> execute stmnt scope) (None, scope) stmnts
        ret, Scope.unnest scope
    | IfStatement(condition, ifTrue, ifFalse) ->
        if evaluate condition scope = LBool(false) then
            execute ifTrue scope
        else
            match ifFalse with
            | Some ifFalse -> execute ifFalse scope
            | None -> None, scope
