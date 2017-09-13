module Evaluator

open AST

type Scope = Map<string, Value>

let rec evaluate expr scope =
    let unop op rhs =
        let rhs = evaluate rhs scope
        match op, rhs with
        | UnOp.Negate, LNumber n -> LNumber -n
        | UnOp.Not, LBool b -> LBool (not b)
        | _ -> failwith "Invalid operand type"
    let binop op lhs rhs =
        let (lhs, rhs) = evaluate lhs scope, evaluate rhs scope
        match op, lhs, rhs with
        | BinOp.Add, LNumber lhs, LNumber rhs -> LNumber (lhs + rhs)
        | BinOp.Add, LString lhs, LString rhs -> LString (lhs + rhs)
        | BinOp.Sub, LNumber lhs, LNumber rhs -> LNumber (lhs - rhs)
        | BinOp.Mul, LNumber lhs, LNumber rhs -> LNumber (lhs * rhs)
        | BinOp.Div, LNumber lhs, LNumber rhs -> LNumber (lhs / rhs)
        | BinOp.Equal, a, b -> LBool (a = b)
        | BinOp.NotEqual, a, b -> LBool (a <> b)
        | _ -> failwith "binop fail"
    match expr with
    | BinOp (op, lhs, rhs) -> binop op lhs rhs
    | UnOp (op, rhs) -> unop op rhs
    | Literal l -> l
    | Identifier n -> Map.find n scope
    | _ -> failwith "missing stuff"

let rec execute stmnt scope =
    match stmnt with
    | Print expr -> printfn "%A" (evaluate expr); None, scope
    | Expression expr -> evaluate expr scope |> Some, scope
    | VariableDeclaration (name, expr) -> let rhs = evaluate expr scope in Some rhs, Map.add name rhs scope