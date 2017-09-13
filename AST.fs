module AST

open System

type Value =
    | LString of string
    | LBool of bool
    | LNumber of double
    | LNil

type UnOp =
    | Negate
    | Not

type BinOp =
    | Add
    | Sub
    | Mul
    | Div
    | Equal
    | NotEqual
    | GreaterThan
    | GreaterThanOrEqual
    | LessThan
    | LessThanOrEqual
    
type Expr =
    | Literal of Value // todo: not this
    | Identifier of string
    | BinOp of BinOp * Expr * Expr
    | UnOp of UnOp * Expr
    | Call of Expr * Expr list

type Statement =
    | VariableDeclaration of string * Expr
    | Print of Expr
    | Expression of Expr