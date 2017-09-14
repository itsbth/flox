[<AutoOpen>]
module Scope

open System

type Scope<'a, 'b when 'a : comparison> private() =
    static member make =
        Map.empty<'a, 'b> :: []
    static member set scope k v =
        match scope with
        | x :: xs when Map.containsKey k x -> (Map.add k v x) :: xs
        | x :: xs -> x :: (Scope<_, _>.set xs k v)
        | [] -> failwithf "%A is not in scope" k
    static member get scope k =
        match scope with
        | x :: xs ->
            match Map.tryFind k x with
            | Some v -> v
            | None -> Scope<_, _>.get xs k
        | [] -> failwithf "%A is not in scope" k
    static member add scope k v =
        match scope with
        | x :: xs -> (Map.add k v x) :: xs
        | _ -> failwith "fixme: empty scope"
    static member nest scope =
        Map.empty<'a, 'b> :: scope
    static member unnest scope =
        match scope with
        | x :: [] -> failwith "can't unnest root, fool"
        | x :: xs -> xs
        | [] -> failwith "fixme: empty scope"
