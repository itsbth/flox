// Learn more about F# at http://fsharp.org

module Flox

open System
open System.IO

let run code scope =
    let tokens = Scanner.scan code
    for token in tokens do
        printfn "%A" token
    let (ast, rest) = Parser.statement (tokens |> List.map fst)
    printfn "%A" ast
    let value, scope = Evaluator.execute ast scope
    match value with
    | Some value -> printfn "= %A" value
    | None -> ()
    scope

let runFile fname =
    let data = File.ReadAllText(fname)
    run data |> ignore
    ()

let runPrompt () =
    let rec runPrompt' scope =
        printf "> "
        let code = Console.ReadLine ()
        if code <> null && code <> "q" then
            let scope = run code scope
            runPrompt' scope
        else
            ()
    runPrompt' Map.empty

[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        runFile argv.[0]
    else
        runPrompt ()
    0 // return an integer exit code
