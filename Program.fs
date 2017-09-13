// Learn more about F# at http://fsharp.org

module Flox

open System
open System.IO

let run code =
    let tokens = Scanner.scan code
    for token in tokens do
        printfn "%A" token

let runFile fname =
    let data = File.ReadAllText(fname)
    run(data)

let rec runPrompt () =
    printf "> "
    let code = Console.ReadLine ()
    if code <> "q" then
        run(code)
        runPrompt()
    else
        ()

[<EntryPoint>]
let main argv =
    if argv.Length > 0 then
        runFile argv.[0]
    else
        runPrompt ()
    0 // return an integer exit code
