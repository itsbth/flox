module Parser

let expression tokens =
    match tokens with
    | x :: xs ->
        match x with
        | NUMBER n -> Some (Literal n)
    | [] -> None

