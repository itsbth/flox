module Scanner

open System

type Token = 
    | EOF
    | LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    | IDENTIFIER of string
    | STRING of string
    | NUMBER of double
    | AND
    | CLASS
    | ELSE
    | FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | TRUE
    | VAR
    | WHILE
    | UNK of char

type EmittedToken = Token * int

type State = char list * int

let private string_of_acc = 
    (List.rev
     >> List.toArray
     >> String)

let scan (code : string) = 
    let rec scan' (code : char list) pos acc = 
        let rec comment (code, pos) = 
            match code with
            | '\n' :: rest -> (rest, pos)
            | _ :: rest -> comment (rest, (pos + 1))
            | [] -> ([], pos)
        
        let string (code, pos) = 
            let rec string' (code, pos) acc = 
                match code with
                | '"' :: rest -> (rest, pos, acc)
                | '\\' :: '"' :: rest -> string' (rest, pos + 2) ('"' :: acc)
                | '\\' :: '\\' :: rest -> string' (rest, pos + 2) ('\\' :: acc)
                | ch :: rest -> string' (rest, pos + 1) (ch :: acc)
                | [] -> failwith "eof found while parsing string"
            
            let (rest, pos, content) = string' (code, pos) []
            (content |> string_of_acc, rest, pos)
        
        let number (code, pos) = 
            let rec number' (code, pos) acc = 
                match code with
                | '.' as ch :: rest | ch :: rest when Char.IsDigit(ch) -> number' (rest, pos + 1) (ch :: acc)
                | _ :: rest -> (code, pos, acc)
                | [] -> ([], pos, acc)
            
            let (rest, pos, num) = number' (code, pos) []
            (num
             |> string_of_acc
             |> Double.Parse, rest, pos)
        
        let word (code, pos) = 
            let rec word' (code, pos) acc = 
                match code with
                | ch :: rest when Char.IsLetterOrDigit(ch) -> word' (rest, pos + 1) (ch :: acc)
                | _ -> (code, pos, acc)
            
            let word_of_acc acc = 
                match acc |> string_of_acc with
                | "var" -> VAR
                | "print" -> PRINT
                | other -> IDENTIFIER other
            
            let (rest, pos, str) = word' (code, pos) []
            (str |> word_of_acc, rest, pos)
        
        // helper functions for single/double char tokens
        let inline single rest token = scan' rest (pos + 1) ((token, pos) :: acc)
        let inline double rest token = scan' rest (pos + 2) ((token, pos) :: acc)
        match code with
        | '(' :: rest -> single rest LEFT_PAREN
        | ')' :: rest -> single rest RIGHT_PAREN
        | '{' :: rest -> single rest LEFT_BRACE
        | '}' :: rest -> single rest RIGHT_BRACE
        | ',' :: rest -> single rest COMMA
        | '.' :: rest -> single rest DOT
        | '-' :: rest -> single rest MINUS
        | '+' :: rest -> single rest PLUS
        | ';' :: rest -> single rest SEMICOLON
        | '*' :: rest -> single rest STAR
        | '=' :: '=' :: rest -> double rest EQUAL_EQUAL
        | '=' :: rest -> single rest EQUAL
        | '!' :: '=' :: rest -> double rest BANG_EQUAL
        | '!' :: rest -> single rest BANG
        | '<' :: '=' :: rest -> double rest LESS_EQUAL
        | '<' :: rest -> single rest LESS
        | '>' :: '=' :: rest -> double rest GREATER_EQUAL
        | '>' :: rest -> single rest GREATER
        | '/' :: '/' :: rest -> 
            let (rest, pos) = comment (rest, pos)
            scan' rest pos acc
        | '/' :: rest -> single rest SLASH
        | '"' :: rest -> 
            let (content, rest, pos) = string (rest, pos)
            scan' rest pos ((STRING content, pos) :: acc)
        | ch :: rest when Char.IsDigit(ch) -> 
            let (num, rest, pos) = number (code, pos)
            scan' rest pos ((NUMBER num, pos) :: acc)
        | ' ' :: rest | '\t' :: rest | '\n' :: rest -> scan' rest (pos + 1) acc
        | ch :: rest when Char.IsLetter(ch) -> 
            let (token, rest, pos) = word (code, pos)
            scan' rest pos ((token, pos) :: acc)
        | ch :: rest -> single rest (UNK ch)
        | [] -> (EOF, pos) :: acc
    scan' (List.ofArray (code.ToCharArray())) 0 [] |> List.rev
