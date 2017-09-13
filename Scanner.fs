module Scanner

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

type EmittedToken = (Token * int)

let scan (code : string) =
    let rec scan' (code : char list) pos =
        seq {
            match code with 
            | '=' :: '=' :: rest -> yield (EQUAL_EQUAL, pos); yield! scan' rest (pos + 2)
            | '=' :: rest -> yield (EQUAL, pos); yield! scan' rest (pos + 1)
            | ' ' :: rest -> yield! scan' rest (pos + 1)
            | _ :: rest -> yield (STRING("???"), pos); yield! scan' rest (pos + 1)
            | [] -> yield (EOF, pos)
        }
    scan' (List.ofArray (code.ToCharArray ())) 0
