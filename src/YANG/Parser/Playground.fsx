// Playground.fsx
// Testing of parser functionality during development.

#load @"../../../.paket/load/net471/FParsec-Big-Data-Edition.fsx"
#load @"../../../.paket/load/net471/NLog.fsx"

#load "Errors.fs"
#load "Comments.fs"
#load "Identifier.fs"
#load "Parser.fs"

open System.IO
open System.Text
open FParsec
open Yang.Parser

let sample_dir = Path.Combine(__SOURCE_DIRECTORY__, @"../Examples/")
Directory.Exists(sample_dir)

let example = Path.Combine(sample_dir, @"RFC7950/example-system.yang")

let ReadAndClean filename =
    use reader = new StreamReader(example)
    let sb = StringBuilder()
    use writer = new StringWriter(sb)
    Comments.Remove(reader, writer)
    sb.ToString()


let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) ->
        printfn "Failure: %s" errorMsg

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        reply

type TextState =
| Normal
| SingleQuotedString
| DoubleQuotedString
| Escaped

let unparsed_block_literal<'a> : Parser<string, 'a> =
    let mutable opened = 0
    let mutable state : TextState = Normal

    let closing c : bool =
        match state, c with
        | SingleQuotedString, '\''  -> state <- Normal; true
        | SingleQuotedString, _     -> true
        | DoubleQuotedString, '"'   -> state <- Normal; true
        | DoubleQuotedString, '\\'  -> state <- Escaped; true
        | DoubleQuotedString, _     -> true
        | Escaped, _                -> state <- DoubleQuotedString; true
        | Normal, '\''              -> state <- SingleQuotedString; true
        | Normal, '"'               -> state <- DoubleQuotedString; true
        | Normal, '}'               ->
            if opened = 0 then
                false
            else
                opened <- opened - 1
                true
        | Normal, _else             -> true

    let normalChar = satisfy closing
    manyChars normalChar


type Statement =
| Unparsed of string

type Module =
    {
        Name : Identifier.Identifier
        Statement : Statement list
    }

let module_parser<'a> : Parser<Module, 'a> =
    let parser =
        spaces >>. skipStringCI "module" >>. spaces >>.
        Identifier.parse_identifier .>> spaces .>>
        skipChar '{' .>> spaces .>>.
        unparsed_block_literal .>> spaces .>>
        skipChar '}' .>> spaces
    parser |>> (
        fun (identifier, block) ->
            {
                Name = identifier
                Statement = [ Unparsed block ]
            }
    )

let model = ReadAndClean example
let parsed = test module_parser model
