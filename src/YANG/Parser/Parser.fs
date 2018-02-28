﻿namespace Yang.Parser

[<AutoOpen>]
module Parser =
    open System.IO
    open System.Text
    open FParsec

    /// <summary>
    /// Reads a YANG model from a file and removes all comments.
    /// (It does NOT parse the statements --- just reads the file)
    /// </summary>
    /// <param name="filename">The input filename</param>
    /// <returns>The contents of the file with all comments removed</returns>
    let ReadAndClean (filename : string) : string =
        use reader = new StreamReader(filename)
        let sb = StringBuilder()
        use writer = new StringWriter(sb)
        Comments.Remove(reader, writer)
        sb.ToString()

    let MakeFromString (model : string) =
        let model' = Comments.Remove model
        if System.String.IsNullOrWhiteSpace(model') then
            raise (YangParserException "Input model cannot be empty")

        match (run  Module.parse_module model') with
        | Success (result, _, _)    -> result
        | Failure (message, _, _)   ->
            raise (YangParserException (sprintf "Failed to parse model, error: %s" message))

