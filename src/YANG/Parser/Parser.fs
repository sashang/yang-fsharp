namespace Yang.Parser

[<AutoOpen>]
module Parser =
    open System.IO
    open System.Text
    open FParsec
    open Yang.Model.Statements

    let Initialize () =
        GenericParser.initialize ()

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

    let MakeFromString (model : string) : ModuleStatement =
        let model' = (Comments.Remove model).Trim()
        if System.String.IsNullOrWhiteSpace(model') then
            raise (YangParserException "Input model cannot be empty")

        match (run  Module.parse_module model') with
        | Success (result, _, _)    -> result
        | Failure (message, _, _)   ->
            raise (YangParserException (sprintf "Failed to parse model, error: %s" message))

    let ParseFile (filename : string) =
        let content = ReadAndClean filename
        // TODO: Do we need to call the generic statement initializer here?
        GenericParser.initialize()
        apply_parser (wse >>. Module.parse_module_or_submodule) content
