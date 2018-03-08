// Generic.fs
// Provides definitions that track the statements defined in the YANG model.
namespace Yang.Model

/// Some utilities for processing a YANG model.
/// Mostly used for searching through the model and other exploratory / debugging activities.
module Generic =

    /// Generic model for representing a YANG statement.
    /// This should be used for debugging / light processing of a model.
    /// It is not used by the model
    type Statement = {
        /// Keyword used by the statement
        Keyword : string

        /// Argument of the statement (if available)
        Argument : string option

        /// List of sub-statements (if available)
        Body: (Statement list) option
    }

    /// A short representation of the identity of a generic statement
    [<StructuredFormatDisplay("{AsString}")>]
    type StatementLabel = | StatementLabel of Keyword:string * Argument:(string option)
    with
        member this.AsString =
            let (StatementLabel (keyword, argument)) = this
            match argument with
            | None      -> keyword
            | Some arg  -> sprintf "%s %s" keyword arg

        override this.ToString() = this.AsString

    /// The unique id of the statement in the model
    [<StructuredFormatDisplay("{AsString}")>]
    type StatementId    = | StatementId of (StatementLabel list)
    with
        member this.AsString =
            let (StatementId path) = this
            path
            |> List.map (
                fun (StatementLabel (keyword, argument)) ->
                    match argument with
                    | Some arg  -> sprintf "%s %s" keyword arg
                    | None      -> sprintf "%s" keyword
            )
            |> String.concat " -> "

        override this.ToString() = this.AsString

    /// <summary>
    /// Find all statements of a YANG statement and its descendants that satisfy a condition.
    /// </summary>
    /// <param name="model">The model to search</param>
    /// <param name="filter">The filter to apply to the statements</param>
    let FindAllNodes (model : Statement, filter : Statement -> bool) : StatementId list =
        // We do not use tail recursion below, because in a couple of cases
        // (although cases with bugs) the search failed with memory exhaustion.
        // The version below worked on those cases, and it should be ok, because
        // the size of the stack will be limited in typical YANG models.
        let rec find (StatementId path) (node : Statement) : StatementId list =
            let label = (StatementLabel (node.Keyword, node.Argument)) :: path |> StatementId
            let current = if filter node then [ label ] else []
            let children =
                match node.Body with
                | None -> []
                | Some children -> children |> List.collect (fun c -> find label c)

            children @ current

        let result = find (StatementId []) model
        result |> List.map (fun r -> let (StatementId t) = r in (List.rev t |> StatementId))

    /// Helper definitions of types
    type Filter =
        /// <summary>
        /// Create a filter that matches a statement by keyword and argument
        /// </summary>
        /// <param name="keyword">The keyword to match</param>
        /// <param name="argument">The argument to match</param>
        static member Make (keyword : string, argument : string option) =
            fun (node : Statement) -> node.Keyword.Equals(keyword) && (node.Argument = argument)

    /// <summary>
    /// Retrieves all keywords used in a YANG statement and its descendants
    /// </summary>
    /// <param name="model">The root statement of the model</param>
    let rec Keywords (model : Statement) : string seq = seq {
        yield model.Keyword

        match model.Body with
        | None      -> ()
        | Some body ->
            for statement in body do
                yield! (Keywords statement)
    }

    /// <summary>
    /// Get the usage of keywords in a YANG statement and its descendants
    /// </summary>
    /// <param name="model">The root statement</param>
    let KeywordUsage (model : Statement) =
        Keywords model
        |> Seq.groupBy id
        |> Seq.map (
            fun (id, uses) ->
                id, Seq.length uses
        )
        |> Seq.sortWith (
            fun (keyword1, _) (keyword2, _) ->
                if keyword1.IndexOf(':') >= 0   && keyword2.IndexOf(':') >= 0   then
                    keyword1.CompareTo(keyword2)
                elif keyword1.IndexOf(':') >= 0 && (keyword2.IndexOf(':') < 0)  then
                    1
                elif keyword1.IndexOf(':') < 0  && keyword2.IndexOf(':') > 0    then
                    -1
                else
                    keyword1.CompareTo(keyword2)
        )
        |> Seq.toList
