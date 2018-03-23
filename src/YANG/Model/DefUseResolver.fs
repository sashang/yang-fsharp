// DefUseResolver.fs
// Collects and processes definition and use statements for types and groupings.
namespace Yang.Model

/// Methods for detecting and processing definition and use statements for types and groupings
module DefUseResolver =
    open StatementHelper.Patterns

    // TODO: Deal with module prefixes
    // TODO: Deal with imports and includes

    [<StructuredFormatDisplay("{AsString}")>]
    type NodeType =
    | TypeUse of IdentifierReference
    | TypeDefinition
    | GroupingUse of IdentifierReference
    | GroupingDefinition
    with
        member this.AsString =
            match this with
            | TypeUse      ``type`` -> sprintf "type use: %s" ``type``.Value
            | TypeDefinition        -> "type definition"
            | GroupingUse  ``type`` -> sprintf "grouping use: %s" ``type``.Value
            | GroupingDefinition    -> "grouping definition"

        override this.ToString() = this.AsString

    [<StructuredFormatDisplay("{AsString}")>]
    type Path = | Path of (IdentifierReference list)
    with
        static member Empty = Path []
        static member Make (identifier : Identifier) =
            Path [ IdentifierReference.Make identifier ]
        static member Make (identifier : IdentifierReference) =
            Path [ identifier ]
        static member Make (identifier : IdentifierWithPrefix) =
            Path [ IdentifierReference.Make identifier ]

        member this._Path = let (Path path) = this in path
        member this._Head = let (Path path) = this in List.head path

        member this.Push (identifier : Identifier) =
            Path ((IdentifierReference.Make identifier) :: this._Path)

        member this.Push (identifier : IdentifierReference) =
            Path (identifier :: this._Path)

        member this.Push (identifier : IdentifierWithPrefix) =
            Path ((IdentifierReference.Make identifier) :: this._Path)

        member this.Pop () =
            let rest = List.tail this._Path
            Path rest

        member this.AsString =
            let path = this._Path |> List.rev |> List.map (fun p -> p.Value) |> String.concat "/"
            sprintf "/%s" path

        override this.ToString() = this.AsString

    [<StructuredFormatDisplay("{AsString}")>]
    type Node = | Node of Path:Path * Type:(NodeType option)
    with
        static member Make (identifier : Identifier, ``type``: NodeType) =
            Node (Path.Make identifier, Some ``type``)

        member this._Path = let (Node (path, _)) = this in path
        member this._Head = let (Node (path, _)) = this in path._Head
        member this._Type = let (Node (_, ``type``)) = this in ``type``

        member this.Push (identifier : Identifier, ``type`` : NodeType) =
            Node (this._Path.Push identifier, Some ``type``)

        member this.Push (identifier : IdentifierReference, ``type`` : NodeType) =
            Node (this._Path.Push identifier, Some ``type``)

        member this.Push (identifier : IdentifierWithPrefix, ``type`` : NodeType) =
            Node (this._Path.Push identifier, Some ``type``)

        member this.Push (identifier : Identifier) =
            Node (this._Path.Push identifier, None)

        member this.Push (identifier : IdentifierReference) =
            Node (this._Path.Push identifier, None)

        member this.Push (identifier : IdentifierWithPrefix) =
            Node (this._Path.Push identifier, None)

        member this.AsString =
            let (Node (path, ``type``)) = this
            match ``type`` with
            | None      -> sprintf "%s [-]" path.AsString
            | Some t    -> sprintf "%s [%s]" path.AsString t.AsString


    let VisitDefinitions (filter : Statement -> bool) (root : Statement) : Node list=
        let get (path : Path) (statement : Statement) : Node option =
            // printfn "Visiting %A" path
            match statement with
            | TypeDef (id, _) ->
                if filter statement then
                    let path' = path.Push id
                    Node (path', Some TypeDefinition) |> Some
                else None
            | Type (id, _) ->
                if filter statement then
                    Node (path, Some (TypeUse id)) |> Some
                else None
            | Grouping (id, _) ->
                if filter statement then
                    let path' = path.Push id
                    Node (path', Some GroupingDefinition) |> Some
                else None
            | Uses (id, _) ->
                if filter statement then
                    Node (path, Some (GroupingUse id)) |> Some
                else None
            | _ -> None

        let rec find (work : (Path * Statement) list) (results : Node list) =
            match work with
            | [] -> results
            | (path, statement) :: tl ->
                if filter statement then
                    let inner = StatementHelper.Children statement
                    match StatementHelper.GetReferenceIdentifier statement with
                    | None  ->
                        // Statement without a label. These ones typically don't have sub-statements.
                        // Ignore them for now.
                        // TODO: Name resolution for statements without labels
                        find tl results
                    | Some id ->
                        let path' = path.Push id
                        let extra_work = inner |> List.map (fun work -> path', work)
                        match get path statement with
                        | None      -> find (extra_work @ tl) results
                        | Some v    -> find (extra_work @ tl) (results @ [v])
                else
                    find tl results

        find [ (Path.Empty, root) ] []

