// DefUseResolver.fs
// Collects and processes definition and use statements for types and groupings.
namespace Yang.Model

/// Methods for detecting and processing definition and use statements for types and groupings
module DefUseResolver =
    open System.Collections.Generic
    open StatementHelper.Patterns

    // TODO: Deal with module prefixes
    // TODO: Deal with imports and includes

    type IdUse = IdentifierReference * (int option)
    type IdDef = IdentifierReference * int

    let private mkIdUse id = id, None
    let private mkIdDef id number = id, number

    [<StructuredFormatDisplay("{AsString}")>]
    type NodeType =
    | TypeUse               of IdUse
    | TypeDefinition        of IdDef
    | GroupingUse           of IdUse
    | GroupingDefinition    of IdDef
    with
        member this.AsString =
            match this with
            | TypeUse               (ty, None)          ->
                sprintf "type use: %s[?]" ty.Value
            | TypeUse               (ty, Some unique)   ->
                sprintf "type use: %s[%d]" ty.Value unique
            | TypeDefinition        (_, unique)         -> 
                sprintf "type definition [%d]" unique
            | GroupingUse           (ty, None)          ->
                sprintf "grouping use: %s[?]" ty.Value
            | GroupingUse           (ty, Some unique)   ->
                sprintf "grouping use: %s[%d]" ty.Value unique
            | GroupingDefinition    (_, unique)         ->
                sprintf "grouping definition [%d]" unique

        override this.ToString() = this.AsString

        member this._IsTypeUse            = match this with | TypeUse            _ -> true | _ -> false
        member this._IsTypeDefinition     = match this with | TypeDefinition     _ -> true | _ -> false
        member this._IsGroupingUse        = match this with | GroupingUse        _ -> true | _ -> false
        member this._IsGroupingDefinition = match this with | GroupingDefinition _ -> true | _ -> false

        member this.AsTypeUse            = match this with | TypeUse            v -> Some v | _ -> None
        member this.AsGroupingUse        = match this with | GroupingUse        v -> Some v | _ -> None

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
        // This version is much faster than using a work list, collection of results,
        // and tail recursion.

        let definitions = Dictionary<IdentifierReference, int>()
        let getUnique (id : IdentifierReference) =
            if definitions.ContainsKey(id) then
                let unique = definitions.[id] + 1
                definitions.[id] <- unique
                unique
            else
                definitions.Add(id, 1)
                1

        let mkTypeDefinition (id : Identifier) =
            let id = IdentifierReference.Make id
            let unique = getUnique id
            Some (TypeDefinition (mkIdDef id unique))

        let mkTypeUse (id : IdentifierReference) =
            Some (TypeUse (id, None))

        let mkGroupingDefinition (id : Identifier) =
            let id = IdentifierReference.Make id
            let unique = getUnique id
            Some (GroupingDefinition (id, unique))

        let mkGroupingUse (id : IdentifierReference) =
            Some (GroupingUse (id, None))

        let get (path : Path) (statement : Statement) : Node option =
            // printfn "Visiting %A" path
            match statement with
            | TypeDef (TypeDefStatement (id, _)) ->
                if filter statement then
                    let path' = path.Push id

                    Node (path', mkTypeDefinition id) |> Some
                else None
            | Type (TypeStatement (id, _)) ->
                if filter statement then
                    Node (path, mkTypeUse id) |> Some
                else None
            | Grouping (GroupingStatement (id, _)) ->
                if filter statement then
                    let path' = path.Push id
                    Node (path', mkGroupingDefinition id) |> Some
                else None
            | Uses (UsesStatement (id, _)) ->
                if filter statement then
                    Node (path, mkGroupingUse id) |> Some
                else None
            | _ -> None

        let rec find (path : Path) (statement : Statement) =
            if filter statement then
                match StatementHelper.GetReferenceIdentifier statement with
                | None  ->
                    // Statement without a label. These ones typically don't have sub-statements.
                    // Ignore them for now.
                    // TODO: Name resolution for statements without labels
                    []
                | Some id ->
                    let path' = path.Push id

                    let active = get path statement

                    let inner = StatementHelper.Children statement
                    if active.IsNone then
                        inner |> List.collect (
                            fun child ->
                                find path' child
                        )
                    else
                        active.Value :: (
                            inner |> List.collect (
                                fun child ->
                                    find path' child
                            )
                        )
            else
                []

        find Path.Empty root

