// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load @"../../../.paket/load/net471/NLog.fsx"

#load "Errors.fs"
#load "Generic.fs"
#load "Identifier.fs"
#load "Arguments.fs"
#load "Expressions.fs"
#load "Statements.fs"
#load "StatementHelper.fs"
#load "Printer.fs"
#load "Generator.fs"

open System
open Yang.Model




open Generator

let address4 =
    mkList "address" (
        [
            mkLeaf "name" "ipv4prefix" |> ListBodyStatement.Leaf
        ]
    ) |> ContainerBodyStatement.List

Printer.StatementToStringCompact (ContainerBodyStatement.Translate address4)

let family =
    mkContainer "family" (
        Some [
            mkContainerInternal "inet" (
                Some [
                    mkPresence "enable inet"
                    mkList "address" (
                        [
                            mkLeaf "name" "ipv4prefix" |> ListBodyStatement.Leaf
                        ]
                    ) |> ContainerBodyStatement.List
                ])

            mkContainerInternal "inet6" (
                Some [
                    mkPresence "enable inet6"
                    mkList "address" (
                        [
                            mkLeaf "name" "ipv4prefix" |> ListBodyStatement.Leaf
                        ]
                    ) |> ContainerBodyStatement.List
                ])
        ])

let configuration =
    mkModule "configuration"
        [
            mkTypeDefFromString "interface-unit"
            mkTypeDefFromString "ipv4prefix"
            mkTypeDefFromString "ipv6prefix"
            mkContainer "configuration" (
                Some [
                    mkLeaf "version" "string" |> ContainerBodyStatement.Leaf

                    mkContainerInternal "system" (
                        Some [
                            mkContainerInternal "interfaces" (
                                Some [
                                    mkContainerInternal "interface" (
                                        Some [
                                            mkLeaf "name" "interface-unit" |> ContainerBodyStatement.Leaf
                                            mkLeaf "description" "string" |> ContainerBodyStatement.Leaf

                                            mkList "unit" (
                                                [
                                                    mkLeaf "unit" "string" |> ListBodyStatement.Leaf
                                                    mkLeaf "description" "string" |> ListBodyStatement.Leaf
                                                    family |> ListBodyStatement.Container
                                                ]) |> ContainerBodyStatement.List
                                        ])
                                ])
                        ])
                ])  |> BodyStatement.Container
        ]

let m = Printer.ModuleToStringCompact configuration
printfn "\n%s\n" m

StatementHelper.CountDescendants (Statement.Container family)
StatementHelper.Children (Statement.Module configuration)
StatementHelper.CountDescendants (Statement.Module configuration)

Printer.StatementToStringCompact (Statement.Container family)








module ResolveType =

    open System.Collections.Specialized
    open System

    // [RFC 7950, p. 24]
    // Name                 Description                         
    //==========================================================
    // binary               Any binary data                     
    // bits                 A set of bits or flags              
    // boolean              "true" or "false"                   
    // decimal64            64=bit signed decimal number        
    // empty                A leaf that does not have any value 
    // enumeration          One of an enumerated set of strings 
    // identityref          A reference to an abstract identity 
    // instance=identifier  A reference to a data tree node     
    // int8                 8=bit signed integer                
    // int16                16=bit signed integer               
    // int32                32=bit signed integer               
    // int64                64=bit signed integer               
    // leafref              A reference to a leaf instance      
    // string               A character string                  
    // uint8                8=bit unsigned integer              
    // uint16               16=bit unsigned integer             
    // uint32               32=bit unsigned integer             
    // uint64               64=bit unsigned integer             
    // union                Choice of member types              
    //
    // Example of 'bits' ([RFC 7950, p. 176]):
    //type bits {
    //    bit UP;
    //    bit PROMISCUOUS
    //    bit DISABLED;
    //}

module Resolver =
    open FSharp.Collections

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
            | StatementHelper.Patterns.TypeDef (id, _) ->
                if filter statement then
                    let path' = path.Push id
                    Node (path', Some TypeDefinition) |> Some
                else None
            | StatementHelper.Patterns.Type (id, _, _) ->
                if filter statement then
                    Node (path, Some (TypeUse id)) |> Some
                else None
            | StatementHelper.Patterns.Grouping (id, _) ->
                if filter statement then
                    let path' = path.Push id
                    Node (path', Some GroupingDefinition) |> Some
                else None
            | StatementHelper.Patterns.Uses (id, _) ->
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

    let xx = VisitDefinitions (fun _ -> true) (Statement.Module configuration)
    xx.[6]


    type Resolver = {
        TypeDefs    : Map<Identifier, Type>
        Containers  : Map<Identifier, ContainerStatement>
        Parent      : Resolver option
        Children    : Map<Identifier, Resolver>
    }

