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

    type ResolvedTypeDef = TypeDefStatement

    type Node = | Node of Label:Identifier * Path:(Identifier list)
    with
        static member Make (identifier : Identifier) =
            Node (identifier, [ identifier ])

        member this._Path = let (Node (_, path)) = this in path

        member this.Push (identifier : Identifier) =
            Node (identifier, identifier :: this._Path)

        member this.Pop () =
            let rest = List.tail this._Path
            let current = List.head rest
            Node (current, rest)


    let VisitDefinitions (filter : Statement -> bool) (root : Statement) =
        let get (node : Node) (statement : Statement) =
            printfn "Visiting %A" node
            match statement with
            | StatementHelper.Patterns.Container (id, _) ->
                if filter statement then
                    let node' = node.Push id
                    Some (node', statement)
                else None
            | StatementHelper.Patterns.TypeDef (id, _) ->
                if filter statement then
                    let node' = node.Push id
                    Some (node', statement)
                else None
            | StatementHelper.Patterns.Grouping (id, _) ->
                if filter statement then
                    let node' = node.Push id
                    Some (node', statement)
                else None
            | _ -> None

        let rec find (work : (Node * Statement) list) (results : (Node * Statement) list) =
            match work with
            | [] -> results
            | (node, statement) :: tl ->
                let inner = StatementHelper.Children statement |> List.choose (get node)
                find (inner @ tl) (results @ inner)

        let id = StatementHelper.GetIdentifier root
        let node = Node.Make id.Value
        let inner = StatementHelper.Children root |> List.choose (get node)
        find inner inner

    let xx = VisitDefinitions (fun _ -> true) (Statement.Module configuration)
    xx.[6]


    type Resolver = {
        TypeDefs    : Map<Identifier, Type>
        Containers  : Map<Identifier, ContainerStatement>
        Parent      : Resolver option
        Children    : Map<Identifier, Resolver>
    }

