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
#load "DefUseResolver.fs"
#load "Generator.fs"

open System
open Yang.Model
open Generator

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


let xx = DefUseResolver.VisitDefinitions (fun _ -> true) (Statement.Module configuration)

open System.Text
open System.Collections.Generic

type TypeResolution =
| TString
| Unknown

let (|BuildInType|_|) (identifier : IdentifierReference) =
    if identifier.Value.Equals("string") then Some TString
    else None


type TypeDefinitions = | TypeDefinitions of Definitions:Dictionary<IdentifierReference, TypeResolution> * Parent:(TypeDefinitions option)
with
    static member private create () = Dictionary<IdentifierReference, TypeResolution>()

    static member Empty = TypeDefinitions (TypeDefinitions.create(), None)
    static member Make(?parent : TypeDefinitions) =
        let definitions = Dictionary<IdentifierReference, TypeResolution>()
        TypeDefinitions (definitions, parent)

    member private this._Definitions = let (TypeDefinitions (definitions, _)) = this in definitions
    member private this._Parent = let (TypeDefinitions (_, parent)) = this in parent

    member this.Search (identifier : IdentifierReference) =
        if this._Definitions.ContainsKey(identifier) then Some (this._Definitions.Item identifier)
        elif this._Parent.IsNone then None
        else this._Parent.Value.Search(identifier)

    member this.Search (identifier : Identifier) = this.Search (Simple identifier)

    member this.Add (identifier : IdentifierReference, resolved) = this._Definitions.Add(identifier, resolved)
    member this.Add (identifier : Identifier, resolved) = this._Definitions.Add(Simple identifier, resolved)

    member this.Push () = TypeDefinitions (TypeDefinitions.create (), Some this)
    member this.Pop  () = this._Parent.Value


open StatementHelper.Patterns

type CSharpGenerator (``namespace`` : string, root : ModuleStatement) =
    let sb = StringBuilder()
    let indent = "    "
    let mutable indentation = 0
    let mutable definitions = TypeDefinitions.Empty

    let indent ()   = Printf.bprintf sb "%s" (String.replicate indentation indent)
    let push ()     = indentation <- indentation + 1
    let pop ()      = indentation <- indentation - 1
    let nl ()       = Printf.bprintf sb "\n"
    let bb ()       = Printf.bprintf sb " {"; nl(); push ()
    let eb ()       = pop (); indent(); Printf.bprintf sb "}"; nl()

    member this.Append (statement : ContainerStatement) =
        let identifier, body = statement
        nl ()
        indent ()
        Printf.bprintf sb "class %s" identifier.Value
        bb ()
        let definitions' = definitions.Push()
        definitions <- definitions'
        if body.IsSome then
            body.Value |> List.map ContainerBodyStatement.Translate |> List.iter this.Append
        eb ()

    member this.Append (statement : ModuleStatement) =
        let children = StatementHelper.Children (Module statement)
        children |> List.iter this.Append

    member this.Append (statement : TypeDefStatement) =
        let identifier, body = statement
        let ty = body |> List.map TypeDefBodyStatement.Translate |> List.choose ``|Type|_|``
        match ty with
        | []        -> failwith (sprintf "Cannot find type for typedef %s" identifier.Value)
        | [ ty ]    ->
            let (using_type, _, _) = ty
            match using_type with
            | BuildInType resolution -> definitions.Add(identifier, resolution)
            | _     -> failwith (sprintf "Don't know how to handle type %s" using_type.Value)
        | _         -> failwith (sprintf "Found multiple types for typedef %s" identifier.Value)
        ()

    member this.Append (statement : Statement) =
        match statement with
        | Statement.Container     st    -> this.Append st
        | Statement.TypeDef       st    -> this.Append st
        | _ ->
            printfn "Not parsing %s" (Statement.Keyword statement)

    member this.Evaluate () =
        Printf.bprintf sb "namespace %s" ``namespace``
        bb()
        this.Append root
        eb()
        sb.ToString()

    member this.Read () = sb.ToString()

    static member Evaluate(``namespace`` : string, root : ModuleStatement) =
        let generator = CSharpGenerator(``namespace``, root)
        generator.Evaluate()

let program = CSharpGenerator.Evaluate("Juniper", configuration)
printfn "\n%s\n" program
