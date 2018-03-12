// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load @"../../../.paket/load/net471/NLog.fsx"

#load "Errors.fs"
#load "Generic.fs"
#load "Identifier.fs"
#load "Arguments.fs"
#load "Expressions.fs"
#load "Statements.fs"

open System
open Yang.Model

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

module NameResolution =
    open FSharp.Collections

    type ResolvedTypeDef = TypeDefStatement

    type Node = | Node of Label:Identifier * Path:(Identifier list)

    type Resolver = {
        TypeDefs    : Map<Identifier, Type>
        Containers  : Map<Identifier, ContainerStatement>
        Parent      : Resolver option
        Children    : Map<Identifier, Resolver>
    }

    let rec resolve (db : Resolver) (id : Identifier) : Type option =
        if Map.containsKey id db.Definitions then
            Map.find id db.Definitions |> Some
        elif db.Parent.IsSome then
            resolve db.Parent.Value id
        else None


let mkId id = Identifier.Make id
let mkIdRef id = IdentifierReference.Make id

let mkModule name body : ModuleStatement =
    let version : YangVersionStatement = Version(1, 1), None
    let ns : NamespaceStatement = Uri("yang://unknown"), None
    let prefix : PrefixStatement = "this", None
    let header : ModuleHeaderStatements = version, ns, prefix, None
    {
        Name        = mkId name
        Header      = header
        Linkage     = []
        Meta        = []
        Revision    = []
        Body        = body
    }

let mkContainer name body = ((mkId name), body)
let mkContainerInternal name body = ((mkId name), body) |> ContainerBodyStatement.Container
let mkType name body : TypeStatement = (mkIdRef name), body, None

let mkTypeDefFromString name : BodyStatement =
    let body : TypeDefBodyStatement = mkType "string" None |> TypeDefBodyStatement.Type
    (mkId "name", [ body ]) |> BodyStatement.TypeDef

let mkLeaf name ``type`` : LeafStatement =
    let t = LeafBodyStatement.Type (mkIdRef ``type``, None, None)
    let leaf : LeafStatement = mkId ``type``, [ t ]
    leaf

let mkList name body : ListStatement = mkId name, body
let mkPresence label = ContainerBodyStatement.Presence (label, None)

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
                                            mkLeaf "description" "uint32" |> ContainerBodyStatement.Leaf

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

let sb = System.Text.StringBuilder()
Statement.print (sb, 0) (configuration |> Statement.Module)
sb.ToString()

open System.Text
type YangPrinter (?sb : StringBuilder, ?indentation : int, ?indent : string) as this =
    let sb = defaultArg sb (System.Text.StringBuilder ())
    let indent = defaultArg indent "\t"
    let mutable indentation = defaultArg indentation 0

    let indent () = Printf.bprintf sb "%s" (String.replicate indentation "\t")
    let nl ()     = Printf.bprintf sb "\n"; indent()

    member __.Append (statement : ModuleStatement) =
        Printf.bprintf sb "module %s {" statement.Name.Value
        indentation <- indentation + 1
        nl()

        let version, ns, prefix, unknowns = statement.Header
        this.Append version
        this.Append ns
        this.Append prefix
        this.Append unknowns

        indentation <- indentation - 1
        Printf.bprintf sb "}"
        nl ()

    member __.Append (statement : NamespaceStatement) =
        let uri, extra = statement
        Printf.bprintf sb "\"%s\"" (uri.ToString())
        this.Append extra

    member __.Append (statement : PrefixStatement) =
        let prefix, extra = statement
        Printf.bprintf sb "prefix %s" prefix
        this.Append extra

    member __.Append (statement : YangVersionStatement) =
        let version, extra = statement
        Printf.bprintf sb "yang-version %d.%d" version.Major version.Minor
        this.Append extra

    member __.Append (statement : Statement) =
        match statement with
        | Module st             -> this.Append st; nl ()
        | Namespace st          -> this.Append st; nl ()
        | Statement.Prefix st   -> this.Append st; nl ()
        | YangVersion st        -> this.Append st; nl ()
        | _                     -> Printf.bprintf sb "%A" statement; nl ()

    member __.Append (statement : ExtraStatements) =
        match statement with
        | None              -> Printf.bprintf sb ";"; nl ()
        | Some statements   ->
            Printf.bprintf sb " {"
            indentation <- indentation + 1
            nl ()

            statements |> List.iter (this.Append)

            indentation <- indentation - 1
            Printf.bprintf sb "}"
            nl ()

    member __.Append (statement : UnknownStatement) =
        let id, arg, extra = statement
        Printf.bprintf sb "%s" (id.ToString())
        if arg.IsSome then Printf.bprintf sb " %s" (arg.Value)
        this.Append extra

    member __.Append (unknowns : UnknownStatement list option) =
        match unknowns with
        | None          -> ()
        | Some unknowns ->
            unknowns |> List.iter (this.Append)

    member __.Clear() =
        sb.Clear() |> ignore
        indentation <- 0

    override this.ToString() = sb.ToString()

let yp = YangPrinter ()
yp.Append(configuration)
let m = yp.ToString()
