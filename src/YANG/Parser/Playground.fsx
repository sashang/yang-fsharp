// Playground.fsx
// Testing of parser functionality during development.

#load "Initialize.fsx"

open System.Collections.Generic
open System.Collections.ObjectModel
open System.Text

open FParsec
open Yang.Model
open Yang.Model.Graph
open Yang.Model.DefUse
open Yang.Parser

open Initialize
open Logging

#time

MyLog.myLog.AddTrace(Header._name)
MyLog.myLog.AddTrace(Types._name)
MyLog.myLog.AddTrace(Statements._name)
MyLog.myLog.AddTrace(GenericParser._name)
MyLog.myLog.AddTrace(Deviation._name)

//
// Test code to parse files
//
let model = get_sample_model @"RFC7950/example-system.yang"
let pm = apply_parser Module.parse_module model

let big_model = get_external_model @"Juniper\16.1\configuration.yang"

// This is what we want to parse eventually

let juniper = apply_parser (spaces >>. Module.parse_module) big_model

let very_big_model = get_external_model @"Juniper\16.2\16.2R1\operational\show-ddos-protection-0.yang"
let _x =
    let model = apply_parser (spaces >>. Module.parse_module) very_big_model
    model.Name


[<StructuredFormatDisplay("{AsString}")>]
type Phi () = class
    let phis = new Dictionary<IdentifierReference, List<DefUse.IdDef>>()

    member this.AsString =
        let sb = StringBuilder()
        phis.Keys
        |> Seq.iteri (
            fun i key ->
                if i > 0 then Printf.bprintf sb ", "
                let values = phis.[key]
                Printf.bprintf sb "%s(" key.Value
                values
                |> Seq.iteri (
                    fun i value ->
                        if i > 0 then Printf.bprintf sb ", "
                        let (_, unique) = value
                        Printf.bprintf sb "%03d" unique
                )
                Printf.bprintf sb ")"
        )

        sb.ToString()

    member this.Add (definition : DefUse.IdDef) =
        let (id, _) = definition
        if (phis.ContainsKey id) = false then
            phis.Add(id, System.Collections.Generic.List())

        let l = phis.[id]
        if l.Contains(definition) = false then l.Add(definition)

    member this.Phis = ReadOnlyDictionary(phis)

    override this.ToString() = this.AsString
end

[<StructuredFormatDisplay("{AsString}")>]
type NodeDefinition () = class
    let properties = System.Collections.Generic.List<Node>()

    member this.AsString = properties |> Seq.map (fun p -> p.AsString) |> String.concat ", "
    member this.Add (node : Node) = properties.Add(node)
    member __.Properties = properties

    member __.GroupingUses =
        properties
        |> Seq.choose (fun (Node (_, t)) -> Patterns.``|GroupingUseOption|_|`` t)
        |> Seq.toList

    member __.GroupingUsesResolved =
        properties
        |> Seq.choose (fun (Node (_, t)) -> Patterns.``|GroupingUseResolvedOption|_|`` t)
        |> Seq.toList

    member __.TypeDefinitions =
        properties
        |> Seq.choose (fun (Node (_, t)) -> Patterns.``|TypeDefOption|_|`` t)
        |> Seq.toList

    member __.TryFindTypeDefinition id =
        properties
        |> Seq.tryPick (
            fun (Node (_, definition)) ->
                match definition with
                | Patterns.TypeDefOption (id, seq)  -> Some seq
                | _                                 -> None
        )

    override this.ToString () = this.AsString
end

type PhiGraph  = Graph<Phi>
type DefUseGraphNode = GraphNode<NodeDefinition>
type DefUseGraph = Graph<NodeDefinition>

let AsTypeDefinitionNode (node : DefUseGraphNode) =
    let defs = node.Value.TypeDefinitions
    if defs.Length = 1      then Some defs.Head
    elif defs.Length > 1    then failwith "internal bug: found too many definitions for a node"
    else None

let AsGroupUseNode (node : DefUseGraphNode) =
    let defs = node.Value.GroupingUses
    if defs.Length = 1      then Some defs.Head
    elif defs.Length > 1    then failwith "internal bug: found too many definitions for a group use"
    else None

let get_local_group_uses (node : DefUseGraphNode) : IdDef list = node.Value.GroupingUsesResolved

let try_find_local_type_definition (node : DefUseGraphNode, id : IdentifierReference) =
    printfn "Searching locally %A in %A" id node
    let candidates =
        node.Children
        |> Seq.choose (
            fun (_, node) ->
                match AsTypeDefinitionNode node with
                | Some (_id, seq)   -> if id = _id then Some seq else None
                | _                 -> None
        )
        |> Seq.toList
    if candidates.Length = 1    then
        printfn "Found local definition of %A in %A" id node
        Some candidates.Head
    elif candidates.Length > 1  then
        failwithf "Error: found many candidates:\n%A\n" candidates
    else None



let mkPhiGraph (filter : NodeType -> IdDef option) (nodes : Node list) =
    let add node (phi : Phi) = phi.Add node

    let g = PhiGraph.Empty()
    nodes
    |> List.iter (
        fun (Node (path, definition)) ->
            if definition.IsSome then
                match filter definition.Value with
                | None          -> ()
                | Some grouping ->
                    g.Add(path, add grouping)
            else
                // Do nothing
                ()
    )
    g

let mkGraph (nodes : Node list) : DefUseGraph =
    let add node (n : NodeDefinition) = n.Add node

    let g = DefUseGraph.Empty()

    nodes
    |> List.iter (
        fun (Node (path, _) as node) -> g.Add(path, add node)
    )

    g

let mkPath (path : string list) = path |> List.map IdentifierReference.Make
let mkPathStr (path : string)   = mkPath (path.Split() |> Array.toList)

let resolve_groups (graph : PhiGraph) statements =
    statements
    |> List.map (
        fun (Node (path, definition) as node) ->
            match definition with
            | Patterns.GroupingUseUnresolvedOption id ->
                let starting = graph.FindClosest path
                let resolved = starting.Search id
                match resolved with
                | None          -> failwithf "Failed to resolve symbol: %A" id
                | Some resolved ->
                    if resolved.Phis.ContainsKey id = false then
                        printfn "Key not in dictionay: %A\n%A\n" id resolved.Phis
                    let resolutions = resolved.Phis.Item id
                    if resolutions.Count = 0    then failwith "Failed to find resolution"
                    elif resolutions.Count > 1  then failwith "too many resolutions"
                    else
                        let resolution = resolutions.Item 0
                        let definition' = Some (Groupings.Use resolution)
                        Node (path, definition')

            | _ -> node
    )

[<StructuredFormatDisplay("{AsString}")>]
type Resolution (statements : Node list, filter : NodeType -> IdDef option) =
    let map  =
        let m = Dictionary<IdentifierReference, Dictionary<int, Path>>()
        statements
        |> List.iter (
            fun (Node (path, definition)) ->
                match Option.bind filter definition with
                | None              -> ()
                | Some (id, seq)    ->
                    if m.ContainsKey(id) = false then
                        let d = Dictionary<int, Path>()
                        m.Add(id, d)
                    m.[id].Add(seq, path)
        )

        m

    static member MakeForTypeDefinitions     (statements : Node list) = Resolution(statements, Patterns.``|TypeDef|_|``)
    static member MakeForGroupingDefinitions (statements : Node list) = Resolution(statements, Patterns.``|GroupingDef|_|``)

    member this.ToStringBuilder (sb : StringBuilder) =
        map
        |> Seq.iter (
            fun kv ->
                let id = kv.Key.Value
                let keys = kv.Value.Keys

                if kv.Value.Count = 1 then
                    let seq = Seq.item 0 keys
                    Printf.bprintf sb "%-50s : (%03d) %A\n" id seq (kv.Value.Item(seq))
                    ()
                else
                    Printf.bprintf sb "%-50s : \n" id
                    keys |> Seq.iter (
                        fun seq ->
                            Printf.bprintf sb "\t (%03d) %A\n" seq (kv.Value.Item(seq))
                    )
                    ()
        )

    member this.AsString =
        let sb = StringBuilder()
        Printf.bprintf sb "\n"
        this.ToStringBuilder sb
        sb.ToString()

    member __.Find (id : IdentifierReference, sequence) = map.[id].[sequence]

    override this.ToString() = this.AsString


let resolve (statements : Node list) =
    let groupings = Resolution.MakeForGroupingDefinitions statements
    let types     = Resolution.MakeForTypeDefinitions     statements
    let groups      = mkPhiGraph Patterns.``|GroupingDef|_|`` statements

    printfn "Finished bulding aux graphs"

    let statements  = resolve_groups groups statements
    let graph       = mkGraph statements

    printfn "Finished phase 1"

    let resolve (id : IdentifierReference, path : Path) =
        printfn "Searching for %A in %A" id path

        let rec search_down (current : DefUseGraphNode) =
            // printfn "Searching at level <%A>" current
            match try_find_local_type_definition (current, id) with
            | Some _ as result      -> result
            | None ->
                match search_up current with
                | Some _ as result  -> result
                | None ->
                    match current.Parent with
                    | Some parent   -> search_down parent
                    | None          -> None

        and search_up (current : DefUseGraphNode) =
            let children = get_local_group_uses current
            // printfn "Node <%A> has %d group uses: %A" current children.Length children
            do_search_up children

        and do_search_up children =
            match children with
            | []                    -> None
            | (group_id, seq) :: tl ->
                printfn "Searching in groupings; %A (%d)" group_id seq
                let candidate = groupings.Find (group_id, seq)
                printfn "Searching in groupings; %A -- %A" group_id candidate
                let jump = graph.FindClosest candidate
                printfn "Will resume from %A" jump
                match try_find_local_type_definition (jump, id) with
                | Some _ as result  -> result
                | None ->
                    let more_children = get_local_group_uses jump
                    printfn "Did not find; new group uses: %A" more_children
                    do_search_up (more_children @ tl)

        let nearest = graph.FindClosest path

        let sequence = search_down nearest
        printfn "Found %A in %A" id sequence
        Node.MakeTypeUse(path, id, sequence)

    statements
    |> List.map (
        fun (Node (path, definition) as statement) ->
            match definition with
            | Patterns.TypeUseUnresolvedOption id ->
                if id.IsPrimitive then Node.MakeTypeUse(path, id, 0)
                else
                    resolve (id, path)
            | _ -> statement
    )

let g0a = [
    Node.MakeGroupingDefinition ("module", "group1", 1)
    Node.MakeGroupingUse        ("module", "group1")
]

resolve g0a


let g0 = [
    Node.MakeGroupingDefinition ("module", "group1", 1)
    Node.MakeGroupingUse        ("module/element", "group1")
]
resolve g0

let g1 = [
    Node.MakeGroupingDefinition ("module", "group1", 1)
    Node.MakeGroupingDefinition ("module/group1", "group2", 1)
    Node.MakeGroupingUse        ("module/group1", "group2")
    Node.MakeGroupingDefinition ("module", "group2", 2)
    Node.MakeGroupingUse        ("module/element", "group1")
    Node.MakeGroupingUse        ("module/element", "group2")
]

resolve g1

let g2a = [
    Node.MakeGroupingDefinition ("module", "group1", 1)
    Node.MakeTypeDefinition     ("module", "T1", 1)
    Node.MakeTypeUse            ("module", "T1")
]

resolve g2a

let g2b = [
    Node.MakeGroupingDefinition ("module", "group1", 1)
    Node.MakeTypeDefinition     ("module", "T1", 1)
    Node.MakeTypeUse            ("module/element", "T1")
]

resolve g2b

let g2 = [
    Node.MakeGroupingDefinition ("module", "group1", 1)
    Node.MakeTypeDefinition     ("module/group1", "T", 1)
    Node.MakeGroupingDefinition ("module", "group2", 2)
    Node.MakeTypeDefinition     ("module/group2", "T", 2)
    Node.MakeGroupingUse        ("module/element", "group1")
    Node.MakeTypeUse            ("module/element/item", "T")
]

resolve g2

let g3 = [
    Node.MakeGroupingDefinition ("module", "group1", 1)
    Node.MakeGroupingUse        ("module/group1", "group2")
    Node.MakeGroupingDefinition ("module/group1", "group2", 1)
    Node.MakeTypeDefinition     ("module/group1/group2", "T", 1)
    Node.MakeGroupingUse        ("module/element", "group1")
    Node.MakeTypeUse            ("module/element/item", "T")
]

resolve g3

let juniper_def_use = DefUse.VisitDefinitions (fun _ -> true) (Yang.Model.Statements.Module juniper)
let juniper_def_use' = resolve juniper_def_use
juniper_def_use' |> List.iter (printfn "%A")
DefUse.HasUnresolved juniper_def_use'
juniper_def_use' |> List.filter (fun v -> v.IsUnresolved)

// At this point, we have resolved all def-use.
// We need to lift them, and put them in order.


  // TODO: test with augment-stmt and uses-augment-stmt

(*
How do we parse examples from p.169?
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:services/ex:ssh"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:user[ex:name=fred]"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:user[ex:name='fred']"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:server[ex:ip='192.0.2.1'][ex:port='80']"
apply_parser Identifier.parse_schema_node_identifier
*)

let xx = apply_parser parse_path "/ex:system/ex:server[ex:ip='192.0.2.1'][ex:port='80']/ex:other"
