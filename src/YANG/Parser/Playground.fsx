// Playground.fsx
// Testing of parser functionality during development.

#load "Initialize.fsx"

open FParsec
open Yang.Model
open Yang.Parser
open Yang.Parser.BodyStatements

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

open System.Collections.Generic
open System.Text
open System.Collections.ObjectModel

open DefUseResolver
open System

[<StructuredFormatDisplay("{AsStringShort}")>]
type GraphNode<'T> (value : 'T, ?id : IdentifierReference, ?parent : GraphNode<'T>) = class
    let children = new Dictionary<IdentifierReference, GraphNode<'T>>()

    member __.Id            = id
    member __.Parent        = parent
    member __.Children      = children |> Seq.map (fun kv -> kv.Key, kv.Value)
    member __.CountChildren = children.Count
    member __.IsRoot        = parent.IsNone
    member __.IsNotRoot     = parent.IsSome
    member __.Value         = value

    member this.ToStringBuilder (sb : StringBuilder, index : int) =
        let indent () = Printf.bprintf sb "%s" (String.replicate index "    ")

        indent ()
        Printf.bprintf sb "(*)\t%A\n"  value
        this.Children
        |> Seq.iter (
            fun (key, child) ->
                indent()
                Printf.bprintf sb "(+)\t%s\n" key.Value
                child.ToStringBuilder (sb, index + 1)
        )

    member this.AsString =
        let sb = StringBuilder ()
        this.ToStringBuilder (sb, 0)
        sb.ToString()

    member this.AsStringShort =
        let root_string = if this.IsRoot then "[Root] " else ""
        match id with
        | None      -> sprintf "%s<unknown id> (%d children)"  root_string this.CountChildren
        | Some id   -> sprintf "%sID: %A (%d children)"        root_string id this.CountChildren

    member __.HasChild (id : IdentifierReference) = children.ContainsKey id
    member __.AddChild (id : IdentifierReference, child : GraphNode<'T>) = children.Add(id, child)
    member __.GetChild (id : IdentifierReference) = children.[id]

    member this.Search (id : IdentifierReference) : 'T option =
        if children.ContainsKey(id) then Some (children.[id].Value)
        elif this.IsNotRoot         then parent.Value.Search id
        else None

    member this.Path =
        let rec find (current : GraphNode<'T>) (result : IdentifierReference list) =
            match parent, id with
            | None, Some id         -> id :: result
            | None, None            -> result
            | Some parent, Some id  -> find parent (id :: result)
            | Some _, None          -> failwith "Internal node, no id"

        match id with
        | None      -> []   // Root node
        | Some _    -> find this []

    member this.PathAsString (?separator : string) =
        let separator = defaultArg separator "/"
        let path = this.Path |> List.map (fun v -> v.Value) |> String.concat separator
        sprintf "/%s" path

    override this.ToString() = this.AsString
end
and GraphNodeChild<'T> = IdentifierReference * GraphNode<'T>

type Graph<'T when 'T : (new : unit -> 'T)> = {
    Root    : GraphNode<'T>
} with
    member private this.GetNode (items : IdentifierReference list) =
        let rec search (current : GraphNode<'T>) (path : IdentifierReference list) =
            match path with
            | [] -> current
            | hd :: tl ->
                if current.HasChild hd = false then
                    let child = GraphNode<'T>(new 'T(), hd, current)
                    current.AddChild(hd, child)

                let next = current.GetChild hd
                search next tl

        search this.Root items

    member private this.SearchNode (items : IdentifierReference list) =
        let rec search (current : GraphNode<'T>) (path : IdentifierReference list) =
            match path with
            | []        -> current
            | hd :: tl  ->
                if current.HasChild(hd) = false then
                    failwithf "Node %A (of %A) not found" hd path
                else
                    let next = current.GetChild hd
                    search next tl

        search this.Root items

    member private this.SearchNodeLongest (items : IdentifierReference list) =
        let rec search (current : GraphNode<'T>) (path : IdentifierReference list) =
            match path with
            | []        -> current
            | hd :: tl  ->
                if current.HasChild(hd) = false then
                    current
                else
                    let next = current.GetChild hd
                    search next tl

        search this.Root items

    static member Empty() = { Root = GraphNode<'T>(new 'T()) }

    member this.Add(path : IdentifierReference list, apply : 'T -> unit) =
        let node = this.GetNode path
        apply node.Value

    member this.Add(path : Path, apply : 'T -> unit) = this.Add (path.AsPathList, apply)

    member this.Search(path : IdentifierReference list) =
        let node = this.SearchNode path
        node.Value

    member this.FindClosest(path : Path) = this.SearchNodeLongest path.AsPathList
    member this.FindClosest(path : string, ?separator : char) =
        let separator = defaultArg separator '/'
        path.Split(separator)
        |> Array.toList
        |> List.filter (fun entry -> (String.IsNullOrWhiteSpace entry) = false)
        |> List.map IdentifierReference.Make
        |> this.SearchNodeLongest

    member this.ToStringBuilder (sb : StringBuilder) =
        this.Root.ToStringBuilder(sb, 0)

    member this.AsString =
        let sb = StringBuilder()
        sb.AppendLine() |> ignore
        this.ToStringBuilder sb
        sb.ToString()

    override this.ToString() = this.AsString


[<StructuredFormatDisplay("{AsString}")>]
type Phi () = class
    let phis = new Dictionary<IdentifierReference, List<DefUseResolver.IdDef>>()

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

    member this.Add (definition : Yang.Model.DefUseResolver.IdDef) =
        let (id, _) = definition
        if (phis.ContainsKey id) = false then
            phis.Add(id, List())

        let l = phis.[id]
        if l.Contains(definition) = false then l.Add(definition)

    member this.Phis = ReadOnlyDictionary(phis)

    override this.ToString() = this.AsString
end

[<StructuredFormatDisplay("{AsString}")>]
type NodeDefinition () = class
    let properties = List<Node>()

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
    //node.Children
    //|> Seq.choose (
    //    fun (_, node) ->
    //        match AsGroupUseNode node with
    //        | None                  -> None
    //        | Some (id, Some seq)   -> Some (id, seq)
    //        | Some (_, None)        -> None
    //)
    //|> Seq.toList

let try_find_local_type_definition (node : DefUseGraphNode, id : IdentifierReference) =
    let candidates =
        node.Children
        |> Seq.choose (
            fun (_, node) ->
                match AsTypeDefinitionNode node with
                | Some (_id, seq)   -> if id = _id then Some seq else None
                | _                 -> None
        )
        |> Seq.toList
    if candidates.Length = 1    then Some candidates.Head
    elif candidates.Length > 1  then failwithf "Error: found many candidates:\n%A\n" candidates
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
            | (id, seq) :: tl       ->
                // printfn "Searching in groupings; %A (%d)" id seq
                let candidate = groupings.Find (id, seq)
                // printfn "Searching in groupings; %A -- %A" (id) candidate
                let jump = graph.FindClosest candidate
                // printfn "Will resume from %A" jump
                match try_find_local_type_definition (jump, id) with
                | Some _ as result  -> result
                | None ->
                    let more_children = get_local_group_uses jump
                    // printfn "Did not find; new group uses: %A" more_children
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

let g0a_graph = mkPhiGraph Patterns.``|GroupingDef|_|`` g0a
g0a_graph.AsString
resolve_groups g0a_graph g0a

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

let g2bgraph = mkGraph g2b
g2bgraph.AsString
let g2broot = g2bgraph.FindClosest("module")
g2broot.Value.TypeDefinitions


let g2 = [
    Node.MakeGroupingDefinition ("module", "group1", 1)
    Node.MakeTypeDefinition     ("module/group1", "T", 1)
    Node.MakeGroupingDefinition ("module", "group2", 2)
    Node.MakeTypeDefinition     ("module/group2", "T", 2)
    Node.MakeGroupingUse        ("module/element", "group1")
    Node.MakeTypeUse            ("module/element/item", "T")
]

let g2graph = mkGraph g2
g2graph.AsString

resolve g2

let juniper_def_use = Yang.Model.DefUseResolver.VisitDefinitions (fun _ -> true) (Yang.Model.Statements.Module juniper)
let juniper_def_use' = resolve juniper_def_use
juniper_def_use' |> List.iter (printfn "%A")

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
