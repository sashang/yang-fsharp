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

// Typical statistics: Real: 00:00:14.234, CPU: 00:00:14.203, GC gen0: 1231, gen1: 1160, gen2: 0
let juniper_def_use = Yang.Model.DefUseResolver.VisitDefinitions (fun _ -> true) (Yang.Model.Statements.Module juniper)

printfn "Length: %d" juniper_def_use.Length
juniper_def_use |> List.iter (printfn "%A")

open System.Collections.Generic
open System.Text
open System.Collections.ObjectModel

[<StructuredFormatDisplay("{AsString}")>]
type GraphNode<'T when 'T : (new : unit -> 'T)> = {
    Parent      : GraphNode<'T> option
    Children    : Dictionary<IdentifierReference, GraphNode<'T>>
    Value       : 'T
} with
    static member Empty() = {
        Parent      = None
        Children    = new Dictionary<IdentifierReference, GraphNode<'T>>()
        Value       = new 'T()
    }
    static member Make(parent : GraphNode<'T>) = {
        Parent      = Some parent
        Children    = new Dictionary<IdentifierReference, GraphNode<'T>>()
        Value       = new 'T()
    }

    member this.ToStringBuilder (sb : StringBuilder, index : int) =
        let indent () = Printf.bprintf sb "%s" (String.replicate index "    ")
        indent ()
        Printf.bprintf sb "(*)\t%A\n"  this.Value
        this.Children
        |> Seq.iter (
            fun child ->
                indent()
                Printf.bprintf sb "(+)\t%s\n" child.Key.Value
                child.Value.ToStringBuilder (sb, index + 1)
        )

    member this.AsString =
        let sb = StringBuilder ()
        this.ToStringBuilder (sb, 0)
        sb.ToString()

    override this.ToString() = this.AsString


[<StructuredFormatDisplay("{AsString}")>]
type Graph<'T when 'T : (new : unit -> 'T)> = {
    Root    : GraphNode<'T>
} with
    member private this.GetNode (items : IdentifierReference list) =
        let rec search (current : GraphNode<'T>) (path : IdentifierReference list) =
            match path with
            | [] -> current
            | hd :: tl ->
                if current.Children.ContainsKey(hd) = false then
                    current.Children.Add(hd, GraphNode<'T>.Make(current))

                let next = current.Children.[hd]
                search next tl

        search this.Root items

    static member Empty() = { Root = GraphNode<'T>.Empty() }
    member this.Add(path : IdentifierReference list, apply : 'T -> unit) =
        let node = this.GetNode path
        apply node.Value

    member this.ToStringBuilder (sb : StringBuilder) =
        this.Root.ToStringBuilder(sb, 0)

    member this.AsString =
        let sb = StringBuilder()
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

type Groupings = Graph<Phi>
let groups =
    let add node (phi : Phi) =
        phi.Add node

    let g = Groupings.Empty()
    juniper_def_use
    |> List.iter (
        fun (Yang.Model.DefUseResolver.Node (Yang.Model.DefUseResolver.Path path, definition)) ->
            match definition with
            | Some (Yang.Model.DefUseResolver.GroupingDefinition grouping) ->
                let path = List.rev path
                g.Add(path, add grouping)
            | _ -> ()
    )
    g

groups

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
