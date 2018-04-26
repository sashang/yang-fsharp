// Graph.fs
// Helper structures and algorithms for navigating (directed acyclic) graphs of definitions
namespace Yang.Model

/// Algorithms and structures for navigating graphs
module Graph =
    open System
    open System.Collections.Generic
    open System.Text
    open Yang.Model.Identifier

    let default_path_separator = '/'

    type TEdge = IdentifierReference

    /// <summary>
    /// Encodes a path in the graph
    /// </summary>
    [<StructuredFormatDisplay("{AsString}")>]
    type Path = | Path of (TEdge list)
    with
        // Note: The order of edges is reversed, i.e. edges closer to the leaf's appear earlier in the list

        /// <summary>
        /// Creates an empty path
        /// </summary>
        static member Empty = Path []

        /// Create a path that contains a single edge
        static member Make (identifier : Identifier) =
            Path [ IdentifierReference.Make identifier ]
        /// Create a path from a list of identifiers; edges closer to root should appear last
        static member Make (identifier : Identifier list) =
            Path ( identifier |> List.map IdentifierReference.Make)
        /// Create a path that contains a single edge
        static member Make (identifier : IdentifierReference) =
            Path [ identifier ]
        /// Create a path from a list of identifiers; edges closer to root should appear last
        static member Make (identifier : IdentifierReference list) =
            Path identifier
        /// Create a path that contains a single edge
        static member Make (identifier : IdentifierWithPrefix) =
            Path [ IdentifierReference.Make identifier ]
        /// Create a path from a list of identifiers; edges closer to root should appear last
        static member Make (identifier : IdentifierWithPrefix list) =
            Path (identifier |> List.map IdentifierReference.Make)
        /// Create a path from an identifier (given as a string)
        static member Make (identifier : string) =
            Path [ IdentifierReference.Make identifier ]

        /// <summary>
        /// Create a path from a textual description
        /// </summary>
        /// <param name="identifier">The path as a string; edges closer to root should appear earlier in the string</param>
        /// <param name="separator"></param>
        /// <example>
        /// Example path string:
        /// <code>
        /// "/configuration/address/ipv4"
        /// <code>
        /// </example>
        static member MakeFromPath (identifier : string, ?separator : char) =
            let separator = defaultArg separator default_path_separator
            let identifier = if identifier.StartsWith(separator.ToString()) then identifier.Substring(1) else identifier
            let ids = identifier.Split(separator) |> Array.toList |> List.rev
            Path ( ids |> List.map IdentifierReference.Make )

        /// Get the internal representation of the path;
        /// Edges closer to leaf appear earlier in the list.
        /// Note: do not depend on this internal representation.
        member this._Path = let (Path path) = this in path

        /// Get the leaf edge
        member this._Head = let (Path path) = this in List.head path

        /// <summary>
        /// Create a new path by adding a leaf
        /// </summary>
        /// <param name="identifier">The new leaf</param>
        member this.Push (identifier : Identifier) =
            Path ((IdentifierReference.Make identifier) :: this._Path)

        /// <summary>
        /// Create a new path by adding a leaf
        /// </summary>
        /// <param name="identifier">The new leaf</param>
        member this.Push (identifier : IdentifierReference) =
            Path (identifier :: this._Path)

        /// <summary>
        /// Create a new path by adding a leaf
        /// </summary>
        /// <param name="identifier">The new leaf</param>
        member this.Push (identifier : IdentifierWithPrefix) =
            Path ((IdentifierReference.Make identifier) :: this._Path)

        /// <summary>
        /// Create a new path by adding a leaf
        /// </summary>
        /// <param name="identifier">The new leaf</param>
        member this.Push (identifier : string) =
            Path ((IdentifierReference.Make identifier) :: this._Path)

        /// <summary>
        /// Create a new path by removing the leaf.
        /// </summary>
        member this.Pop () =
            let rest = List.tail this._Path
            Path rest

        /// <summary>
        /// Get the path that corresponds to the parent of this path
        /// </summary>
        member this.Parent = this.Pop ()

        /// Get the edges of the path. Edges closer to root appear first in the list.
        member this.AsPathList = this._Path |> List.rev

        /// Get the textual representation of the path
        member this.AsString =
            let path = this._Path |> List.rev |> List.map (fun p -> p.Value) |> String.concat "/"
            sprintf "/%s" path

        override this.ToString() = this.AsString

    /// <summary>
    /// Represents a node in the graph
    /// </summary>
    [<StructuredFormatDisplay("{AsStringShort}")>]
    type GraphNode<'T> (value : 'T, ?id : TEdge, ?parent : GraphNode<'T>) = class
        /// Descendants of the node
        let children = new Dictionary<TEdge, GraphNode<'T>>()

        /// Gets the id of the node
        member __.Id            = id
        /// Gets the parent of the node
        member __.Parent        = parent
        /// Gets the descendants of the node
        member __.Children      = children |> Seq.map (fun kv -> kv.Key, kv.Value)
        /// Gets the number of descendants of this node
        member __.CountChildren = children.Count
        /// Checks whether this node is root in the graph (i.e. does not have a parent)
        member __.IsRoot        = parent.IsNone
        /// Checks whether this node is not a root in the graph
        member __.IsNotRoot     = parent.IsSome
        /// Gets the value associated with the node
        member __.Value         = value

        /// Write the textual represenattion of this node and all of its descendants to a string builder
        member this.ToStringBuilder (sb : StringBuilder, ?index : int) =
            let index = defaultArg index 0
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

        /// Retrieves the textual represtantaion of the node of all its descendants as a string
        member this.AsString =
            let sb = StringBuilder ()
            this.ToStringBuilder (sb, 0)
            sb.ToString()

        /// Short textual representation of node
        member this.AsStringShort =
            let root_string = if this.IsRoot then "[Root] " else ""
            match id with
            | None      -> sprintf "%s<unknown id> (%d children)"  root_string this.CountChildren
            | Some id   -> sprintf "%sID: %A (%d children)"        root_string id this.CountChildren

        /// <summary>
        /// Checks whether this node contains a particular descendant with the given id
        /// </summary>
        /// <param name="id">The id of the descendant</param>
        member __.HasChild (id : TEdge) = children.ContainsKey id

        /// <summary>
        /// Add a descendant to this node
        /// </summary>
        /// <param name="id">The id of the descendnat</param>
        /// <param name="child">The node of the descendant</param>
        member __.AddChild (id : TEdge, child : GraphNode<'T>) = children.Add(id, child)

        /// <summary>
        /// Retrieves the descendant with the given id
        /// </summary>
        /// <param name="id">The id of the descendant</param>
        member __.GetChild (id : TEdge) = children.[id]

        /// <summary>
        /// Retrieves the descendant with the given id
        /// </summary>
        /// <param name="id">The id of the descendant</param>
        member __.TryGetChild (id : TEdge) =
            if children.ContainsKey id then Some children.[id]
            else None

        /// <summary>
        /// Search for a node with the specified id, starting from this node
        /// and searching its parents until id is found (or, the search reaches the root)
        /// </summary>
        /// <param name="id"></param>
        member this.Search (id : TEdge) : 'T option =
            if children.ContainsKey(id) then Some (children.[id].Value)
            elif this.IsNotRoot         then parent.Value.Search id
            else None

        /// Gets the path in the graph that corresponds to this node
        member this.Path : Path =
            let rec find (current : GraphNode<'T>) (result : TEdge list) =
                match parent, id with
                | None, Some id         -> id :: result
                | None, None            -> result
                | Some parent, Some id  -> find parent (id :: result)
                | Some _, None          -> failwith "Internal node, no id"

            match id with
            | None      -> Path.Empty
            | Some _    -> let path = find this [] in Path (List.rev path)

        override this.ToString() = this.AsString
    end
    and GraphNodeChild<'T> = TEdge * GraphNode<'T>


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
