// Program.fs
// Intermediate representation for the library that will be generated
namespace Yang.Generator

module Program =
    open Yang.Model
    open Yang.Model.Arguments.Path

    let private throw fmt =
        let do_throw message = raise (ProgramCreationException message)
        Printf.ksprintf do_throw fmt

    type UInt32Type = {
        Description     : string option
        Range           : Arguments.Range option
        Default         : System.UInt32 option
    }

    type StringType = {
        Description     : string option
        Restrictions    : Statements.StringRestrictions
        Default         : string option
    }
    with
        member this.HasRestrictions =
            match this.Restrictions with
            | None, []  -> false
            | _         -> true

    [<StructuredFormatDisplay("{Display}")>]
    type YangInfo = | YangInfo of Name:Identifier * Description:(string option) * Path:(Identifier list) * PathDescription:(string list)
    with
        static member Make (name : string) =
            let id = Identifier.Make name
            YangInfo (id, None, [id], [])

        static member Make (name : string, description : string) =
            let id = Identifier.Make name
            YangInfo (id, Some description, [id], [description])

        static member Make (name : string, description : string option) =
            let id = Identifier.Make name
            match description with
            | None      -> YangInfo (id, description, [id], [])
            | Some help -> YangInfo (id, description, [id], [help])

        member this.Prepend (label : Identifier list) =
            let (YangInfo (name, description, path, help)) = this
            YangInfo (name, description, label @ path, help)

        member this.Prepend (label : Identifier list, label_help : string) =
            let (YangInfo (name, description, path, help)) = this
            YangInfo (name, description, label @ path, label_help :: help)

        member this.Prepend (label : Identifier list, label_help : string list) =
            let (YangInfo (name, description, path, help)) = this
            YangInfo (name, description, label @ path, label_help @ help)

        member this.Prepend (predecessor : YangInfo) =
            let (YangInfo (name, description, path, help)) = this
            let (YangInfo (_, _, path', help')) = predecessor
            YangInfo (name, description, path' @ path, help' @ help)

        member this._Path = let (YangInfo (_, _, path, _)) = this in path

        member this._Description = let (YangInfo (_, description, _, _)) = this in description

        member this.Display =
            let (YangInfo (name, _, path, _)) = this
            let length = path.Length
            if length = 0 then     throw "Path length cannot be zero"
            elif length = 1 then   name.Value
            else sprintf "%s [%s]" name.Value (path |> List.take (length - 1) |> List.map (fun id -> id.Value) |> String.concat "/")

        override this.ToString () = this.Display

    type [<StructuredFormatDisplay("{Display}")>] Definition =
    | Container     of ContainerDefinition
    | Member        of MemberDefinition * Type
    | ListWithKey   of Member:MemberDefinition * Type:Type * Key:MemberDefinition
    with
        member this.Display =
            match this with
            | Container     container           -> sprintf "%s { %A }" container.Info.Display container.Body
            | Member (``member``, ty)           -> sprintf "%s : %s" ``member``.Display ty.Display
            | ListWithKey (``member``, ty, key) -> sprintf "%s : Dictionary<%s, %s>" ``member``.Display key.Display ty.Display
    and [<StructuredFormatDisplay("{Display}")>] Type =
    | String    of StringType
    | UInt32    of UInt32Type
    | Class     of ContainerDefinition
    with
        member this.Display =
            match this with
            | String _          -> "string"
            | UInt32 _          -> "UInt32"
            | Class container   -> container.Info.Display

        override this.ToString() = this.Display

        member this.ClrName =
            match this with
            | String _          -> "string"
            | UInt32 _          -> "uint32"
            | Class cl          -> cl.Name.Value

    and ContainerDefinition = {
        Info            : YangInfo

        mutable Name    : string option
        Presence        : bool
        Body            : Definition list
    }
    and [<StructuredFormatDisplay("{Display}")>] MemberDefinition = {
        Info            : YangInfo

        mutable Name    : string option
        Optional        : bool
    }
    with
        member this.Display = this.Info.Display
        override this.ToString() = this.Display

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Definition =
        let IsContainer = function
        | Definition.Container  _  -> true
        | _                        -> false

        let (|Container|_|) = function
        | Definition.Container c    -> Some c
        | _                         -> None

        let GetYangInfo = function
        | Definition.Container container            -> container.Info
        | Definition.Member    (``member``, _)      -> ``member``.Info
        | Definition.ListWithKey (``member``, _, _) -> ``member``.Info

        let Prepend (info : YangInfo) = function
        | Definition.Container container                -> Definition.Container { container with Info = container.Info.Prepend info }
        | Definition.Member    (``member``, ty)         -> Definition.Member ( { ``member`` with Info = ``member``.Info.Prepend info }, ty )
        | Definition.ListWithKey (``member``, ty, key)  -> Definition.ListWithKey ( { ``member`` with Info = ``member``.Info.Prepend info }, ty, key )

        let Description = function
        | Definition.Container container            ->  container.Info._Description
        | Definition.Member    (``member``, _)      -> ``member``.Info._Description
        | Definition.ListWithKey (``member``, _, _) -> ``member``.Info._Description


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module ContainerDefinition =

        let CanEliminate (definition : ContainerDefinition) =
            definition.Presence = false
