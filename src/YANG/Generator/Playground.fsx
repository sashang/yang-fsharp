// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "System.Xml.dll"
#r "System.Xml.Linq.dll"

#r @"..\Model\bin\Debug\Yang.Model.dll"
#r @"..\XmlHelper\bin\Debug\Yang.XmlHelper.dll"

//#load @"..\..\..\paket-files\fsprojects\FSharp.TypeProviders.StarterPack\src\ProvidedTypes.fs"
//#load @"..\..\..\paket-files\fsprojects\FSharp.TypeProviders.StarterPack\src\ProvidedTypesTesting.fs"

#load "Errors.fs"
#load "Program.fs"

open Yang.Model
open Yang.Generator.Program

let mkTypeStringSimple (description : string option) : Yang.Generator.Program.Type = Type.String {
    Description     = description
    Restrictions    = []
    Default         = None
}

let mkTypeUInt32 (description : string option, range : Arguments.Range.Range option) : Yang.Generator.Program.Type = Type.UInt32 {
    Description     = description
    Range           = range
    Default         = None
}

let mkMember (yang_name : string, description : string option, ``type`` : Yang.Generator.Program.Type) : Definition =
    Member (
        {
            Info        = YangInfo.Make (yang_name, description)
            Name        = None
            Optional    = true
        }, ``type``)

let mkMemberListWithKey (yang_name : string, description : string option, ``type`` : Yang.Generator.Program.Type, key : MemberDefinition) : Definition =
    ListWithKey (
        {
            Info        = YangInfo.Make (yang_name, description)
            Name        = None
            Optional    = true
        }, ``type``, key
    )




let address_v4_name : MemberDefinition = {
    Info        = YangInfo.Make ("name", "Interface address/destination prefix")
    Name        = None
    Optional    = false
}

let address_v6_name : MemberDefinition = {
    Info        = YangInfo.Make ("name", "Interface address/destination prefix")
    Name        = None
    Optional    = false
}

let address_v4 : ContainerDefinition = {
    Info        = YangInfo.Make ("address", "Interface address/destination prefix")
    Name        = None
    Presence    = false
    Body        =
        [
            Definition.Member (address_v4_name, mkTypeStringSimple None)
        ]
}

let address_v6 : ContainerDefinition = {
    Info        = YangInfo.Make ("address", "Interface address/destination prefix")
    Name        = None
    Presence    = false
    Body        =
        [
            Definition.Member (address_v6_name, mkTypeStringSimple None)
        ]
}

let inet : Definition = Definition.Container {
    Info        = YangInfo.Make ("inet", "IPv4 parameters")
    Name        = None
    Presence    = true
    Body        =
        [
            mkMemberListWithKey ("address", Some "IPv4 protocol parameters", Type.Class address_v4, address_v4_name)
        ]
}

let inet6 : Definition = Definition.Container {
    Info        = YangInfo.Make ("inet6", "IPv6 parameters")
    Name        = None
    Presence    = true
    Body        =
        [
            mkMemberListWithKey ("address", Some "IPv6 protocol parameters", Type.Class address_v6, address_v6_name)
        ]
}

let family : Definition = Definition.Container {
    Info        = YangInfo.Make ("family", "Protocol family")
    Name        = None
    Presence    = false
    Body        = [ inet; inet6 ]
}

let unit_class_name : MemberDefinition = {
    Info        = YangInfo.Make "name"
    Name        = None
    Optional    = false
}

let unit_class : ContainerDefinition = {
    Info        = YangInfo.Make ("unit", "Logical interface")
    Name        = None
    Presence    = false
    Body        =
        [
            Member (unit_class_name, mkTypeStringSimple None)
            mkMember ("description", Some "Text description of interface", mkTypeStringSimple None)
            family
        ]
}

let interface_class_name : MemberDefinition = {
    Info        = YangInfo.Make ("name", "Interface name")
    Name        = None
    Optional    = false
}

let interface_class : ContainerDefinition = {
    Info        = YangInfo.Make ("interface", "Physical interface")
    Name        = None
    Presence    = false
    Body        =
        [
            Member (interface_class_name, mkTypeStringSimple None)
            mkMember ("description", Some "Text description of interface", mkTypeStringSimple None)
            mkMember ("mtu", Some "Maximum transmission unit packet size", mkTypeUInt32 (None, Some (Arguments.Range.MakeInt32Range(256, 9216))))
            mkMemberListWithKey ("unit", Some "Logical interface", Type.Class unit_class, unit_class_name)
        ]
}

let interfaces : Definition =
    Definition.Container {
        Info        = YangInfo.Make ("interfaces", "Interface configuration")
        Name        = None
        Presence    = false
        Body        =
            [
                mkMemberListWithKey ("interface", Some "Physical interface", Type.Class interface_class, interface_class_name)
            ]
    }

let system : Definition = Definition.Container {
    Info        = YangInfo.Make ("system", "Logical systems")
    Name        = None
    Presence    = false
    Body        = [ interfaces ]
}

let configuration : Definition = Definition.Container {
    Info        = YangInfo.Make "configuration"
    Name        = None
    Presence    = false
    Body        =
        [
            mkMember ("version", Some "Software version information", mkTypeStringSimple None)
            system
        ]
}


let FlattenHierarchy (root : Definition) : Definition =
    // TODO: Propagate documentation information

    let rec do_simplify (root : Definition) : Definition list =
        match root with
        | Definition.Container definition ->
            if ContainerDefinition.CanEliminate definition = false then
                // Cannot eliminate the path; try to simplify children
                [ Definition.Container { definition with Body = definition.Body |> List.collect do_simplify } ]
            else
                let path = definition.Info
                let body' = definition.Body |> List.collect do_simplify
                body' |> List.map (Definition.Prepend path)
        | Definition.ListWithKey (``member``, ty, key) ->
            let ty' =
                match ty with
                | Type.Class cl     -> Type.Class { cl with Body = cl.Body |> List.collect do_simplify }
                | _                 -> ty
            [ Definition.ListWithKey (``member``, ty', key) ]

        | _ -> [ root ]

    match root with
    | Definition.Container root -> Definition.Container { root with Body = root.Body |> List.collect do_simplify }
    | _                         -> root


let program = FlattenHierarchy configuration


open System.Text

let default_using = """
using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics.Contracts;
using System.Text;
using System.Text.RegularExpressions;
using Yang.Generator;
"""

let template_summary_help = """
/// <summary>
/// #DESCRIPTION#
/// </summary>
"""

let customize_one (key : string, value : string) (template : string) =
    let key' = sprintf "#%s#" key
    template.Replace(key', value)

let make_clr_name (name : string, class_ending : bool, index : int option) =
    let sb = StringBuilder()
    let mutable capitalize = true

    for i = 0 to (name.Length - 1 ) do
        let ch = name.Chars i
        if ch = '-' then
            capitalize <- true
        elif capitalize then
            sb.Append(System.Char.ToUpperInvariant ch) |> ignore
            capitalize <- false
        else
            sb.Append(ch) |> ignore

    if index.IsSome then Printf.bprintf sb "_%05d_" index.Value
    if class_ending then Printf.bprintf sb "Class"

    sb.ToString()

let make_clr_name_simple name = make_clr_name (name, false, None)

make_clr_name ("configuration", true, None)
make_clr_name ("vlan-id", true, None)
make_clr_name ("vlan-id-range", true, None)
make_clr_name ("system", true, Some 1)

type Printer (ns : string, ?tab : string) =
    let tab = defaultArg tab "    "

    let sb = StringBuilder ()
    let mutable indentation = 0

    let push () = indentation <- indentation + 1
    let pop  () = indentation <- indentation - 1

    let indent ()   = sb.Append(String.replicate indentation tab) |> ignore
    let nl ()       = Printf.bprintf sb "\n"

    let bb ()       = Printf.bprintf sb "{"; nl(); push()
    let eb ()       = pop(); indent(); Printf.bprintf sb "}"; nl()

    let write (line : string) =
        indent ()
        Printf.bprintf sb "%s" line
        nl()

    let write_many (line : string) = line.Trim().Split('\n') |> Array.iter write

    member __.Reset () =
        indentation <- 0
        sb.Clear() |> ignore

    member this.Append (container : ContainerDefinition) =
        let (YangInfo (name, help, _, _)) = container.Info
        if help.IsSome then
            write_many (customize_one ("DESCRIPTION", help.Value) template_summary_help)

        if container.Name.IsNone then
            container.Name <- Some (make_clr_name (name.Value, true, None))
        let class_name = container.Name.Value

        indent ()
        Printf.bprintf sb "public class %s" class_name; nl()
        indent ()
        bb()

        let last_index = List.length container.Body - 1
        // Write information about internal methods
        container.Body |> List.iteri (
            fun index definition ->
                this.Append definition
                if index < last_index then nl ()
        )

        // End class definition
        eb()

    member this.Append (m : MemberDefinition, ty : Type) =
        let (YangInfo (name, help, _, _)) = m.Info
        if help.IsSome then
            write_many (customize_one ("DESCRIPTION", help.Value) template_summary_help)

        if m.Name.IsNone then
            m.Name <- Some (make_clr_name (name.Value, false, None))
        let member_name = m.Name.Value

        indent()
        Printf.bprintf sb "public readonly %s %s;" ty.ClrName member_name
        nl ()


    member this.Append (m : MemberDefinition, ty : Type, key : MemberDefinition) =
        let (YangInfo (name, help, _, _)) = m.Info
        if m.Name.IsNone then m.Name <- Some (make_clr_name (name.Value, false, None))

        if key.Name.IsNone then key.Name <- Some (make_clr_name (m.Name.Value, false, None))
        let key_type = "string"; // TODO: find key type

        match ty with
        | Type.Class container ->
            this.Append container
            nl ()
        | _ -> failwith "Not expected"

        let name = m.Name.Value
        if help.IsSome then
            write_many (customize_one ("DESCRIPTION", help.Value) template_summary_help)

        indent ()
        Printf.bprintf sb "public readonly IReadOnlyDictionary<%s, %s> %s;" key_type ty.ClrName name
        nl ()

    member this.Append (root : Definition) =
        match root with
        | Definition.Container container        -> this.Append container
        | Definition.Member    (m, ty)          -> this.Append (m, ty)
        | Definition.ListWithKey (m, ty, key)   -> this.Append (m, ty, key)

    member this.Generate (root : Definition) =
        this.Reset()

        Printf.bprintf sb "namespace %s" ns; nl()
        bb()
        write_many default_using
        nl()
        this.Append root
        eb()

        sb.ToString()


    static member Generate (ns : string, root : Definition) =
        let pp = Printer ns
        pp.Generate root


printfn "\n%s" (Printer.Generate ("Juniper", program))
