// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "System.Xml.dll"
#r "System.Xml.Linq.dll"

#r @"..\Model\bin\Debug\Yang.Model.dll"

//#load @"..\..\..\paket-files\fsprojects\FSharp.TypeProviders.StarterPack\src\ProvidedTypes.fs"
//#load @"..\..\..\paket-files\fsprojects\FSharp.TypeProviders.StarterPack\src\ProvidedTypesTesting.fs"

#load "Errors.fs"
#load "XmlHelper.fs"
#load "Program.fs"

open Yang.Model
open Yang.Generator.Program

let mkYangInfoSimple (name : string) = YangInfo (name, None)
let mkYangInfo (name : string, description) = YangInfo (name, Some description)

let mkTypeStringSimple (description : string option) : Yang.Generator.Program.Type = Type.String {
    Description     = description
    Restrictions    = None, []
    Default         = None
}

let mkTypeUInt32 (description : string option, range : Arguments.Range option) : Yang.Generator.Program.Type = Type.UInt32 {
    Description     = description
    Range           = range
    Default         = None
}

let mkMember (yang_name : string, description : string option, ``type`` : Yang.Generator.Program.Type) : Definition =
    Member (
        {
            Info        = YangInfo (yang_name, description)
            Name        = None
            Optional    = true
        }, ``type``)

let mkMemberListWithKey (yang_name : string, description : string option, ``type`` : Yang.Generator.Program.Type, key : MemberDefinition) : Definition =
    ListWithKey (
        {
            Info        = YangInfo (yang_name, description)
            Name        = None
            Optional    = true
        }, ``type``, key
    )




let address_v4_name : MemberDefinition = {
    Info        = mkYangInfo ("name", "Interface address/destination prefix")
    Name        = None
    Optional    = false
}

let address_v6_name : MemberDefinition = {
    Info        = mkYangInfo ("name", "Interface address/destination prefix")
    Name        = None
    Optional    = false
}

let address_v4 : ClassDefinition = {
    Info        = mkYangInfo ("address", "Interface address/destination prefix")
    Name        = None
    Presence    = false
    Body        =
        [
            Definition.Member (address_v4_name, mkTypeStringSimple None)
        ]
}

let address_v6 : ClassDefinition = {
    Info        = mkYangInfo ("address", "Interface address/destination prefix")
    Name        = None
    Presence    = false
    Body        =
        [
            Definition.Member (address_v6_name, mkTypeStringSimple None)
        ]
}

let inet : Definition = Definition.Class {
    Info        = mkYangInfo ("inet", "IPv4 parameters")
    Name        = None
    Presence    = true
    Body        =
        [
            Definition.Class address_v4
            mkMemberListWithKey ("address", Some "IPv4 protocol parameters", Type.Class address_v4, address_v4_name)
        ]
}

let inet6 : Definition = Definition.Class {
    Info        = mkYangInfo ("inet6", "IPv6 parameters")
    Name        = None
    Presence    = true
    Body        =
        [
            Definition.Class address_v6
            mkMemberListWithKey ("address", Some "IPv6 protocol parameters", Type.Class address_v6, address_v6_name)
        ]
}

let family : Definition = Definition.Class {
    Info        = mkYangInfo ("family", "Protocol family")
    Name        = None
    Presence    = false
    Body        = [ inet; inet6 ]
}

let unit_class_name : MemberDefinition = {
    Info        = mkYangInfoSimple "name"
    Name        = None
    Optional    = false
}

let unit_class : ClassDefinition = {
    Info        = mkYangInfo ("unit", "Logical interface")
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
    Info        = mkYangInfo ("name", "Interface name")
    Name        = None
    Optional    = false
}

let interface_class : ClassDefinition = {
    Info        = mkYangInfo ("interface", "Physical interface")
    Name        = None
    Presence    = false
    Body        =
        [
            Member (interface_class_name, mkTypeStringSimple None)
            mkMember ("description", Some "Text description of interface", mkTypeStringSimple None)
            mkMember ("mtu", Some "Maximum transmission unit packet size", mkTypeUInt32 (None, Some (Arguments.Range "256 .. 9216")))

            Definition.Class unit_class
            mkMemberListWithKey ("unit", Some "Logical interface", Type.Class unit_class, unit_class_name)
        ]
}

let interfaces : Definition =
    Definition.Class {
        Info        = mkYangInfo ("interfaces", "Interface configuration")
        Name        = None
        Presence    = false
        Body        =
            [
                Definition.Class interface_class
                mkMemberListWithKey ("interface", Some "Physical interface", Type.Class interface_class, interface_class_name)
            ]
    }

let system : Definition = Definition.Class {
    Info        = mkYangInfo ("simple", "Logical systems")
    Name        = None
    Presence    = false
    Body        = [ interfaces ]
}

let configuration : Definition = Definition.Class {
    Info        = mkYangInfoSimple "configuration"
    Name        = None
    Presence    = false
    Body        =
        [
            mkMember ("version", None, mkTypeStringSimple None)
            system
        ]
}
