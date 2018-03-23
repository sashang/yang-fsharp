// Generator.fs
// Helper methods for creating models programmatically
namespace Yang.Model

/// Helper methods for creating simple models programmatically.
/// This is intended for quick experimentation and testing.
module Generator =
    open System

    let mkId    (id : string) = Identifier.Make id
    let mkIdRef (id : string) = IdentifierReference.Make id

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
    let mkType name body : TypeStatement = (mkIdRef name), body

    let mkTypeDefFromString name : BodyStatement =
        let body : TypeDefBodyStatement = mkType "string" None |> TypeDefBodyStatement.Type
        (mkId name, [ body ]) |> BodyStatement.TypeDef

    let mkLeaf name ``type`` : LeafStatement =
        let t = LeafBodyStatement.Type (mkIdRef ``type``, None)
        let leaf : LeafStatement = mkId name, [ t ]
        leaf

    let mkList name body : ListStatement = mkId name, body
    let mkPresence label = ContainerBodyStatement.Presence (label, None)
