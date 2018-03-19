// Program.fs
// Intermediate representation for the library that will be generated
namespace Yang.Generator

module Program =
    open Yang.Model

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

    type Type =
    | String of StringType
    | Class of string

    type YangInfo = | YangInfo of Name:string * Description:(string option)

    type Definition =
    | Class         of ClassDefinition
    | Member        of MemberDefinition * Type
    | ListWithKey   of Member:MemberDefinition * Type:Type * Key:MemberDefinition
    and ClassDefinition = {
        Info            : YangInfo
        Name            : string option
        Body            : Definition list
    }
    and MemberDefinition = {
        Info            : YangInfo
        Name            : string option
        Optional        : bool
    }

