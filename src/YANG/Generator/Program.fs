﻿// Program.fs
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

    type YangInfo = | YangInfo of Name:string * Description:(string option)

    type Definition =
    | Class         of ClassDefinition
    | Member        of MemberDefinition * Type
    | ListWithKey   of Member:MemberDefinition * Type:Type * Key:MemberDefinition
    and Type =
    | String    of StringType
    | UInt32    of UInt32Type
    | Class     of ClassDefinition
    and ClassDefinition = {
        Info            : YangInfo
        mutable Name    : string option
        Presence        : bool
        Body            : Definition list
    }
    and MemberDefinition = {
        Info            : YangInfo
        mutable Name    : string option
        Optional        : bool
    }

