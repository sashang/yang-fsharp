module DotnetYang.Intermediate

open Yang.Parser.Module
open Yang.Model
open Fantomas.Core.SyntaxOak
open Fabulous.AST

type BasicType =
    | Undefined
    | String
    | Float
    | Decimal
    | Int16
and RecordFieldSubType =
    { Identifier: string; Type: BasicType }
and RecordField =
    | Undefined
    | RecordField of RecordFieldSubType
and RecordSubType =
    { Identifier: string;
      Fields: RecordField list }
and Record =
    | Undefined
    | Record of RecordSubType
and ModuleElement =
    | Undefined
    | MEUnion
    | MERecord of Record
and Module =
    Elements of ModuleElement list

let makeFSharpBasicType value =
    match value with
    | "string" -> String
    | "float" -> Float
    | "decimal" -> Decimal
    | "int8" -> Int16
    | _ -> BasicType.Undefined

// convert a Yang leaf statement to a record field.
let private makeRecordField (leaf: Statements.LeafStatement) =
    let (LeafStatement (leafIdentifier, leafBodyStatements)) = leaf

    // a leaf only has one type statement according to the grammar https://www.rfc-editor.org/rfc/rfc7950
    // it must be there and there is only one of them, and if the parser has done it's job
    // then this premise should always be true. A failure indicates an error in parsing.
    let typeStatement =
        leafBodyStatements
        |> List.pick
            (fun statement ->
                match statement with
                | LeafBodyStatement.Type lt -> Some lt
                | _ -> None)

    let (TypeStatement(idRef, typeBodyStatementOpt)) = typeStatement
    RecordField ({ Identifier = leafIdentifier.Value; Type = makeFSharpBasicType idRef.Value })

// takes a container statement and converts it to a Record
let private makeRecord (containerStatement: ContainerStatement) =

    let (ContainerStatement (identifier, containerBodyStatementsOpt)) = containerStatement

    let result =
        [
            match containerBodyStatementsOpt with
            | Some containerBodyStatements ->
                for body in containerBodyStatements do
                    match body with
                    | ContainerBodyStatement.Leaf leafStatement ->
                        makeRecordField leafStatement
                    | _ -> RecordField.Undefined
            | None -> RecordField.Undefined
        ] |> List.filter
                (fun item ->
                    match item with
                    | RecordField.Undefined -> false
                    | _ -> true )
    let r = {RecordSubType.Identifier = identifier.Value; Fields = result }
    Record.Record r


let private makeModuleElement (bodyStatement: BodyStatement) =
    match bodyStatement with
    // a Yang container maps to a F# record
    | Container containerStatement -> MERecord (makeRecord containerStatement)
    // undefined represents the fact we don't know what to do with the element yet.
    | _ -> ModuleElement.Undefined

let private makeModule (moduleStatement: ModuleStatement) =
    [
        for bodyStatement in moduleStatement.Body do
            makeModuleElement bodyStatement
    ]


let makeIntermediateRepresentation (model: ModelUnit) =
    match model with
    | ModuleUnit moduleStatement ->
        makeModule moduleStatement
    | _ -> []
