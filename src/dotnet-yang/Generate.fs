module DotnetYang.Generate

open Yang.Parser.Module
open Yang.Model
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Fabulous.AST
open Fabulous.AST.StackAllocatedCollections
open Fabulous.AST.StackAllocatedCollections.MutStackArray1
open type Fabulous.AST.Ast

let typeStatementIdRefToBasicType value =
    match value with
    | "string" -> String()
    | "float" -> Float()
    | "decimal" -> Decimal()
    | _ -> Float()

let generateLeafStatement (leaf: Statements.LeafStatement) =
    let (LeafStatement (identifier, leafBodyStatements)) = leaf
    let leafNodes = [
        for lbStatement in leafBodyStatements do
            match lbStatement with
            | LeafBodyStatement.Type typeStatement ->
                let (TypeStatement(idRef, typeBodyStatementOpt)) = typeStatement
                Field(identifier.Value, (typeStatementIdRefToBasicType idRef.Value))
            | _ ->
                Field(identifier.Value, Float())
    ]
    leafNodes

let generateContainer (container: Statements.ContainerStatement) =

    let (ContainerStatement (identifier, containerBodyStatementsOpt)) = container

    Record(identifier.Value) { // enter a computation expression
        match containerBodyStatementsOpt with
        | Some containerBodyStatements ->
            for body in containerBodyStatements do
                match body with
                | ContainerBodyStatement.Leaf leafStatement ->
                    yield! (generateLeafStatement leafStatement |> List.map (fun leafNode -> (leafNode)))
                    // for leafNode in generateLeafStatement leafStatement do
                    //     leafNode // note that leafNode is yielded here into the computation expression of the Record.
                | _ -> () // this is an empty AST node and not a unit because it is in a ce
        | None -> ()
    }

let generateBodyStatement (body: BodyStatement ) =
    match body with
    | Container containerStatement -> generateContainer containerStatement
    | _ -> Record(""){ () }



let generateModuleStatement (moduleStatement: ModuleStatement) =
    [
        for body in moduleStatement.Body do
            generateBodyStatement body
    ]

let generateSubmoduleUnit (submodule: SubmoduleStatement) =
    ()


let genValue (identifier: string, value: string) =
    Value(identifier, value)

let generateModel (model: ModelUnit) =
    Oak() {
        AnonymousModule() {
            match model with
            | ModuleUnit moduleStatement ->
                for widget in generateModuleStatement moduleStatement do
                    widget
            | SubmoduleUnit submoduleStatement -> generateSubmoduleUnit submoduleStatement
        }
    }
