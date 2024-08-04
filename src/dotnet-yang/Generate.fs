module DotnetYang.Generate

open Yang.Parser.Module
open Yang.Model
open Fantomas.Core.SyntaxOak
open Fabulous.AST





let generateModel (model: ModelUnit) =
    Ast.Oak() {
        Ast.AnonymousModule() {
            ()
        }
    }
