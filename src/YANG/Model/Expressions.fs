// Expressions.fs
namespace Yang.Model

/// Definition of Yang expressions
[<AutoOpen>]
module Expressions =
    open System.Text

    /// If feature expression
    // TODO: Expand definition of IfFeatureExpression
    type IfFeatureExpression = |NA

    let PrettyPrint (sb : StringBuilder, indent : int) (expr : IfFeatureExpression) =
        Printf.bprintf sb "<Not Implemented>"
