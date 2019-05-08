// Expressions.fs
namespace Yang.Model

/// Definition of Yang expressions
[<AutoOpen>]
module Expressions =
    open System.Text

    // [RFC7950, p.188]
    //if-feature-expr     = if-feature-term
    //                        [sep or-keyword sep if-feature-expr]
    //
    //if-feature-term     = if-feature-factor
    //                        [sep and-keyword sep if-feature-term]
    //
    //if-feature-factor   = not-keyword sep if-feature-factor /
    //                        "(" optsep if-feature-expr optsep ")" /
    //                        identifier-ref-arg

    /// Expression that may appear in an if-feature statement
    type Expression = Term list
    /// Term of an if-feature expression
    and Term        = Factor list
    /// Factor of an if-feature term
    and Factor      =
    | Not        of Factor
    | Expression of Expression
    | Identifier of IdentifierReference

    let FactorAsIdentifier = function
    | Factor.Identifier identifier  -> Some identifier
    | _                             -> None

    let FactorAsExpression = function
    | Factor.Expression expression  -> Some expression
    | _                             -> None

    /// Shallow check of whether the factor is a negation; it will not search to see whether it can
    /// be reduced to negation.
    let IsFactorNegation = function
    | Not _     -> true
    | _         -> false

    let rec IsSimpleExpr (expr : Expression) =
        match expr with
        | []            -> true
        | [ term ]      -> IsSimpleTerm term
        | _             -> false
    and IsSimpleTerm (term : Term) =
        match term with
        | []            -> true
        | [ factor ]    -> IsSimpleFactor factor
        | _             -> false
    and IsSimpleFactor (factor : Factor) =
        match factor with
        | Not _         -> false
        | Identifier _  -> true
        | Expression _  -> false

    let PrettyPrint (sb : StringBuilder, _ : int) (expr : Expression) =
        let rec print_expression (expression : Expression) =
            match expression with
            | []        -> ()
            | [ term ]  -> print_term term
            | hd :: tl  ->
                print_term hd
                tl |> List.iter (
                    fun term ->
                        Printf.bprintf sb " or "
                        print_term term
                )
        and print_term (term : Term) =
            match term with
            | []            -> ()
            | [ factor ]    ->
                let parenthesize =
                    if IsSimpleFactor factor then false
                    else
                        match factor with
                        | Expression ([ _:: _ ]) ->
                            // the inner expression will use parenthesis
                            false
                        | Expression ( [ term ] ) ->
                            match term with
                            | [ (Not _) ]   -> false
                            | _             -> true
                        | Not _ -> false
                        | _     -> true

                if parenthesize then Printf.bprintf sb "("
                print_factor factor
                if parenthesize then Printf.bprintf sb ")"
            | hd :: tl      ->
                Printf.bprintf sb "("
                print_factor hd
                tl |> List.iter (
                    fun factor ->
                        Printf.bprintf sb " and "
                        print_factor factor
                )
                Printf.bprintf sb ")"
        and print_factor (factor : Factor) =
            match factor with
            | Not factor'           ->
                Printf.bprintf sb "(not "
                print_factor factor'
                Printf.bprintf sb ")"
            | Expression expression ->
                print_expression expression
            | Identifier identifier ->
                Printf.bprintf sb "%s" identifier.Value

        let is_simple = IsSimpleExpr expr
        if is_simple = false then Printf.bprintf sb "'"
        print_expression expr
        if is_simple = false then Printf.bprintf sb "'"

    let ToString (expr : Expression) =
        let sb = StringBuilder ()
        PrettyPrint (sb, 0) expr
        sb.ToString()
