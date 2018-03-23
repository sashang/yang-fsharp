// Revisions.fs
// Parsing and handling revisions

namespace Yang.Parser

/// Parsing and processing module revision information
[<AutoOpen>]
module Revisions =
    open FParsec
    open Yang.Model

    /// Parse a statement in the body of a revision
    let private parse_revision_body_statement<'a> : Parser<RevisionBodyStatement, 'a> =
            (parse_description_statement    |>> RevisionBodyStatement.Description)
        <|> (parse_reference_statement      |>> RevisionBodyStatement.Reference)
        <|> (parse_unknown_statement        |>> RevisionBodyStatement.Unknown)

    /// Parses the entire body of a revision
    let private parse_revision_body<'a> : Parser<RevisionBodyStatement list, 'a> =
        /// Element parser
        let elementParser = parse_revision_body_statement

        /// Create the state from the first element
        let stateFromFirstElement = function
        | RevisionBodyStatement.Description _ as e  -> [e], (true, false)
        | RevisionBodyStatement.Reference   _ as e  -> [e], (false, true)
        | RevisionBodyStatement.Unknown     _ as e  -> [e], (false, false)

        /// Update state from element
        let foldState state element =
            match state, element with
            | (v, (false, re)), RevisionBodyStatement.Description _     -> v @ [element], (true, re)
            | (_, (true, _)),   RevisionBodyStatement.Description _     ->
                raise (YangParserException "Detected duplicate description statement in revision info")

            | (v, (de, false)), RevisionBodyStatement.Reference _       -> v @ [element], (de, true)
            | (_, (_, true)),   RevisionBodyStatement.Reference _       ->
                raise (YangParserException "Detected duplicate description statement in reference info")

            | (v, st),          RevisionBodyStatement.Unknown _         -> v @ [element], st

        let result (v, _) = v

        Inline.Many(
            elementParser           = elementParser,
            stateFromFirstElement   = stateFromFirstElement,
            foldState               = foldState,
            resultFromState         = result
        )

    /// Parser for the revision statement
    let parse_revision<'a> : Parser<RevisionStatement, 'a> =
        // From [RFC 7950, page 186]
        // revision-stmt       = revision-keyword sep revision-date optsep
        //                       (";" /
        //                        "{" stmtsep
        //                            ;; these stmts can appear in any order
        //                               [description-stmt]
        //                               [reference-stmt]
        //                        "}") stmtsep
        //
        // revision-date       = date-arg-str
        //
        // In this case, we parse the entire body with the method above. We do that to
        // also check the validity of the statements.
        skipString "revision" >>. spaces >>.
        Arguments.parse_date .>> spaces .>>.
        (     (skipChar ';' .>> wse |>> (fun _ -> None))
          <|> (skipChar '{' .>> wse >>. (parse_revision_body |>> Some) .>> wse .>> skipChar '}' .>> wse)
        )
        |>> RevisionStatement

    /// Parses all revision statements
    let parse_revision_list<'a> : Parser<RevisionStatement list, 'a> =
        many parse_revision
