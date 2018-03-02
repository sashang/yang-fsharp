// Revisions.fs
// Parsing and handling revisions

namespace Yang.Parser

/// Parsing and processing module revision information
[<AutoOpen>]
module Revisions =
    open System
    open FParsec
    open Identifier

    /// Module Revision Information
    type Revision = {
        /// Version information of revision
        Version : Tokens.Date

        /// Description of revision, if available
        Description : DescriptionStatement option

        /// Cross reference related to revision, if available
        Reference : ReferenceStatement option

        /// Extra unparsed statements that appear in the body.
        /// (those statements are kept in order, but caller should
        /// not make any assumption of position of description and
        /// reference statements)
        Options : ExtraStatements
    }
    with
        /// Creates an empty revision entry
        static member Empty = {
            Version     = Tokens.Date.Make(DateTime.Now)
            Description = None
            Reference   = None
            Options     = None
        }

        /// Creates a revision entry from revision date
        static member Make version = {
            Version     = version
            Description = None
            Reference   = None
            Options     = None
        }

    /// Type of statements that may exist in the revision block
    type private RevisionBlockStatement =
    | Description   of DescriptionStatement
    | Reference     of ReferenceStatement
    | Unknown       of UnknownStatement

    /// Parse a statement in the body of a revision
    let private parse_revision_body_statement<'a> : Parser<RevisionBlockStatement, 'a> =
            (parse_description_statement    |>> Description)
        <|> (parse_reference_statement      |>> Reference)
        <|> (parse_unknown_statement        |>> Unknown)

    /// Parses the entire body of a revision
    let private parse_revision_body<'a> : Parser<Revision, 'a> =
        // TODO: check that there is at most one description and reference statements.
        // [RFC 7950, Sec. 7.1.9.1, p. 60] specifies that in the body of the revision
        // should be at most one description and reference statements.
        // Below we ignore that constrain and let the last statement survive.

        /// Element parser
        let elementParser = spaces >>. parse_revision_body_statement

        /// Create the state from the first element
        let stateFromFirstElement = function
            | Description   e   -> { Revision.Empty with Description   = Some e }
            | Reference     e   -> { Revision.Empty with Reference     = Some e }
            | Unknown       e   -> { Revision.Empty with Options       = Some [ Statements.Unknown e ] }

        /// Update state from element
        let foldState (state : Revision) element =
            match element with
            | Description   e   -> { state with Description   = Some e }
            | Reference     e   -> { state with Reference     = Some e }
            | Unknown       e   ->
                match state.Options with
                | None          -> { state with Options = Some [ Statements.Unknown e ] }
                | Some op       -> { state with Options = Some ( op @ [Statements.Unknown e] ) }

        Inline.Many(
            elementParser           = elementParser,
            stateFromFirstElement   = stateFromFirstElement,
            foldState               = foldState,
            resultFromState         = id
        )

    /// Parser for the revision statement
    let parse_revision<'a> : Parser<Revision, 'a> =
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

        skipStringCI "revision" >>. spaces >>.
        Tokens.parse_date .>> spaces .>>.
        (     (skipChar ';' .>> spaces |>> (fun _ -> Revision.Empty))
          <|> (skipChar '{' .>> spaces >>. parse_revision_body .>> spaces .>> skipChar '}' .>> spaces)
        )
        |>> (fun (revision, body) ->
            { body with Version = revision }
        )

    /// Parses all revision statements
    let parse_revision_list<'a> : Parser<Revision list, 'a> =
        many (parse_revision .>> spaces)
