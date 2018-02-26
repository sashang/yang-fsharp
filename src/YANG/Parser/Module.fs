// Generic.fs

namespace Yang.Parser

/// Parser for the module statement
module Module =
    open FParsec
    open System

    // The module statement is defined in RFC 7950, page 184:
    // module-stmt     = optsep module-keyword sep identifier-arg-str
    //                   optsep
    //                   "{" stmtsep
    //                       module-header-stmts
    //                       linkage-stmts
    //                       meta-stmts
    //                       revision-stmts
    //                       body-stmts
    //                   "}" optsep
    // [..., page 206]
    // module-keyword           = %s"module"
    // [..., page 208]
    // identifier-arg-str  = < a string that matches the rule >
    //                       < identifier-arg >
    // identifier-arg      = identifier
    //
    // with:
    // RFC 7950, page 209:
    // stmtsep             = *(WSP / line-break / unknown-statement)
    // line-break          = CRLF / LF
    // optsep              = *(WSP / line-break)
    // (..., p. 210)
    // WSP                 = SP / HTAB
    //                       ; whitespace
    // SP                  = %x20
    //                       ; space
    // HTAB                = %x09
    //                       ; horizontal tab
    // CRLF                = CR LF
    //                       ; Internet standard newline
    // CR                  = %x0D
    //                       ; carriage return
    // LF                  = %x0A
    //                       ; line feed
    //
    // The unknown-statement is defined as follows:
    // RFC 7950, page 202:
    // unknown-statement   = prefix ":" identifier [sep string] optsep
    //                       (";" /
    //                        "{" optsep
    //                            *((yang-stmt / unknown-statement) optsep)
    //                         "}") stmtsep
    // [..., page 208]
    // prefix              = identifier
    //
    // Some remarks:
    //
    // TODO: Parsing unknown-statement declarations, see Note-03.

    // TODO: define the type of imports
    type Imports = string

    // TODO: define the type of includes
    type Includes = string

    type Module =
        {
            // Module header statements

            /// Name of module
            Name : Identifier.Identifier

            // Header information
            Header : Header

            // Linkage statements
            Imports : Imports option
            Includes : Includes option

            // Meta statements
            Meta : Meta option

            // Revision statements
            Revisions : Revision list option

            // Rest of body statements
            Statements : Body
        }
    with
        /// Creates an empty (invalid) module
        static member Empty = {
            Name = Identifier.Identifier.MakeUnchecked "<empty>"
            Header = Header.Empty

            Imports = None
            Includes = None

            Meta = None
            Revisions = None

            Statements = []
        }

        /// Retrieve the most recent revision of the module
        member this.CurrentRevision =
            match this.Revisions with
            | Some revisions -> Some (revisions |> List.maxBy (fun v -> v.Version))
            | None           -> None

    /// Parser for the module statement
    let parse_module<'a> : Parser<Module, 'a> =
        let parser =
            spaces .>> skipStringCI "module" .>> spaces >>.
            Identifier.parse_identifier .>> spaces .>>
            skipChar '{' .>> spaces .>>.
            tuple4 parse_header (opt parse_meta) (opt parse_revision_list) parse_body .>>
            spaces .>> skipChar '}' .>> spaces
        parser |>> (
            fun (identifier, (header, meta, revision, body)) ->
                // We need the two adjustments below, because the parsers
                // are guaranteed to return a value, even if the values are empty.

                let meta' =
                    match meta with
                    | None      -> None
                    | Some v    -> if Object.ReferenceEquals(null, v) then None else meta

                let revision' =
                    match revision with
                    | None
                    | Some []   -> None
                    | _         -> revision

                { Module.Empty with
                    Name        = identifier
                    Header      = header
                    Meta        = meta'
                    Revisions   = revision'
                    Statements  = body
                }
        )

