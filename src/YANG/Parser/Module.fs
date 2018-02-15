// Generic.fs

namespace Yang.Parser

/// Parser for the module statement
module Module =
    open FParsec

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

    type Statement =
    | Unparsed of string

    type Module =
        {
            Name : Identifier.Identifier
            Statement : Statement list
        }

    /// Parser for the module statement
    let module_parser<'a> : Parser<Module, 'a> =
        let parser =
            spaces >>. skipStringCI "module" >>. spaces >>.
            Identifier.parse_identifier .>> spaces .>>.
            Generic.unparsed_block_literal
        parser |>> (
            fun (identifier, block) ->
                {
                    Name = identifier
                    Statement = [ Unparsed (block.Trim()) ]
                }
        )

