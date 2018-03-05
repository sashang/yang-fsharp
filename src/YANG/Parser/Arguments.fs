// Tokens.fs
// Parsing of basic tokens of the YANG model

namespace Yang.Parser

module Arguments =
    open System
    open FParsec
    open Yang.Model.Arguments

    // Below we define a custom date field. We could have used the system DateTime,
    // but that gives more information (time) that specified by the grammar.

    /// Parses a YANG date
    let parse_date<'a> : Parser<Date, 'a> =
        // From [RFC 7950, page 206]
        // date-arg-str        = < a string that matches the rule >
        //                       < date-arg >
        // date-arg            = 4DIGIT "-" 2DIGIT "-" 2DIGIT

        regex "\d{4}\-\d{2}-\d{2}"
        |>> (fun date_string ->
            let value index = int (date_string.Chars index) - int ('0')
            let year = (value 0) * 1000 + (value 1) * 100 + (value 2) * 10 + value 3
            let month = (value 5) * 10 + value 6
            let day = (value 8) * 10 + value 9
            Date.Make (year, month, day)
        )
