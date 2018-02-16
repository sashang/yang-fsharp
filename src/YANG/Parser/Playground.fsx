// Playground.fsx
// Testing of parser functionality during development.

#load @"../../../.paket/load/net471/FParsec-Big-Data-Edition.fsx"
#load @"../../../.paket/load/net471/NLog.fsx"

#load "Utilities.fs"
#load "Errors.fs"
#load "Comments.fs"
#load "Strings.fs"
#load "Generic.fs"
#load "Identifier.fs"
#load "Module.fs"
#load "Parser.fs"

open System.IO
open System.Text
open FParsec
open Yang.Parser

let sample_dir = Path.Combine(__SOURCE_DIRECTORY__, @"../Examples/")
Directory.Exists(sample_dir)

let example = Path.Combine(sample_dir, @"RFC7950/example-system.yang")

let ReadAndClean filename =
    use reader = new StreamReader(example)
    let sb = StringBuilder()
    use writer = new StringWriter(sb)
    Comments.Remove(reader, writer)
    sb.ToString()

let model = ReadAndClean example
run Module.module_parser model

let empty_module = """
module empty {
}
"""

run Module.module_parser empty_module

run Strings.parse_string "test"
run Strings.parse_string "'test 123 !£$%'"
run Strings.parse_string "\"string with space\""
run Strings.parse_string "\'string with space\'"
run Strings.parse_string "\"Test \\\" string\""
run Strings.parse_string "\"string A \" + \"and string B\""
run Strings.parse_string "'string A ' + \"and string B\""
run Strings.parse_string "'string A ' + 'and string B'"

let multiline_string = """
        "the message starts here and
         continues to the next line"
"""

let multiline_string_2 = """
        "the message starts here and " +
        "continues to the next line" + " and a bit more"
"""

let multiline_string_concatenated = """
        "the message starts here and
         continues to the next line " +
     "and continues here
      and there"
"""

run (spaces >>. Strings.parse_double_quoted_string) multiline_string
run (spaces >>. Strings.parse_string .>> spaces) multiline_string
run (spaces >>. Strings.parse_string .>> spaces) multiline_string_2
run (spaces >>. Strings.parse_string .>> spaces) multiline_string_concatenated


