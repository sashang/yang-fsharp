// Playground.fsx
// Testing of parser functionality during development.

#load "Initialize.fsx"

open FParsec
open Yang.Parser

open Initialize
open Logging

//
// Test code to parse files
//
let model = get_sample_model @"RFC7950/example-system.yang"
apply_parser Module.parse_module model

let big_model = get_external_model @"Juniper\16.1\configuration.yang"

// This is what we want to parse eventually
MyLog.myLog.AddTrace(Header._name)
MyLog.myLog.AddTrace(Types._name)
let juniper = apply_parser (spaces >>. Module.parse_module) big_model

#time
// Typical statistics: Real: 00:00:14.234, CPU: 00:00:14.203, GC gen0: 1231, gen1: 1160, gen2: 0
let juniper_def_use = Yang.Model.DefUseResolver.VisitDefinitions (fun _ -> true) (Yang.Model.Statements.Module juniper)
printfn "Length: %d" juniper_def_use.Length


// File:Models-External\Juniper\17.2\17.2R1\junos\configuration.yang        Line:86639, 86672, 86705, 86738, 89183
// File:Models-External\Juniper\17.2\17.2R1\junos-es\configuration.yang     Line:61494
// File:Models-External\Juniper\17.2\17.2R1\junos-ex\configuration.yang     Line:44820
// File:Models-External\Juniper\17.2\17.2R1\junos-qfx\configuration.yang    Line:47896
// File:Models-External\Juniper\17.3\17.3R1\junos\configuration.yang        Line:98101, 98134, 98167, 98200, 100879
// File:Models-External\Juniper\17.3\17.3R1\junos-es\configuration.yang     Line:68797
// File:Models-External\Juniper\17.3\17.3R1\junos-ex\configuration.yang     Line:49429
// File:Models-External\Juniper\17.3\17.3R1\junos-qfx\configuration.yang    Line:53134
// File:Models-External\Juniper\17.4\17.4R1\junos\conf\junos-conf-class-of-service@2017-01-01.yang      Line: 1168, 1201, 1234


let configuration = """container configuration {
     uses juniper-config;
     list groups {
     }
   }"""

let _ = apply_parser BodyStatements.parse_container_statement configuration
let _ = apply_parser BodyStatements.parse_body_statement configuration


apply_parser Arguments.parse_length "'1 .. 128'"
apply_parser parse_length_statement """length "1 .. 128";"""

apply_parser parse_unknown_statement """junos:posix-pattern "^.{1,64}$";"""



apply_parser parse_length_statement """length "8";"""

apply_parser (pip Strings.parse_string Identifier.parse_schema_node_identifier_absolute) "'/if:interfaces/if:interface'"

let file1 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\draft\interface\bbf-fiber-base.yang"
let file2 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\draft\interface\bbf-fiber-channelpair-body.yang"
let file3 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\draft\interface\bbf-fiber-if-type.yang"
let file4 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\draft\interface\bbf-fiber-types.yang"
let file5 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\standard\interface\bbf-fast-line-performance-body.yang"
let file6 = __SOURCE_DIRECTORY__ + @"../../../../Models-External\BroadbandForum\standard\interface\bbf-interfaces-performance-management.yang"
let filex = @"e:\temp\failed.yang"

Parser.ParseFile file1
Parser.ParseFile file2
Parser.ParseFile file3
Parser.ParseFile file4
Parser.ParseFile file5
Parser.ParseFile file6

Parser.ParseFile filex

// TODO: why does the following work?
apply_parser parse_when_statement """when "../crypto = 'mc:aes'";"""

(*
How do we parse examples from p.169?
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:services/ex:ssh"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:user[ex:name=fred]"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:user[ex:name='fred']"
apply_parser Identifier.parse_schema_node_identifier "/ex:system/ex:server[ex:ip='192.0.2.1'][ex:port='80']"
apply_parser Identifier.parse_schema_node_identifier 
*)


let input = """identity bbf-fiber-interface-type {
    base if:interface-type;
    description
      "This identity is used as a base for all xPON interface types
       defined by the BBF that are not in the 'ifType definitions'
       registry maintained by IANA.";
  }"""


apply_parser parse_identity_statement input

open FParsec
open Yang.Parser.BodyStatements
open Yang.Parser

let bad_type_def = """typedef performance-15min-history-interval {
type performance-15min-interval {
    range "1..96";
}
}"""

let bad_type = """type performance-15min-interval {
}"""
apply_parser Types.parse_type_statement bad_type


apply_parser (spaces >>. parse_typedef_statement) bad_type_def



#r @"E:\temp\qe\FSharp.Quotations.Evaluator.1.1.0\lib\net45\FSharp.Quotations.Evaluator.dll"
#r @"E:\temp\qe\FSharp.Quotations.Evaluator.1.1.0\lib\net45\FSharp.Quotations.Evaluator.Hacks.dll"

open Yang.Model

let mutable generator : System.Type option = None
let implementations = System.Collections.Generic.Dictionary<System.Type, obj>()

let generic_parser<'a> : GenericParser<'a> =
    let mutable initialized = false
    let (parse_statement : Parser<Statement, 'a>), (parse_statement_ref : Parser<Statement, 'a> ref) =
        createParserForwardedToRef<Statement, 'a>()

    let debug_parser : Parser<Statement, 'a> =
        printfn "In debug ..."

        if initialized = false then
            printfn "In here..."

            let key = typeof<'a>
            if implementations.ContainsKey(key) then
                printfn "Found type"
                parse_statement_ref := implementations.[key] :?> Parser<Statement, 'a>
                initialized <- true
            elif generator.IsSome then
                printfn "Constructing type"
                let ty = generator.Value
                let generic = ty.GetGenericTypeDefinition()
                let proper = generic.MakeGenericType(typeof<'a>)
                let method = proper.GetMethod("Parser")
                let parser = method.Invoke(null, [| |])
                implementations.Add(key, parser)
                parse_statement_ref := parser :?> Parser<Statement, 'a>
                initialized <- true
            else
                printfn "Cannot construct type"

        !parse_statement_ref

    {
        Parser          = debug_parser
        Implementation  = parse_statement_ref
    }

type BigGenerator<'a> =
    static member Parser() : Parser<Statement, 'a> = generic_yang_statement_implementation

generator <- Some (typeof<BigGenerator<unit>>)

let parse_statement<'a> = generic_parser<'a>.Parser


apply_parser parse_statement "description 'help';"
apply_parser (many parse_statement) "description 'help';"

// TODO: should give 1 .. 96
apply_parser Arguments.parse_range_part "1..96"


let un = UnknownStatement (IdentifierWithPrefix.Make "t:h", None, None)
StatementPrinter.Reset()
let pr = Printer.YangPrinter ()
pr.Append un
pr.ToString()


open Yang.Model
StatementPrinter.Reset()

let unknown = Statement.Unknown (UnknownStatement.UnknownStatement (IdentifierWithPrefix.Make "t:h", None, None))
let description = DescriptionStatement ("help", None)

pr.Append unknown
pr.ToString()

let description_as_statement = Statement.Description description

let _ = 
    let description_as_statement = Statement.Description description
    ()



// TODO: Fix printing

type XX = | XX of int
let xx = typeof<XX>.GetMethod("ToString")
xx.Module

let oo = typeof<obj>.GetMethod("ToString")
oo.Invoke(description, [| |])
