// Playground.fsx
// Testing of parser functionality during development.

#load "Initialize.fsx"

open FParsec
open Yang.Parser
open Yang.Model.Statements

open Initialize
open Logging
open Yang.Model.Identifier

//
// Test code to parse files
//
let model = get_sample_model @"RFC7950/example-system.yang"
apply_parser Module.parse_module model

let big_model = get_external_model @"Juniper\16.1\configuration.yang"

// This is what we want to parse eventually
MyLog.myLog.AddTrace(Header._name)
MyLog.myLog.AddTrace(Types._name)
let juniper = apply_parser Module.parse_module big_model

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

apply_parser BodyStatements.parse_container_statement configuration
apply_parser BodyStatements.parse_body_statement configuration


let input = "'1 .. 128'"
apply_parser Arguments.parse_length input

let input = """length "1 .. 128";"""
apply_parser parse_length_statement input

let input = """type string;"""
apply_parser Types.parse_type_statement input

let input = """type string {}"""
apply_parser Types.parse_type_statement input

let input = """type string {
             length "1 .. 128";
           }"""
apply_parser Types.parse_type_statement input

let unknown1 = """junos:posix-pattern "^.{1,64}$";"""
apply_parser parse_unknown_statement unknown1

let input = """type string {
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
           }"""
apply_parser Types.parse_type_statement input

// The following is wrong!
let input = """type string {
             length "1 .. 128";
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
           }"""

let input = """type string {
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
             length "1 .. 128";
           }"""

let input = """type string {
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
             length "1 .. 64";
             length "1 .. 128";
           }"""

let input = """type string {
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
             length "1 .. 64";
             pattern "^.{1,64}$";
           }"""

let input = """type string {
             junos:posix-pattern "^.{1,64}$";
             junos:pattern-message "Must be string of 64 characters or less";
             length "1 .. 64";
             pattern "^.{1,62}$";
             pattern "^.{1,64}$";
             pattern "^.{1,60}$";
           }"""


let parse_type_body_string_restrictions<'a> : Parser<StringRestrictions, 'a> =
    // [RFC 7950, p. 189]
    //string-restrictions = ;; these stmts can appear in any order
    //                        [length-stmt]
    //                        *pattern-stmt
        (parse_length_statement     |>> fun v -> Some v, [])
    <|> (parse_pattern_statement    |>> fun v -> None,   [v])


let parse_type_body_restriction_statement<'a> (``type`` : string) : Parser<TypeBodyStatement, 'a> =
    let restrictions =
        Map.ofList [
            ("string",  parse_type_body_string_restrictions     |>> TypeBodyStatement.StringRestrictions)
            //("int8",    parse_type_body_numerical_restrictions  |>> TypeBodyStatement.NumericalRestrictions)
            //("int16",   parse_type_body_numerical_restrictions  |>> TypeBodyStatement.NumericalRestrictions)
        ]

    restrictions.Item ``type``

let force_type_name<'a> (``type`` : string) : Parser<IdentifierReference, 'a> =
    pstring ``type`` .>> spaces
    |>> fun v -> Yang.Model.Identifier.IdentifierReference.Make ``type``

let translate_type_restrictions (``type`` : string) (restrictions : TypeBodyStatement list) : TypeBodyStatement option =
    if ``type``.Equals("string") then
        let result =
            restrictions
            |> List.fold (
                fun state restriction ->
                    match state, restriction with
                    | (None, patterns), StringRestrictions (Some length, [])    -> Some length, patterns
                    | (None, _), StringRestrictions (Some length, _)            ->
                        failwith "Internal error: expected either length restriction or pattern; got both"
                    | (Some _, _),      StringRestrictions (Some _, _)          ->
                        failwith "Error: only one length restriction statement allowed"
                    | (length, patterns), StringRestrictions (None, pattern)    -> length, patterns @ pattern
                    | _ ->
                        failwith "Only string restrictions allowed for string type"
            ) (None, [])

        match result  with
        | None, []      -> None
        | _             -> Some (TypeBodyStatement.StringRestrictions result)

    else
        failwith (sprintf "Support for type %s not implemented" ``type``)

let try_parse_type_restrictions<'a, 'T> (restrictions : Parser<'T, 'a>) : Parser<(UnknownStatement list option) * ('T list option), 'a> =
    let rec parse_unknowns (stream : CharStream<'a>) (success : bool, result : UnknownStatement list) =
        let state = stream.State
        let reply = parse_unknown_statement stream
        if reply.Status = Ok then
            parse_unknowns stream (true, reply.Result :: result)
        else
            stream.BacktrackTo state
            success, result |> List.rev

    fun stream ->
        let success, unknowns = parse_unknowns stream (false, [])
        let state = stream.State
        let reply = (many restrictions) stream

        match success, reply.Status with
        | true, Ok  ->
            // Observe that below we assume that unknowns is a non-empty list
            let result = Some unknowns, Some reply.Result
            Reply result

        | false, Ok ->
            let result = None, Some reply.Result
            Reply result

        | true, _   ->
            stream.BacktrackTo state
            // Observe that below we assume that unknowns is a non-empty list
            let result = Some unknowns, None
            Reply result

        | false, _  ->
            stream.BacktrackTo state
            Reply(Error, messageError "Neither parser managed to make progress")


let parse_type_implementation_statement<'a> (``type`` : string) : Parser<(TypeBodyStatement option) * (UnknownStatement list option), 'a> =
        (skipChar ';' >>. wse   |>> fun _ -> None, None)
    <|> (skipChar '{' >>. wse >>.
         (try_parse_type_restrictions (parse_type_body_restriction_statement ``type``)) .>>
         skipChar '}' .>> wse
        )
    |>> fun (unknowns, restrictions) ->
            let unknowns' =
                if unknowns.IsSome && unknowns.Value.Length = 0 then None
                else unknowns
            if restrictions.IsNone then None, unknowns
            else (translate_type_restrictions ``type`` restrictions.Value), unknowns'

let parse_type_statement<'a> : Parser<TypeStatement, 'a> =
    skipString "type" >>. spaces >>.
    (
            (force_type_name "string" .>>. parse_type_implementation_statement "string")
    )
    |>> fun (id, (restriction, unknowns)) -> id, restriction, unknowns

apply_parser parse_type_statement input
