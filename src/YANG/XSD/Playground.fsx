#r "System.Xml.dll"
#r "System.Xml.Linq.dll"

#r @"..\XmlHelper\bin\Debug\Yang.XmlHelper.dll"

open System
open System.IO
open System.Xml
open System.Xml.Linq
open FSharp.Collections

open Yang.XmlHelper.Reader

// Define your library scripting code here
let input = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "Models-External", "Juniper", "XSD", "juniper-netconf.xsd")

// For XSD, see:
// - https://msdn.microsoft.com/en-us/library/aa468557.aspx

let streq string1 string2 = String.Equals(string1, string2, StringComparison.InvariantCultureIgnoreCase)

type Namespace =
| XML2001
| Generic of Uri
with
    static member Make (definition : string) =
        if streq "http://www.w3.org/2001/XMLSchema" definition then XML2001
        else Generic (Uri(definition))

type Schema = {
    Namespace : Map<string, Namespace>
    XsdPrefix : string
    Qualified : bool
}
with
    static member Empty = {
        Namespace = Map.empty
        XsdPrefix = ""
        Qualified = false
    }

    member this.MakeElementName (element : string) =
        if this.Qualified then sprintf "%s:%s" this.XsdPrefix element
        else element

type XsdHelperException (message:string, ?innerException:exn) =
    inherit ApplicationException(
        message,
        match innerException with | Some ex -> ex | _ -> null)

let throw fmt =
    let do_throw message = raise (XsdHelperException message)
    Printf.ksprintf do_throw fmt


let check_local_name (name : string) (reader : XmlReaderHelper) =
    if streq name reader.LocalName then ()
    else throw "Expected local name to be %s; found %s" name reader.LocalName

let check_statement_begin (reader : XmlReaderHelper) =
    if reader.NodeType <> XmlNodeType.Element then
        throw "Expected beginning of element; found %A" reader.NodeType

type EndStatement = XmlReaderHelper -> unit

let expected_end (label : string) (reader : XmlReaderHelper) =
    if reader.IsElementEnd label = false then
        throw "Expected ending element for %s; found '%s'" label reader.CurrentState

let parse_header (reader : XmlReaderHelper) (work : EndStatement list) =
    check_local_name "schema" reader
    check_statement_begin reader

    let rec process_attribute (result : Schema) =
        let result' =
            if streq "xmlns" reader.Prefix then
                let ns = Namespace.Make reader.Value
                if ns = Namespace.XML2001 then
                    { result with
                        Namespace = Map.add reader.LocalName ns result.Namespace
                        XsdPrefix = reader.LocalName
                    }
                else
                    { result with
                        Namespace = Map.add reader.LocalName ns result.Namespace }
            elif streq "elementFormDefault" reader.Name then
                if streq "qualified" reader.Value then
                    { result with Qualified = true }
                elif streq "unqualified" reader.Value then
                    { result with Qualified = false }
                else
                    throw "Unknown value for qualified name control: %s" reader.Value
            else
                throw "Unknown attribute for schema element: %s, with value %s" reader.Name reader.Value

        if reader.NextAttribute () then process_attribute result'
        else result'

    let schema =
        if reader.MoveToAttributes() then
            process_attribute Schema.Empty
        else Schema.Empty

    schema, (expected_end reader.Name) :: work

type Type =
| String
| Decimal
| Integer
| Boolean
| Date
| Time
| Unspecified

type Import = | Import of Location:string * Namespace:Uri
with
    static member Make (location : string, ns : string) =
        Import (location, Uri(ns))

type ComplexType = NA

type Node =
| Import        of Import
| Element       of Element
| ComplexType   of ComplexType
and Element = {
    Name    : string
    Type    : Type
    Default : string option
    Fixed   : string option
}

let reader = XmlReaderHelper.Parse (File.ReadAllText input)

let schema, work = parse_header reader []
reader.Forward()

let parse_import_statement () =
    reader.CheckElementBegin (schema.MakeElementName "import")
    let location = reader.Reader.GetAttribute("schemaLocation")
    let ns = reader.Reader.GetAttribute("namespace")
    Import.Make (location, ns)

parse_import_statement ()

reader.Forward()
reader.CurrentState
reader.Reader
