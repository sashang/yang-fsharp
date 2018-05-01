// XmlHelper.fs
// Helper methods for parsing XML
namespace Yang.XmlHelper

module Reader =
    open System.IO
    open System.Xml
    open System.Xml.Linq

    let private throw fmt =
        let do_throw message = raise (XmlParsingException message)
        Printf.ksprintf do_throw fmt

    type XmlReaderHelper (reader : XmlReader, ?document : XDocument) =
        let advance () =
            if reader.Read() = false then
                throw "Unexpected end of stream"

        let check (label : string, state : XmlNodeType) =
            if reader.Name <> label then
                throw "Expecting element with name: '%s'; got '%s'" label reader.Name
            if reader.NodeType <> state then
                throw "Expecting element with type: '%A'; got '%A'" state reader.NodeType

        let consume_comments_until (label : string) =
            let mutable searching = true
            while searching do
                if reader.NodeType = XmlNodeType.Comment then advance ()
                elif reader.NodeType = XmlNodeType.EndElement && reader.Name = label then
                    searching <- false
                else
                    throw "Unexpected state: %s, %s" reader.Name reader.Value

        let consume_comments () =
            let mutable searching = true
            while searching do
                if reader.NodeType = XmlNodeType.Comment then advance ()
                else searching <- false

        let rec move_to_content () =
            let hasElement = reader.Read()

            if hasElement = false then
                false
            elif reader.NodeType = XmlNodeType.Comment then
                move_to_content ()
            elif reader.NodeType = XmlNodeType.Text then
                true
            else
                false

        let get_content label =
            if move_to_content () then
                let result = reader.Value
                advance ()
                consume_comments_until label
                Some result
            else None

        do advance ()

        static member Parse(input : string) =
            let document = XDocument.Parse(input)
            let reader = document.CreateReader()
            XmlReaderHelper(reader, document)

        static member ParseFromFile(filename : string) =
            XmlReaderHelper.Parse (File.ReadAllText filename)

        member this.ReadString (label : string) =
            check (label, XmlNodeType.Element)
            let result = get_content label
            check (label, XmlNodeType.EndElement)
            reader.Read() |> ignore
            consume_comments ()
            result

        member this.ReadStringValue (label : string) =
            let result = this.ReadString(label)
            if result.IsNone then
                throw "Expected to read a string; got nothing."
            else result.Value

        /// Access the underlying reader (use for experimental purposes only)
        member this.Reader      = reader

        member this.NodeType    = reader.NodeType
        member this.Prefix      = reader.Prefix
        member this.Name        = reader.Name
        member this.LocalName   = reader.LocalName
        member this.Value       = reader.Value
        member this.HasValue    = reader.HasValue
        member this.ValueType   = reader.ValueType

        member this.HasAttributes       = reader.HasAttributes
        member this.MoveToAttributes()  = reader.MoveToFirstAttribute()
        member this.NextAttribute()     = reader.MoveToNextAttribute()
        member this.Attributes          =
            if reader.MoveToFirstAttribute() then
                seq {
                    let mutable more_attributes = true
                    while more_attributes do
                        yield (reader.Name, reader.Value)
                        more_attributes <- reader.MoveToNextAttribute()
                }
            else Seq.empty

        member this.IsElementBegin (name : string) =
            reader.NodeType = XmlNodeType.Element && reader.Name = name
        member this.IsElementEnd (name : string) =
            reader.NodeType = XmlNodeType.EndElement && reader.Name = name

        member this.CheckElementBegin (name : string) =
            if this.IsElementBegin(name) = false then
                throw "XML parsing error: expecting begin '%s', got '%s' (%A)" name reader.Name reader.NodeType

        member this.CheckElementEnd (name : string) =
            if this.IsElementEnd(name) = false then
                throw "XML parsing error: expecting end '%s', got '%s' (%A)" name reader.Name reader.NodeType

        member this.Forward () =
            reader.Read() |> ignore
            consume_comments()

        member this.CurrentState =
            sprintf "Type: %A; Name: %s; Value: %s" reader.NodeType reader.Name reader.Value


