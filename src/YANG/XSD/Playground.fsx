#r "System.Xml.Linq.dll"

#r @"..\XmlHelper\bin\Debug\Yang.XmlHelper.dll"

open System.IO
open System.Xml
open System.Xml.Linq

open Yang.XmlHelper.Reader

// Define your library scripting code here
let input = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", "Models-External", "Juniper", "XSD", "juniper-netconf.xsd")
let parser = XmlReaderHelper.Parse (File.ReadAllText input)

parser.Reader.AttributeCount
parser.Reader.GetAttribute(1)
parser.Reader.MoveToAttribute(0)
parser.Reader.MoveToNextAttribute()
parser.Reader.GetAttribute(0)

parser.Reader.MoveToFirstAttribute()
parser.Reader.MoveToNextAttribute()
parser.Attributes |> Seq.toList
parser.Forward()

parser.CurrentState
parser.Name
parser.NodeType
parser.Value

parser.Forward()
