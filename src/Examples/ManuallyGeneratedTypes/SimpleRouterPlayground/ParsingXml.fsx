#r "System.Xml.Linq.dll"
#r @"..\..\..\..\build\Yang.Generator.dll"
#r @"..\SimpleRouter\bin\Debug\SimpleRouter.dll"

open System.IO
open System.Xml
open System.Xml.Linq
open System.Xml.XPath

open SimpleRouter
open Yang.Generator.XmlHelper


let address = "<address><name>1.2.0.1/31</name></address>"
let reader = XmlReaderHelper.Parse address

let name = Configuration.InterfaceClass.UnitClass.InetClass.AddressClass(reader)
name.Name


let empty = """
<inet>
    <address>
    </address>
</inet>
"""
let z = Configuration.InterfaceClass.UnitClass.InetClass(XmlReaderHelper.Parse empty)

let one = """
<inet>
    <address>
        <name>1.2.0.1/31</name>
    </address>
</inet>
"""

let a = Configuration.InterfaceClass.UnitClass.InetClass(XmlReaderHelper.Parse one)
a.ToString()


let two = """
<inet>
    <address>
        <name>1.2.0.1/31</name>
    </address>
    <address>
        <name>1.2.0.5/31</name>
    </address>
</inet>
"""
let r = XmlReaderHelper.Parse two
r.CurrentState
r.Forward ()
Configuration.InterfaceClass.UnitClass.InetClass.AddressClass(r)
let b = Configuration.InterfaceClass.UnitClass.InetClass(XmlReaderHelper.Parse two)
b.ToString()



let _unit = """
<unit>
	<name>0</name>
	<description>A very important interface</description>
	<family>
		<inet>
			<address>
				<name>1.2.0.1/31</name>
			</address>
		</inet>
	</family>
</unit>
"""
let u = XmlReaderHelper.Parse _unit
let uu = Configuration.InterfaceClass.UnitClass(u)






let configuration = """
<configuration>
	<version>14.XX or 15.XX or 16.XX or 17.XX or whatever</version>
	<system>
		<interfaces>
			<interface>
				<name>xe-0/0/0</name>
				<unit>
					<name>0</name>
					<family>
						<inet>
							<address>
								<name>1.1.1.15/24</name>
							</address>
						</inet>
					</family>
				</unit>
			</interface>
        </interfaces>
    </system>
</configuration>"""
let c = XmlReaderHelper.Parse configuration
let cc = Configuration(c)
cc.Version
cc.Interface.["xe-0/0/0"].Unit.["0"]

let configuration2 = """
<configuration junos:commit-seconds="1518227656" junos:commit-localtime="2018-02-10 01:54:16 UTC" junos:commit-user="autotest">
	<version>14.XX or 15.XX or 16.XX or 17.XX or whatever</version>
	<system>
		<interfaces>
			<interface>
				<name>xe-0/0/0</name>
				<unit>
					<name>0</name>
					<family>
						<inet>
							<address>
								<name>1.1.1.15/24</name>
							</address>
						</inet>
					</family>
				</unit>
			</interface>
        </interfaces>
    </system>
</configuration>"""




[<StructuredFormatDisplay("{Display}")>]
type Name (name) =
    new (reader : XmlReaderHelper) =
        let str = reader.ReadString("name").Value
        Name(str)

    member __.Name = name
    member __.Display = sprintf "{ name='%s' }" name
    override this.ToString() = this.Display

[<StructuredFormatDisplay("{Display}")>]
type Address (name : Name []) =
    new (reader : XmlReaderHelper) =
        assert (reader.IsElementBegin "address")
        reader.Forward ()

        let mutable results = []

        while reader.NodeType = XmlNodeType.Element && reader.Name = "name" do
            let name = Name(reader)
            results <- name :: results

        assert (reader.IsElementEnd "address")
        reader.Forward()

        Address (results |> List.rev |> Array.ofList)

    member __.Name = name
    member __.Display = sprintf "{ address=%s }" (name |> Array.map (fun n -> n.Display) |> String.concat ", ")
    override this.ToString() = this.Display

Address (XmlReaderHelper.Parse empty)
Address (XmlReaderHelper.Parse one)
let xx = Address (XmlReaderHelper.Parse two)
xx.Name.[0].Name
