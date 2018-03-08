// Statistics.fsx
// Some statistics on the YANG modules

#load "Initialize.fsx"

open System
open System.IO
open System.Text
open FParsec
open Yang.Parser
open System.Data
open Yang.Model.Statements

open Initialize
open Logging

let big_model = get_external_model @"Juniper\16.1\configuration.yang"
let juniper = apply_parser Generic.parse_many_statements big_model |> List.head

Yang.Model.Generic.KeywordUsage juniper
|> List.iter (
    fun (keyword, popularity) ->
        printfn "%-20s\t%10d" keyword popularity
)

(* The output of the command above is:
case                	      3083
choice              	      1738
config              	         1
container           	      6672
default             	      1157
description         	     43610
enum                	     13557
fraction-digits     	        11
grouping            	       643
import              	         1
key                 	      2019
leaf                	     20905
leaf-list           	      1048
length              	      1062
list                	      2019
mandatory           	      1076
max-elements        	       269
module              	         1
namespace           	         1
ordered-by          	      1748
organization        	         1
prefix              	         2
presence            	      1719
range               	      6429
status              	       946
type                	     22011
typedef             	        58
units               	      3838
uses                	      7737
junos:must          	      4905
junos:must-message  	      4905
junos:pattern-message	      1042
junos:posix-pattern 	      1042
 *)

//
// Keyword usage across all external models
//
#time
let statistics =
    get_all_external_models.Value
    |> Seq.toList
    |> List.collect (
        fun filename ->
            printfn "%A\t\tParsing: %s" (DateTime.Now) (filename.Substring(external_modules_dir.Length))
            let model = apply_parser Generic.parse_many_statements big_model |> List.head
            Yang.Model.Generic.KeywordUsage model
    )
    |> List.groupBy (fun (keyword, _) -> keyword)
    |> List.map (
        fun (keyword, statistics) ->
            let total = statistics |> List.sumBy (fun (_, v) -> v)
            keyword, total
    )

statistics
|> List.iter (
    fun (keyword, popularity) ->
        printfn "%-20s\t%10d" keyword popularity
)


// Some tracing of position of particular elements in the Juniper configuration
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "interfaces"))
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "traceoptions"))
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "vlan_tag_mode"))
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "vlan-id"))
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "family"))
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "inet6"))
