// Statistics.fsx
// Some statistics on the YANG modules

#load "Initialize.fsx"

open System
open System.Collections.Generic
open Yang.Parser

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
    // It would be good to avoid parsing identical files many times,
    fold_on_all_models ignore_known_incorrect_models (fun _ -> System.Collections.Generic.Dictionary<string, int>()) (
        fun (state : Dictionary<string, int>) filename ->
            let model = get_external_model filename
            let root = apply_parser Generic.parse_many_statements model |> List.head

            Yang.Model.Generic.KeywordUsage root
            |> List.iter (
                fun (keyword, popularity) ->
                    if state.ContainsKey(keyword) then
                        state.[keyword] <- state.[keyword] + popularity
                    else
                        state.Add(keyword, popularity)
            )

            state
    )


statistics.Keys
|> Seq.sortWith (
    fun keyword1 keyword2 ->
        if keyword1.IndexOf(':') >= 0   && keyword2.IndexOf(':') >= 0   then
            keyword1.CompareTo(keyword2)
        elif keyword1.IndexOf(':') >= 0 && (keyword2.IndexOf(':') < 0)  then
            1
        elif keyword1.IndexOf(':') < 0  && keyword2.IndexOf(':') > 0    then
            -1
        else
            keyword1.CompareTo(keyword2)
)
|> Seq.iter (
    fun keyword ->
        let popularity = statistics.[keyword]
        printfn "%-40s\t%10d" keyword popularity
)

// Based on 12329 input files (1.68 GBytes total size), on a reasonable spec'ed, but old in 2018, Intel Xeon E5-1620 @ 3.60GHz,
// the (single-threaded, 32-bit, VS 15.6.0 Preview 7.0, last-commit: 92cc00c) computation above took roughly 4.5min.
// FSI reported the following statistics:
// Real: 00:04:15.654, CPU: 00:04:30.578, GC gen0: 6921, gen1: 6119, gen2: 130
// The results are as follows:
(*
action                                  	        26
anydata                                 	        25
anyxml                                  	     49506
argument                                	      1123
augment                                 	      6916
base                                    	     13479
belongs-to                              	      2753
bit                                     	      3964
case                                    	    207658
choice                                  	     90754
config                                  	     84741
contact                                 	     10545
container                               	   1814333
default                                 	     71102
description                             	   8928685
deviate                                 	      8328
deviation                               	      8328
enum                                    	    609956
error-message                           	       397
extension                               	      2020
feature                                 	      1994
fraction-digits                         	       726
grouping                                	     93388
identity                                	     13185
if-feature                              	      3030
import                                  	     23450
include                                 	      3574
input                                   	     40555
key                                     	    104448
leaf                                    	   8171781
leaf-list                               	     55771
length                                  	     56081
list                                    	    139986
mandatory                               	     39380
max-elements                            	      9195
min-elements                            	       357
module                                  	      9602
must                                    	      3721
namespace                               	      9602
notification                            	      2296
ordered-by                              	     47848
organization                            	     12133
output                                  	     50860
path                                    	     19169
pattern                                 	    138793
position                                	      4710
prefix                                  	     35805
presence                                	     50884
range                                   	    200851
reference                               	     20638
refine                                  	       242
require-instance                        	       130
revision                                	     18929
revision-date                           	      2777
rpc                                     	     66856
status                                  	     22315
submodule                               	      2753
type                                    	   8640709
typedef                                 	    136929
unique                                  	        31
units                                   	    101642
uses                                    	    337705
value                                   	    145090
when                                    	     12853
yang-version                            	      1158
yin-element                             	       210
config:inner-state-bean                 	         1
config:java-class                       	        15
config:java-name-prefix                 	        17
config:provided-service                 	        16
config:required-identity                	        14
csc:cli-command                         	      1566
csc:xr-task                             	      1566
ext:allowDelete                         	      2511
ext:augment-identifier                  	        22
ext:bit                                 	      1091
ext:context-instance                    	         5
ext:context-reference                   	        10
ext:item                                	        31
ext:masklen                             	        38
ext:meaning                             	        31
ext:support-filter                      	       556
ext:value-replace                       	         8
govern:proposed                         	        14
junos:must                              	     91472
junos:must-message                      	     91472
junos:pattern-message                   	     29408
junos:posix-pattern                     	     29458
md:annotation                           	         6
nacm:default-deny-all                   	        35
nacm:default-deny-write                 	        10
nc:get-filter-element-attributes        	        14
ncs:servicepoint                        	         2
notif-bis:control-plane-notif           	        21
oc-ext:openconfig-version               	       344
rc:yang-data                            	        18
rpcx:rpc-context-instance               	         8
smi:display-hint                        	         4
smiv2:alias                             	     69152
smiv2:defval                            	      8374
smiv2:display-hint                      	       716
smiv2:implied                           	       110
smiv2:max-access                        	     54675
smiv2:oid                               	    137995
sn:subscription-state-notification      	         7
tailf:action                            	         5
tailf:actionpoint                       	        45
tailf:alt-name                          	       481
tailf:annotate                          	        36
tailf:args                              	         1
tailf:arg-type                          	       881
tailf:callpoint                         	        24
tailf:cli-add-mode                      	       269
tailf:cli-allow-join-with-key           	       110
tailf:cli-allow-join-with-value         	       140
tailf:cli-allow-key-abbreviation        	         3
tailf:cli-allow-range                   	         1
tailf:cli-before-key                    	        42
tailf:cli-boolean-no                    	       347
tailf:cli-break-sequence-commands       	       217
tailf:cli-case-insensitive              	         3
tailf:cli-compact-syntax                	      2496
tailf:cli-delete-container-on-delete    	        15
tailf:cli-delete-when-empty             	       738
tailf:cli-diff-dependency               	       334
tailf:cli-disallow-value                	       122
tailf:cli-display-joined                	       250
tailf:cli-display-separated             	        65
tailf:cli-drop-node-name                	      5367
tailf:cli-exit-command                  	        99
tailf:cli-explicit-exit                 	        34
tailf:cli-expose-key-name               	       110
tailf:cli-flat-list-syntax              	       179
tailf:cli-flatten-container             	       794
tailf:cli-full-command                  	      3769
tailf:cli-full-no                       	        27
tailf:cli-hide-in-submode               	       463
tailf:cli-incomplete-command            	       787
tailf:cli-incomplete-no                 	        65
tailf:cli-list-syntax                   	       102
tailf:cli-mode-name                     	       805
tailf:cli-multi-value                   	       367
tailf:cli-multi-word-key                	        12
tailf:cli-no-key-completion             	        24
tailf:cli-no-keyword                    	       304
tailf:cli-no-match-completion           	         6
tailf:cli-no-name-on-delete             	        12
tailf:cli-no-value-on-delete            	        70
tailf:cli-optional-in-sequence          	      1029
tailf:cli-prefix-key                    	       219
tailf:cli-range-list-syntax             	        69
tailf:cli-remove-before-change          	         9
tailf:cli-replace-all                   	         6
tailf:cli-reset-all-siblings            	        73
tailf:cli-reset-container               	       164
tailf:cli-reset-siblings                	       250
tailf:cli-sequence-commands             	      1650
tailf:cli-show-long-obu-diffs           	        23
tailf:cli-show-no                       	        52
tailf:cli-show-with-default             	        15
tailf:cli-suppress-key-abbreviation     	        45
tailf:cli-suppress-list-no              	        39
tailf:cli-suppress-mode                 	      1441
tailf:cli-suppress-no                   	        12
tailf:cli-suppress-range                	        60
tailf:cli-suppress-show-path            	         8
tailf:cli-trim-default                  	       167
tailf:code-name                         	       274
tailf:dependency                        	       415
tailf:display-when                      	       102
tailf:error-info                        	         4
tailf:exec                              	         4
tailf:export                            	        14
tailf:hidden                            	        20
tailf:id                                	        36
tailf:id-value                          	         7
tailf:info                              	      2527
tailf:internal                          	        27
tailf:invocation-mode                   	         1
tailf:java-class-name                   	         2
tailf:key-default                       	        33
tailf:link                              	         1
tailf:no-leafref-check                  	         4
tailf:non-strict-leafref                	       198
tailf:occurence                         	       550
tailf:raw-xml                           	         1
tailf:set-hook                          	         3
tailf:snmp-mib-module-name              	        31
tailf:snmp-name                         	         4
tailf:snmp-oid                          	       252
tailf:sort-order                        	        27
tailf:step                              	         3
tailf:structure                         	        28
tailf:substatement                      	      1921
tailf:suppress-echo                     	         3
tailf:transaction-hook                  	         1
tailf:use-in                            	      5523
tailf:validate                          	         4
tailf:value-length                      	        45
tailf:wd                                	         4
xr:event-telemetry                      	       100
xr:xr-cli-map                           	      4070
xr:xr-xml-map                           	    132657
yangmnt:mount-point                     	         8
ymt:text-media-type                     	         1
 *)
// The popularity statistics are not particularly relevant, as many models are evaluated many times, since they appear in
// different versions of the same software. In addition, there is plenty of repetition even for models of the same software and version.


// Some tracing of position of particular elements in the Juniper configuration
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "interfaces"))
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "traceoptions"))
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "vlan_tag_mode"))
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "vlan-id"))
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "family"))
Yang.Model.Generic.FindAllNodes (juniper, Yang.Model.Generic.Filter.Make("container", Some "inet6"))


//
// Parsing of all modules
//

open FParsec

let ignore_juniper (filename : string) = if filename.Contains("Juniper") then false else true
let fand<'T> (fn1 : 'T -> bool) (fn2 : 'T -> bool) = fun (v : 'T) -> (fn1 v) && (fn2 v)

// let filter = ignore_known_incorrect_models
let filter = fand ignore_known_incorrect_models ignore_juniper

let mutable total = 0
let mutable correct = 0

#time
let _ =
    // It would be good to avoid parsing identical files many times,
    try_for_all_models filter (
        fun filename ->
            total <- total + 1
            let model = get_external_model filename
            let root = apply_parser (wse >>. Module.parse_module_or_submodule) model
            correct <- correct + 1
            root
    )
    |> Seq.choose id
    |> Seq.toList


printfn "Total: %05d, Correct: %05d, Success: %4.3f" total correct ((float correct) / (float total))

// 2018-03-23
// Real: 00:33:00.883, CPU: 00:32:16.703, GC gen0: 56193, gen1: 14103, gen2: 71
// Total: 12367, Correct: 04024, Success: 0.325
//
// 2018-03-25
// Real: 00:30:28.246, CPU: 00:29:48.656, GC gen0: 52212, gen1: 14207, gen2: 45
// Total: 12367, Correct: 11562, Success: 0.935
// Real: 00:26:17.771, CPU: 00:25:28.890, GC gen0: 51737, gen1: 14022, gen2: 41
// Total: 12367, Correct: 11580, Success: 0.936
//
//
// Real: 11:01:26.054, CPU: 11:00:20.781, GC gen0: 54898, gen1: 39779, gen2: 345
// Total: 07763, Correct: 07732, Success: 0.996
