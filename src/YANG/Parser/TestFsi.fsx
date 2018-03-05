(*
 * This script is just for testing the F# Interactive.
 *)

#r @"../Model/bin/Debug/Yang.Model.dll"

open Yang.Model

let body = LeafBodyStatement.Description ("Some test here", None)
printfn "Description is: %A" body
