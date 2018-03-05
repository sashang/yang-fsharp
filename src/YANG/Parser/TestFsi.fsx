//
// This script is just for testing the F# Interactive.
//
// Some ways to run this script that should work (in producing the output)
//
// - with fsi:
//      fsi .\TestFsi.fsx
//
// - with fsc:
//      fsc TestFsi.fsx
//      .\TestFsi.exe
// For this you may need to copy the Yang.Model.dll to the same directory, and, maybe, setup the proper F# compiler:
//      $Env:PATH="C:\Program Files (x86)\Microsoft SDKs\F#\10.1\Framework\v4.0\;" + $Env:PATH
//      $Env:FSHARPINSTALLDIR="C:\Program Files (x86)\Microsoft SDKs\F#\10.1\Framework\v4.0\"
//
// - from Visual Studio: you may also give as a parameter to interactive: --noframework
// - from Visual Studio Code
//
//

// If the following fails, Tools->Options->F# Tools->F# Interactive: In F# interactive options, add '--noframework'
#r @"..\..\..\packages\FSharp.Core\lib\net45\FSharp.Core.dll"
#r @"../Model/bin/Debug/Yang.Model.dll"

open Yang.Model

let body = LeafBodyStatement.Description ("Some test here", None)
printfn "Description is: %A" body
