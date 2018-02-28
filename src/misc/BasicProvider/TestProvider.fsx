// Compile with: fsc -r:.\bin\Debug\BasicProvider.dll .\TestProvider.fsx

#r @"bin/Debug/BasicProvider.dll"
open StaticProperty.Provided

type T = StaticProperty.Provided.MyType
printfn "%s" T.MyProperty