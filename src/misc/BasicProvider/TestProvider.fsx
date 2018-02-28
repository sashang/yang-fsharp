// Compile with: fsc -r:.\bin\Debug\BasicProvider.dll .\TestProvider.fsx

#r @"bin/Debug/BasicProvider.dll"
open StaticProperty.Provided

printfn "%s" MyType.MyProperty