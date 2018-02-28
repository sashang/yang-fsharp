namespace Yang.Generator

// Some pointers to creating TypeProviders:
// - <https://github.com/fsprojects/FSharp.TypeProviders.SDK>
// - <https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/creating-a-type-provider>
//   This seems to be a bit outdated.

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Yang.Parser

open Microsoft.FSharp.Quotations

type GenericYangProviderError (message:string, ?innerException:exn) =
    inherit ApplicationException(
        message,
        match innerException with | Some ex -> ex | _ -> null)

//[<TypeProvider>]
//type public YangProvider (config: TypeProviderConfig) as this =
//    inherit TypeProviderForNamespaces()

//    /// Static parameters for type provider.
//    let staticParams =
//        [
//            // Specifies the file that contains the YANG model
//            ProvidedStaticParameter("file", typeof<string>)
//        ]

//    let asm = Assembly.LoadFrom(config.RuntimeAssembly)
//    let schema = makeType asm "YangProvider"

//    /// Each provider needs a unique temporary file
//    let provAsm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))

//    // Set the logger; observe that the logger will just write to console
//    do SetLogger !ProvidedTypeDefinition.Logger

//    /// Method for starting the type generation process
//    let schemaCreation =
//        fun (typeName:string) (parameterValues: obj[]) ->
//            match parameterValues with
//            | [| :? string as fileName |] ->
//                if File.Exists(fileName) = false then
//                    raise (GenericYangProviderError (sprintf "Cannot find model file '%s'" fileName))

//                let model = Parser.ReadAndClean fileName
//                if System.String.IsNullOrWhiteSpace(model) then
//                    raise (GenericYangProviderError "Input model cannot be empty")

//                match (run  Module.parse_module model) with
//                | Success (result, _, _)    ->
//                    typeName
//                    |> makeTypeInAssembly asm
//                    |> addMember (makeIncludedType "ModuleInformation" |> addMembers (createTypes result))
//                    |> addIncludedType provAsm

//                | Failure (message, _, _)   -> raise (GenericYangProviderError (sprintf "Failed to parse model, error: %s" message))

//            | _ -> raise (GenericYangProviderError "Unexpected parameter values when using Yang provider")

//    do this.AddNamespace(ns, [ addIncludedType provAsm schema ])
//    do schema.DefineStaticParameters( parameters=staticParams, instantiationFunction=schemaCreation )


/// This provider creates the type from a string;
/// it is useful for testing, otherwise, it is very similar to the functionality above.
[<TypeProvider>]
type public YangFromStringProvider (config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces(config)

    /// Static parameters for type provider.
    let staticParameters =
        [
            // This parameter is used to specify the model from which to generate types.
            ProvidedStaticParameter("model", typeof<string>)
        ]

    let asm = ProvidedAssembly()

    /// This is the namespace for the type provider
    let ns = "Yang.YangProvider"
    let schema = makeType ns asm "YangFromStringProvider"

    /// Each provider needs a unique temporary file
    let provAsm = ProvidedAssembly()

    // Set the logger; observe that the logger will just write to console
    do SetLogger !ProvidedTypeDefinition.Logger

    /// Method for starting the type generation process
    let schemaCreation =
        fun (typeName:string) (parameterValues: obj[]) ->
            match parameterValues with
            | [| :? string as model |] ->
                try
                    let model' = MakeFromString model

                    typeName
                    |> makeType ns asm
                    |> addMember (makeIncludedType "Information" |> addMembers (createModule model'))
                    |> addIncludedType provAsm
                with
                | :? YangParserException as  ex ->
                    raise (GenericYangProviderError ("Error in parsing model", ex))

            | _ -> raise (GenericYangProviderError "Unexpected parameter values when using Yang provider")

    do
        schema.DefineStaticParameters( parameters=staticParameters, instantiationFunction=schemaCreation )
        this.AddNamespace(ns, [ addIncludedType provAsm schema ])

[<TypeProviderAssembly>]
do ()
