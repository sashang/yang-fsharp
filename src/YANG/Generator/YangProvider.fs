namespace Yang.Generator

// Some pointers to creating TypeProviders:
// - <https://docs.microsoft.com/en-us/dotnet/fsharp/tutorials/type-providers/creating-a-type-provider>

open System
open System.IO
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
//open Yang.Parser

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
//[<TypeProvider>]
//type public YangFromStringProvider (config: TypeProviderConfig) as this =
//    inherit TypeProviderForNamespaces()

//    /// Static parameters for type provider.
//    let staticParameters =
//        [
//            // This parameter is used to specify the model from which to generate types.
//            ProvidedStaticParameter("model", typeof<string>)
//        ]

//    let asm = Assembly.LoadFrom(config.RuntimeAssembly)
//    let schema = makeType asm "YangFromStringProvider"

//    /// Each provider needs a unique temporary file
//    let provAsm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))

//    // Set the logger; observe that the logger will just write to console
//    do SetLogger !ProvidedTypeDefinition.Logger

//    /// Method for starting the type generation process
//    let schemaCreation =
//        fun (typeName:string) (parameterValues: obj[]) ->
//            match parameterValues with
//            | [| :? string as model |] ->
//                try
//                    let model' = MakeFromString model
//                    typeName
//                    |> makeType asm
//                    |> addMember (ProvidedLiteralField(
//                                    "Test",
//                                    typeof<string>,
//                                    "Example"
//                                 ))
//                    // |> addMember (makeIncludedType "ModuleInformation" |> addMembers (createTypes result))
//                    |> addIncludedType provAsm
//                with
//                | :? YangParserException as  ex ->
//                    raise (GenericYangProviderError ("Error in parsing model", ex))

//            | _ -> raise (GenericYangProviderError "Unexpected parameter values when using Yang provider")

//    do this.AddNamespace(ns, [ addIncludedType provAsm schema ])
//    do schema.DefineStaticParameters( parameters=staticParameters, instantiationFunction=schemaCreation )

[<TypeProvider>]
type SampleTypeProvider(config: TypeProviderConfig) as this = 

  // Inheriting from this type provides implementations of ITypeProvider 
  // in terms of the provided types below.
  inherit TypeProviderForNamespaces()

  let namespaceName = "Samples.HelloWorldTypeProvider"
  let thisAssembly = Assembly.GetExecutingAssembly()

  // Make one provided type, called TypeN.
  let makeOneProvidedType (n:int) =
    let t = ProvidedTypeDefinition(thisAssembly,namespaceName, "Type" + string n, baseType = Some typeof<obj>)
    t.AddXmlDocDelayed (fun () -> sprintf "This provided type %s" ("Type" + string n))

    let staticProp = ProvidedProperty(propertyName = "StaticProperty",
                                      propertyType = typeof<string>, 
                                      IsStatic=true,
                                      GetterCode= (fun args -> <@@ "Hello!" @@>))
    staticProp.AddXmlDocDelayed(fun () -> "This is a static property")
    t.AddMember staticProp

    let ctor = ProvidedConstructor(parameters = [ ], InvokeCode= (fun args -> <@@ "The object data" :> obj @@>))
    ctor.AddXmlDocDelayed(fun () -> "This is a constructor")
    t.AddMember ctor

    let ctor2 = ProvidedConstructor(parameters = [ ProvidedParameter("data",typeof<string>) ],
                                    InvokeCode= (fun args -> <@@ (%%(args.[0]) : string) :> obj @@>))
    t.AddMember ctor2

    let instanceProp =
        ProvidedProperty(propertyName = "InstanceProperty", 
                         propertyType = typeof<int>, 
                         GetterCode= (fun args -> 
                            <@@ ((%%(args.[0]) : obj) :?> string).Length @@>))
    instanceProp.AddXmlDocDelayed(fun () -> "This is an instance property")
    t.AddMember instanceProp

    let instanceMeth = 
        ProvidedMethod(methodName = "InstanceMethod", 
                       parameters = [ProvidedParameter("x",typeof<int>)], 
                       returnType = typeof<char>, 
                       InvokeCode = (fun args -> 
                       <@@ ((%%(args.[0]) : obj) :?> string).Chars(%%(args.[1]) : int) @@>))

    instanceMeth.AddXmlDocDelayed(fun () -> "This is an instance method")
    // Add the instance method to the type.
    t.AddMember instanceMeth
    t

  let types = [ for i in 1 .. 100 -> makeOneProvidedType i ]
  do this.AddNamespace(namespaceName, types)


[<TypeProviderAssembly>]
do ()
