namespace Yang.Generator

[<AutoOpen>]
module Types =
    open System
    open System.Reflection
    open ProviderImplementation.ProvidedTypes
    open Yang.Parser

    (*
     * Boiler plate code for creating types
     *)

    let private _logger : (string -> unit) option ref = ref None

    let SetLogger (logger : (string -> unit) option) = _logger := logger

    type private Logger =
        static member Log (message : string) =
            match !_logger with
            | None -> ()
            | Some logger -> logger message


    let internal makeType ns asm typeName =
        let ty = ProvidedTypeDefinition( asm, ns, typeName, Some typeof<obj>, isErased = false)
        ty.SetAttributes (TypeAttributes.Class ||| TypeAttributes.Public)
        ty

    let internal makeIncludedType typeName =
        let ty = ProvidedTypeDefinition(typeName, Some typeof<obj>, isErased=false)
        ty.SetAttributes (TypeAttributes.Class ||| TypeAttributes.Public)
        ty

    let internal makeTypeInAssembly ns asm typeName =
        let ty = ProvidedTypeDefinition( asm, ns, typeName, Some typeof<obj>, isErased = false)
        ty.SetAttributes (TypeAttributes.Class ||| TypeAttributes.Public)
        ty


    let internal addIncludedType (provAsm : ProvidedAssembly) (ty:ProvidedTypeDefinition) = provAsm.AddTypes([ ty ]) ; ty
    let internal addMembers (mi:#MemberInfo list) (ty:ProvidedTypeDefinition)   = ty.AddMembers mi  ; ty
    let internal addMember (mi:#MemberInfo) (ty:ProvidedTypeDefinition)         = ty.AddMember mi   ; ty

    let internal createDefaultConstructor () =
        let constructor = ProvidedConstructor(
                            [],
                            invokeCode = fun args ->
                                // All fields will get the default values
                                <@@ () @@>
                    )
        constructor.AddXmlDoc(sprintf "Default constructor")
        constructor

    (*
     * end of boilerplate code
     *)

    let internal appendModuleInformation (``module`` : Module.Module) =
        SetLogger (Some (fun str -> printfn "%s" str))

        let (version, _) = ``module``.Header.YangVersion
        let (ns, _) = ``module``.Header.Namespace
        let (prefix, _) = ``module``.Header.Prefix

        let header = [
            ProvidedField.Literal ("YangVersion", typeof<Version>, version);
            ProvidedField.Literal ("Namespace", typeof<Uri>, ns);
            ProvidedField.Literal ("Prefix", typeof<string>, prefix)
        ]

        let meta =
            match ``module``.Meta with
            | None -> []
            | Some meta ->
                [
                    meta.Contact        |> Option.map (fun (c, _) -> ProvidedField.Literal ("Contact", typeof<string>, c))
                    meta.Description    |> Option.map (fun (d, _) -> ProvidedField.Literal ("Description", typeof<string>, d))
                    meta.Organization   |> Option.map (fun (o, _) -> ProvidedField.Literal ("Organization", typeof<string>, o))
                    meta.Reference      |> Option.map (fun (r, _) -> ProvidedField.Literal ("Reference", typeof<string>, r))
                ] |> List.choose id

        (header @ meta)

    let internal createModule (``module`` : Module.Module) =
        let statics = appendModuleInformation ``module``  |> List.map (fun v -> v :> MemberInfo)
        let fields  = [ ProvidedField("Test", typeof<string>) ] |> List.map (fun v -> v :> MemberInfo)
        // let constructors = [ createDefaultConstructor() ] |> List.map (fun v -> v :> MemberInfo)
        statics @ fields // @ constructors
