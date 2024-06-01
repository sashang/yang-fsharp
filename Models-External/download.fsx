#!/usr/bin/env -S dotnet fsi

// Clone some yang repositories for use in testing. Based on the powershell script in the same directory.
#r "nuget:Argu"

open Argu
open System
open System.Diagnostics
open System.Threading

type CliArguments =
    | Timeout of int
    | ShowRepos

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Timeout _ -> "Time in ms to wait before cancelling a clone. Applies to all invocations of git clone."
            | ShowRepos -> "Prints the repositories to be cloned"
let repositories = [
    {| Name = "BroadbandForum"; Link = "https://github.com/BroadbandForum/yang.git" |}
    {| Name = "Juniper"; Link = "https://github.com/Juniper/yang.git" |}
    {| Name = "networkop"; Link = "https://github.com/networkop/yang.git"|}
    {| Name = "OpenROADM"; Link = "https://github.com/OpenROADM/OpenROADM_MSA_Public.git"|}
    {| Name = "openconfig"; Link = "https://github.com/openconfig/public.git"|}
    {| Name = "sonic-object-model-schema"; Link = "https://github.com/Azure/sonic-object-model-schema.git"|}
    {| Name = "sysrepo"; Link = "https://github.com/sysrepo/sysrepo.git" |}
    {| Name = "tail-f-systems"; Link = "https://github.com/tail-f-systems/JNC.git" |}
    {| Name = "YangModels"; Link = "https://github.com/YangModels/yang.git" |}
]

let printRepos () =
    for repo in repositories do
        printfn "%s: %s" repo.Name repo.Link

let rec concatExMessage (ex : exn) message =
    if isNull ex.InnerException then
        message
    else
        concatExMessage ex (message + ": " + ex.InnerException.Message)

let download (repo: {|Name:string; Link:string|}) ct = async {
    let p = Process.Start("git", [|"clone"; repo.Link; repo.Name|])
    try
        do! p.WaitForExitAsync(ct) |> Async.AwaitTask
        if p.ExitCode = 0 then
            return Ok repo
        else
            return (Error (repo, sprintf "git clone failed with exit code %d" p.ExitCode))
    with
    | ex ->
        p.Kill()
        return Error (repo, (concatExMessage ex ex.Message))
}

let downloadAll (timeout: int) = async {
    let cts = new CancellationTokenSource()
    cts.CancelAfter(timeout)
    let! downloadResult = repositories |> List.map (fun repo -> download repo cts.Token) |> Async.Parallel
    // report errors
    for result in downloadResult do
        match result with
        | Ok _ -> ()
        | Error (repo, message) -> printfn "%s failed to clone: %s" repo.Name message
}

let parser = ArgumentParser.Create<CliArguments>(programName = "download")
let results = parser.Parse(fsi.CommandLineArgs.[1..])
let defaultTimeout = 5*60*1000 //5 minutes converted to ms
let timeout = results.GetResult(Timeout, defaultValue = defaultTimeout)
if results.Contains(ShowRepos) then
    printRepos ()
else
    downloadAll timeout |> Async.RunSynchronously
