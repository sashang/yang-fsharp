// Logging.fsx
// Sets up logging for working with F# interactive

#load @"../../../.paket/load/net471/NLog.fsx"

[<AutoOpen>]
module MyLog =
    open NLog

    type MyLog internal () =
        let config = new Config.LoggingConfiguration()
        let target = new Targets.ConsoleTarget("fsi")

        do
            config.AddTarget(target)
            LogManager.Configuration <- config

        member this.AddTrace (name : string) =
            let name' = sprintf "*_%s" name
            let rule = new Config.LoggingRule(name', NLog.LogLevel.Trace, target)
            config.LoggingRules.Add(rule)
            LogManager.Configuration <- config

    let myLog = MyLog ()
