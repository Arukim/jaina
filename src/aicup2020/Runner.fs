namespace Aicup2020

open System
open System.IO
open System.Net.Sockets
open Jaina.Core

module Runner =
    type T(host, port, token: string) =
        let client = new TcpClient(host, port)
        let stream = new BufferedStream(client.GetStream())
        let reader = new BinaryReader(stream)
        let writer = new BinaryWriter(stream)
        let tokenData = System.Text.Encoding.UTF8.GetBytes token

        do
            client.NoDelay <- true
            writer.Write tokenData.Length
            writer.Write tokenData
            writer.Flush()

        member this.run =
            if System.Environment.GetEnvironmentVariable("JAINA_ENV") <> null then 
                let _ = System.Diagnostics.Process.Start("powershell", @"C:\runner\addBots.ps1")
                ()

            printfn $"I see {System.Environment.ProcessorCount} advisors" 
            let myStrategy = new MyStrategy()
            let debugInterface = new DebugInterface(reader, writer)
            let perfMeter = new PerfMeter("turn", 1)

            let rec loop () =
                match Model.ServerMessage.readFrom reader with
                | Model.ServerMessage.GetAction message ->
                    perfMeter.Start()
                    (Model.ClientMessage.ActionMessage
                        { Action =
                              myStrategy.getAction
                                  (message.PlayerView, (if message.DebugAvailable then Some debugInterface else None)) }).writeTo
                        writer
                    writer.Flush()
                    perfMeter.Stop()
                    loop ()
                | Model.ServerMessage.Finish message -> ()
                | Model.ServerMessage.DebugUpdate message ->
                    myStrategy.debugUpdate (message.PlayerView, debugInterface)
                    (new Model.ClientMessageDebugUpdateDone()).writeTo writer
                    writer.Flush()
                    loop ()

            try
                loop ()
            finally
                printfn $"{perfMeter.Summarize()}"

    [<EntryPoint>]
    let main argv =
        let host =
            match argv with
            | x when x.Length >= 1 -> x.[0]
            | _ -> "127.0.0.1"

        let port =
            match argv with
            | x when x.Length >= 2 -> x.[1] |> Int32.Parse
            | _ -> 31001

        let token =
            match argv with
            | x when x.Length >= 3 -> x.[2]
            | _ -> "0000000000000000"

        (new T(host, port, token)).run

        0 // return an integer exit code
