open ETF
open N2O
open NITRO

module Program =

    let index : Nitro -> Msg = function
        | Init token ->
            Elem.Button ("send", [], "Send", { Source = ["msg"]; Kind = "click";
                                               Postback = Atom "send" })
            |> update "send"
        | Message (Atom "send", query) ->
            match query.TryFind "msg" with
            | Some value -> insertBottom "hist" (div [] [Elem.Liter value])
            | None -> Nope
        | _ -> Nope

    let about : Nitro -> Msg = function
        | Init token -> updateText "app" "This is the N2O Hello World App"
        | _ -> Nope

    let router (req : Req) : Nitro -> Msg =
        match req.path with
        | "/ws/static/index.html" -> index
        | "/ws/static/about.html" -> about
        | _ -> empty

    [<EntryPoint>]
    let main _ =
        let mutable port = 1900
        let mutable (cpu, io) = (4, 4)
        let mutable ret = 0

        try
            System.Threading.ThreadPool.GetMinThreads(&cpu, &io)
            printfn "N2O/F# WebSocket Server 1.0"
            printfn "[smp] [cpu:%i] [io:%i]" cpu io
            System.Threading.ThreadPool.SetMaxThreads(cpu, io) |> ignore

            Server.ticker <- false
            Server.proto <- nitroProto router
            use ws = Server.start "0.0.0.0" port
            System.Threading.Thread.Sleep -1
        with exn ->
            printfn "EXIT: %s" exn.Message
            ret <- 1
        ret