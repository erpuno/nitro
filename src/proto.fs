namespace NITRO

open ETF
open N2O

[<AutoOpen>]
module Proto =

    type Nitro =
        | Message of Term * Map<string, string>
        | Error   of string
        | Nothing
        | Init
        | Ping

    let readQuery : Term -> (string * string) option = function
        | Term.Tuple [Term.Atom name; Term.String value] -> Some (name, value)
        | _ -> None

    let nitroProc (req : Req) : Msg -> Nitro = function
        | Bin bytes ->
            match decodeTerm bytes with
            | Term.Tuple [Term.Atom "pickle"; Term.Binary target;
                          Term.Binary term; Term.List (linked, Term.Nil)] ->
                Nitro.Message (decodeTerm term, Map (Seq.choose readQuery linked))
            | Term.Error err -> Nitro.Error err
            | _ -> Nitro.Error "unknown term"

        | Text "N2O," -> Init
        | Text "PING" -> Ping
        | Nope -> Nothing
        | _ -> Nitro.Error "unknown message"

    let nitroProto (router : Req -> Nitro -> Msg) =
        fun (req : Req) (msg : Msg) ->
            nitroProc req msg
            |> router req

    let empty = fun (msg : Nitro) -> Nope
