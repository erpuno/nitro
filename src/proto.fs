namespace NITRO

open ETF
open N2O

[<AutoOpen>]
module Proto =

    type Nitro =
        | Message of Term * Map<string, string>
        | Error   of string
        | Init    of string
        | Ping
        | Ok

    let readQuery : Term -> (string * string) option = function
        | Term.Tuple [Term.Atom name; Term.String value] -> Some (name, value)
        | _ -> None

    let (|Prefix|_|) (prefix : string) (s : string) =
        if s.StartsWith prefix then Some (s.Substring prefix.Length)
        else None

    let nitroProc (req : Req) : Msg -> Nitro = function
        | Bin bytes ->
            match decodeTerm bytes with
            | Term.Tuple [Term.Atom "pickle"; Term.Binary target;
                          Term.Binary term; Term.List (linked, Term.Nil)] ->
                Nitro.Message (decodeTerm term, Map (Seq.choose readQuery linked))
            | Term.Error err -> Nitro.Error err
            | _ -> Nitro.Error "unknown term"
        | Text (Prefix "N2O," token) -> Init token
        | Text "PING" -> Ping
        | Nope -> Ok
        | _ -> Nitro.Error "unknown message"

    let nitroProto (router : Req -> Nitro -> Msg) =
        fun (req : Req) -> nitroProc req >> router req

    let empty = fun (msg : Nitro) -> Nope
