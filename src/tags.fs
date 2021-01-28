namespace NITRO

open ETF
open System
open System.Web

[<AutoOpen>]
module Tags =

    type Name = string

    type Attr =
        | Int   of Name * int
        | Str   of Name * string
        | List  of Name * string list
        | NoVal of Name

        member this.NameValue =
            match this with
            | Int  (name,  v) -> (name, string v)
            | Str  (name,  v) -> (name, v)
            | List (name, xs) -> (name, String.concat " " xs)
            | NoVal name      -> (name, "")

        member this.Render =
            match this with
            | NoVal name -> name
            | _          ->
                let (k, v) = this.NameValue
                sprintf "%s=\"%s\"" k v

    let renderAttrs : Attr list -> string =
        List.map (fun attr -> attr.Render) >> String.concat " "

    type Event =
        { Source : string list; Kind : string; Postback : Term }

        member ev.Render (target : string) =
            let join = String.concat ","
            let escape = sprintf "'%s'"
            let renderSource = fun s -> $"tuple(atom('{s}'),string(querySourceRaw('{s}')))"

            let sources = join (List.map escape ev.Source)
            let binary = String.concat "," (encodeTerm ev.Postback |> Array.map string)
            let sourcesBERT = join (List.map renderSource ev.Source)

            sprintf
                "{ var x=qi('%s'); x && x.addEventListener('%s',
                 function(event){ if (validateSources([%s]))
                 { ws.send(enc(tuple(atom('pickle'),bin('%s'),
                 bin(new Uint8Array([%s])),[%s]))); }
                 else console.log('Validation error'); })}"
                target ev.Kind sources target binary sourcesBERT

    let htmlEscape = HttpUtility.HtmlEncode

    type Elem =
        | Tag      of Name * Attr list * Elem list
        | Button   of Name * Attr list * String * Event
        | Unpaired of Name * Attr list
        | Liter    of String

        member this.Render =
            let render = fun (elem : Elem) -> elem.Render
            let join = String.concat ""

            match this with
            | Tag (tag, attrs, body) ->
                let (html, js) = List.unzip (List.map render body)
                (sprintf "<%s %s>%s</%s>" tag (renderAttrs attrs) (join html) tag, join js)
            | Button (name, attrs, value, ev) ->
                let attrs' = Attr.Str ("id", name) :: attrs
                (sprintf "<button %s>%s</button>" (renderAttrs attrs') value,
                 ev.Render name)
            | Unpaired (name, attrs) ->
                (sprintf "<%s %s />" name (renderAttrs attrs), "")
            | Liter s -> (htmlEscape s, "")