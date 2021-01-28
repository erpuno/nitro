namespace NITRO

[<AutoOpen>]
module Elements =

    let div   = fun attrs body -> Tag ("div", attrs, body)
    let title = fun body -> Tag ("title", [], body)

    let idAttr    = fun value -> Attr.Str ("id", value)
    let classAttr = fun value -> Attr.List ("class", value)

    let br = Elem.Unpaired ("br", [])
    let hr = Elem.Unpaired ("hr", [])
