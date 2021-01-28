namespace NITRO

open ETF
open N2O
open System.Text

[<AutoOpen>]
module JavaScript =

    let encodeJS (b : byte array) =
        Term.Tuple
            [Term.Atom "io";
             Term.Binary b;
             Term.Binary [||]]

    let action (x : string) : Msg =
        let bytes = Encoding.UTF8.GetBytes x
        Msg.Bin (encodeTerm (encodeJS bytes))

    let update (target : string) (elem : Elem) =
        let (html, js) = elem.Render
        sprintf "qi('%s').outerHTML='%s';%s" target html js
        |> action

    let updateText target text =
        sprintf "qi('%s').innerText='%s';" target text
        |> action

    let insertTagTop tag target (elem : Elem) =
        let (html, js) = elem.Render

        sprintf
            "qi('%s').insertBefore((function() {
                var div = qn('%s'); div.innerHTML = '%s';
                return div.firstChild;
             })(), qi('%s').firstChild);%s"
            target tag html target js
        |> action

    let insertTagBottom tag target (elem : Elem) =
        let (html, js) = elem.Render

        sprintf
            "(function(){ var div = qn('%s'); div.innerHTML = '%s';
             qi('%s').appendChild(div.firstChild); })();%s"
            tag html target js
        |> action

    let insertTop    = insertTagTop    "div"
    let insertBottom = insertTagBottom "div"

    let insertAdjacent position target (elem : Elem) =
        let (html, js) = elem.Render

        sprintf "qi('%s').insertAdjacentHTML('%s', '%s');%s"
            target position html js
        |> action

    let insertBefore = insertAdjacent "beforebegin"
    let insertAfter  = insertAdjacent "afterend"

    let clear (target : string) =
        sprintf
            "(function(){var x = qi('%s'); while (x.firstChild)
             x.removeChild(x.firstChild);})();"
            target
        |> action

    let remove (target : string) =
        sprintf
            "(function(){var x=qi('%s'); x && x.parentNode.removeChild(x);})();"
            target
        |> action

    let redirect (url : string) =
        sprintf "(function(){document.location = '%s';})();" url
        |> action

    let display elem status =
        sprintf "(function(){var x = qi('%s'); if (x) x.style.display = '%s';})();"
            elem status
        |> action

    let show elem = display elem "block"
    let hide elem = display elem "none"