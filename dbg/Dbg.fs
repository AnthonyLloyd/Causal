namespace global

open System
open System.Text

[<AutoOpen>]
module private Auto =
    let inline sqr x = x * x

module Dbg =
    let Write (s:string) =
        [|
            "\u001b[32mDEBUG "
            DateTime.Now.ToString("dd MMM HH:mm:ss.fffffff")
            "> "
            s
            "\u001b[0m"
        |]
        |> String.Concat
        |> stdout.WriteLine