namespace global

open System
open System.Text

[<AutoOpen>]
module private Auto =
    let inline sqr x = x * x

module Dbg =
    let Write (s:string) =
        let n = DateTime.Now
        let sb = StringBuilder("DEBUG ")
        sb.Append(n.ToString("dd MMM HH:mm:ss.fffffff")) |> ignore
        sb.Append "> " |> ignore
        sb.Append s |> ignore
        sb.ToString() |> Console.WriteLine