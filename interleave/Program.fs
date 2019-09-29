// Learn more about F# at http://fsharp.org

open System
let rec interleave lst1 lst2 =
    match lst1 with
    | [] -> lst2
    | x::xs -> x :: interleave lst2 xs


[<EntryPoint>]
let main _ =
    interleave [3;4;6;7;8] [1;2]
    |> (=) [3;1;4;2;6;7;8]
    |> printfn "%b"
    interleave [1;2] [3;4]
    |> (=) [1;3;2;4]
    |> printfn "%b"
    0 // return an integer exit code
