// Learn more about F# at http://fsharp.org

let rec loop tmp i =
    if i <= 0 then tmp else
    let tmp2 = loop (Array.copy tmp) (i-1)
    tmp2.[0] <- tmp2.[0] + 1
    tmp2


[<EntryPoint>]
let main _ =
    let a = loop [|1..10000|] 10000
    printfn "%d" a.[0]
    0
