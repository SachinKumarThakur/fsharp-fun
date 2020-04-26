module math493
open System


let allPowers p gen = 
    (gen, [1.. p-2])
    ||> List.scan (fun acc x -> acc * gen % p)
    |> List.distinct

let findGenerator p = 
    [2 .. p - 1]
    |> List.map (allPowers p)
    |> List.filter (fun lst -> List.length lst = p - 1)
    |> List.head

let printGenerator p = 
    let lst = findGenerator p
    printfn "%d & %d & %A\\ \hline" p (List.head lst) lst

[<EntryPoint>]
let main _ = 
    [3; 5; 7; 11; 13; 17; 19]
    |> List.map printGenerator
    |> ignore
    0
