// Learn more about F# at http://fsharp.org

open System
let FIELD_SIZE = 5

let add (a, b, c) (d, e, f) = 
    ( (a+d)%FIELD_SIZE, (b+e)%FIELD_SIZE, (c + f)% FIELD_SIZE)

let mult k (a, b, c) = 
    (k*a%FIELD_SIZE, k*b%FIELD_SIZE, k*c%FIELD_SIZE)

let genMultiples tup1 = 
    (Set.empty, [0 .. FIELD_SIZE - 1])
    ||> List.fold (fun s x -> Set.add (mult x tup1) s )

let genSubspace tup1 tup2 = 
    set [for t1 in genMultiples tup1 do
         for t2 in genMultiples tup2 do
         yield add t1 t2
    ]

let genTup() = 
    seq {
        for i in 0 .. FIELD_SIZE - 1 do
        for j in 0 .. FIELD_SIZE - 1 do
        for k in 0 .. FIELD_SIZE - 1 do
        yield (i,j,k)
    }

[<EntryPoint>]
let main _ =
    seq {for t1 in genTup() do
         for t2 in genTup() do
         let s = genSubspace t1 t2
         yield s}
    |> Set.ofSeq
    |> Set.toList
    |> List.countBy (fun s -> Set.count s)
    |> printfn "There are %A subspaces"
    0

