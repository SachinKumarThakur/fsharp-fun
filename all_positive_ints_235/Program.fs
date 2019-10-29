// Learn more about F# at http://fsharp.org

open System.Numerics

let scale iterable scaleFactor = seq {
    for i in iterable -> scaleFactor * i }

let rec merge iter1 iter2 = 
    (iter1, iter2)
    |> Seq.unfold (fun (i1, i2) ->
        let num1 = Seq.head i1
        let num2 = Seq.head i2
        if num1 = num2 then
            Some (num1, ((Seq.skip 1 i1), (Seq.skip 1 i2)))
        else if num1 < num2 then
            Some (num1, ((Seq.skip 1 i1), i2))
        else
            Some (num2, (i1, (Seq.skip 1 i2)))
    )

let printSeq sequence num = 
    List.ofSeq (Seq.take num sequence)
    |> printfn "%A"

let rec makeAllFactors = seq {
    yield 1
    let res = (scale makeAllFactors 2)
                |> merge (scale makeAllFactors 3)
                |> merge (scale makeAllFactors 5)
    yield! res
}

let unfolder mySet = 
    let (m: bigint) = Set.minElement mySet
    let newSet = 
        Set.remove m mySet
        |> Set.add (BigInteger.Two *m)
        |> Set.add (3*m)
        |> Set.add (5*m)
    Some (m, newSet)

let heapFactors = Seq.unfold unfolder (Set.add BigInteger.One Set.empty)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
