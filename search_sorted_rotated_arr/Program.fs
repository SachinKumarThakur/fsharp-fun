// Learn more about F# at http://fsharp.org

let rec binSearch target arr =
    match Array.length arr with
    | 0 -> None
    | i -> 
        let middle = i / 2
        if target < arr.[middle] then
            if middle = 0 then None 
            else binSearch target arr.[..middle-1]
        else if target > arr.[middle] then
            if middle = i then None
            else binSearch target arr.[middle+1..]
        else 
            Some target

let findPivotInd arr = 
    let rec findPivotIndHelper arr firstElt = 
        match Array.length arr with
        | 0 -> None
        | 1 -> None
        | i ->
            let middle = i / 2
            if arr.[middle] > arr.[middle + 1] then
                // This is the pivot
                Some middle
            else if arr.[middle] > firstElt then
                // pivot is in right half
                if middle = i then None 
                else findPivotIndHelper arr.[middle+1..] firstElt
            else
                // pivot in left half
                if middle = 0 then None 
                else findPivotIndHelper arr.[..middle-1] firstElt
    match Array.length arr with 
    | 0 -> None
    | _ ->
        findPivotIndHelper arr arr.[0]



[<EntryPoint>]
let main argv =
    findPivotInd [|1;2|]
    |> printfn "%A"
    0 // return an integer exit code
