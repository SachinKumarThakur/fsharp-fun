module LongestCommonSubsequence
open System

type ListWithLen = {l: char list; len: int}

let longest a b = 
    if a.len <= b.len then b else a

let LCS s1 s2 = 
    // construct LCS table
    let n = String.length s1
    let m = String.length s2
    let lcsMap = Array2D.create (n+1) (m+1) {l = []; len = 0}
    for i = n-1 downto 0 do
        for j = m-1 downto 0 do
            lcsMap.[i, j] <- if s1.[i] = s2.[j] then
                                { l = s1.[i] :: lcsMap.[i+1, j+1].l;
                                  len = lcsMap.[i+1, j+1].len + 1 }
                             else
                                longest lcsMap.[i+1, j] lcsMap.[i, j+1]
    let lcsArr = Array.ofList lcsMap.[0,0].l
    String.Concat(lcsArr)
    

[<EntryPoint>]
let main _ =
    let str1 = Console.ReadLine()
    let str2 = Console.ReadLine()
    LCS str1 str2
    |> printfn "%s"

    0 // return an integer exit code
