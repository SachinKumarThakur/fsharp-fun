(* Sliding window idea
    Find the index of a non-unique character of the string
    , given a set
    findFirstNonUnique: str -> set -> str * set option
*)
open System

let rec findFirstNonUnique str result charMap = 
    // function that if it is the set, breaks, else 
    if List.isEmpty str then
        ([], result, charMap)
    else 
        let ch = List.head str
        match Set.contains ch charMap with
        | true -> (str, result, charMap)
        | false -> findFirstNonUnique (List.tail str) 
                    (ch :: result) (Set.add ch charMap)

let takeWhileNotUnique str =
    let (_, result, _) = findFirstNonUnique str [] Set.empty
    List.rev result
    |> Array.ofList
    |> String




let longestUniqueSubstring str = // str -> str

    "hello"

[<EntryPoint>]
let main argv =
    longestUniqueSubstring "abcabde"
    |> printfn "%s"
    0 // return an integer exit code
