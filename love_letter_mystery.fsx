(*  Well, we first need to check each pair of characters
    Whichever pair is higher in num value, add that to the count
*)

open System

let chunkSize str = 
    let strSize = String.length str
    if strSize % 2 = 0 then
        strSize / 2
    else
        strSize / 2 + 1

let splitStrIntoTwoArrays (str: string) = 
    let chunkedArray = 
        str.ToCharArray() 
        |> Array.chunkBySize (chunkSize str)
    let firstArr = chunkedArray.[0]
    let secondArr = chunkedArray.[1]
    if Array.length firstArr = Array.length secondArr then
        (firstArr, Array.rev secondArr)
    else
        (firstArr.[0..((Array.length firstArr) - 2)], Array.rev secondArr)

let diffBetweenTwoCharInTuple tup = 
    let charDifference = int (fst tup) - int (snd tup)
    abs charDifference


let numChangesToPalindrome str: int =
    let firstArr, secondArr = splitStrIntoTwoArrays str
    Array.zip firstArr secondArr
    |> Array.sumBy diffBetweenTwoCharInTuple

let numChangesToPalindromeMut str = 
    let mutable changes = 0
    for index in 0 .. (((String.length str) / 2) - 1) do
        let firstChar = str.[index]
        let secondChar = str.[(String.length str) - index - 1]
        let charDifference = (int firstChar) - (int secondChar)
        changes <- changes + abs charDifference
    changes

let numLines = Console.ReadLine() |> int
for _ in [1..numLines] do
    numChangesToPalindrome (Console.ReadLine())
    |> printfn "%d"
