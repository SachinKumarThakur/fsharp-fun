open System

let int0 = int '0'
let inline toChar (i: int): char = char (i + int0)
let inline toInt (c: char): int = int c - int0
let inline intListToString (l: int list): string =
    List.map toChar l
    |> Array.ofList
    |> String

let addThree (a, b) carry =
    let a = toInt a
    let b = toInt b
    match a + b + carry with
    | 0 -> (0, 0)
    | 1 -> (1, 0)
    | 2 -> (0, 1)
    | 3 -> (1, 1)
    | _ -> failwith "Not possible"

// zips s1 and s2, right aligned, in reverse
// if either list is empty, puts as 0
// e.g. rightRevZip "10" "100" -> (0, 0), (1, 0), (0, 1)
let zipRightRev s1 s2 =
    let n1 = String.length s1
    let n2 = String.length s2
    [1..max n1 n2]
    |> List.map (fun i ->
        let a = if i > n1 then '0' else s1.[n1-i]
        let b = if i > n2 then '0' else s2.[n2-i]
        (a, b)
    )

// Adds two binary strings
let addStr (s1: string) (s2: string) =
    // Compute the addition of two lists
    // uses zipRightRev as a helper function to zip the lists
    // nicely together
    let l =
        ((0, 0), zipRightRev s1 s2)
        ||> List.scan (fun (_, carry) pair -> addThree pair carry)
        |> List.tail // the head of the list is just the initial state
        |> List.rev
    // Now, account for the fact that the carry might be 1
    // in this case, we extend the list by one element
    // Then, we save only the result list (getting rid of the carries)
    let carry = List.head l |> snd
    match carry with
    | 0 -> l
    | 1 -> (1, 0) :: l
    | _ -> failwith "carry is only 0 or 1"
    |> List.map fst
    |> intListToString

[<EntryPoint>]
let main _ =
    let str1 = "11001"
    let str2 = "11100"
    assert (addStr str1 str2 = "110101")
    0 // return an integer exit code