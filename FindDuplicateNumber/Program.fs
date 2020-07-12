// Learn more about F# at http://fsharp.org
// Solves the leetcode problem Find Duplicate Number
// Based on: https://leetcode.com/problems/find-the-duplicate-number/discuss/72844/

// Finds the duplicate in arr
let findDuplicateNum (arr: int list): int =
    // search is binary search, in the range [left, right)
    let rec search l r =
        // base case
        if (r - l) <= 1 then l else
        // recursive case
        let mid = (l + r) / 2
        // count the number of elements less than the middle
        let numEltsLessThanMid =
            arr
            |> List.filter (fun x -> x < mid)
            |> List.length
        if numEltsLessThanMid < mid then
            // Since there are less elements than we expect
            // this means the duplicate number is to the right of mid
            search mid r
        else
            // Since there are more elements than we expect
            // that are lesser than mid
            // This means that the duplicate number is to the left of mid
            search l mid
    search 1 (List.length arr)

[<EntryPoint>]
let main _ =
    assert ((findDuplicateNum [1; 3; 4; 2; 2]) = 2)
    assert ((findDuplicateNum [5; 4; 1; 3; 3; 2]) = 3)
    assert ((findDuplicateNum [3; 1; 3; 4; 2]) = 3)
    0