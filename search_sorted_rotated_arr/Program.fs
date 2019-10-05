// Learn more about F# at http://fsharp.org

let rec binSearch target arr =
    match Array.length arr with
    | 0 -> false
    | 1 -> target = Array.head arr
    | i -> 
        let middle = i / 2
        if target < arr.[middle] then
            binSearch target arr.[..middle-1]
        else if target > arr.[middle] then
            binSearch target arr.[middle+1..]
        else true

// arr must be sorted and rotated 
// e.g. [|3;4;5;1;2|]
let rec find target arr = 
    match Array.length arr with
    | 0 -> false
    | 1 -> target = Array.head arr
    | i -> 
        let middle = i / 2 //  5->2, 1->0
        if target = arr.[middle] then true
        else 
            if arr.[0] < arr.[middle] then
            // left half of array is sorted
                if target >= arr.[0] && target < arr.[middle] then
                    // target is bounded between first and middle - 1
                    find target arr.[..middle-1]
                else 
                    // target is in the unsorted half
                    find target arr.[middle+1..]
            else 
                // right half of array is sorted
                if target > arr.[middle] && target <= (Array.last arr) then
                    // target is in the sorted half
                    find target arr.[middle+1..]
                else 
                    // target is in the unsorted half
                    find target arr.[..middle-1]

let rec findAlt target arr = 
    let rec find2 target arr l r =
        match r - l with
        | 0 -> false
        | 1 -> target = Array.get arr l
        | i -> 
            let middle = i / 2 //  5->2, 1->0
            let leftElt = Array.get arr l
            let rightElt = Array.get arr r
            let midElt = Array.get arr middle
            if target = midElt then true
            else 
                if leftElt < midElt then
                // left half of array is sorted
                    if target >= leftElt && target < midElt then
                        // target is bounded between first and middle - 1
                        find2 target arr l (middle-1)
                    else 
                        // target is in the unsorted half
                        find2 target arr (middle+1) r
                else 
                    // right half of array is sorted
                    if target > midElt && target <= rightElt then
                        // target is in the sorted half
                        find2 target arr (middle+1) r
                    else 
                        // target is in the unsorted half
                        find2 target arr l (middle-1)
    find2 target arr 0 (Array.length arr - 1)





[<EntryPoint>]
let main argv =
    0 // return an integer exit code
