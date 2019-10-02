module takeUntilDup
open System



// foldList :: ('State -> 'State) -> 'State -> 'T list -> 'State
let rec foldList fn (acc: 'State) (l: 'T list) = 
    match l with
    | [] -> acc
    | x :: xs -> foldList fn (fn acc) xs

// foldCont :: ('State -> 'T -> ('State -> 'State) -> 'State) -> 'State -> 'T list -> 'State
let rec foldCont cont (acc: 'State) (l: 'T list)  = 
    match l with 
    | [] -> acc
    | x :: xs -> cont acc x (fun lacc -> foldCont cont lacc xs)

// takeUntilDuplicate :: 'a list -> 'a list
let takeUntilDuplicate str = 
    (([], Set.empty), str)
    ||> foldCont (fun (lst, uniqueChars) ch cont ->
                    match Set.contains ch uniqueChars  with
                    | true -> (lst, uniqueChars)
                    | false -> cont (ch :: lst, Set.add ch uniqueChars)
                    )
    |> fst
    |> List.rev

let rec dropWhileRec p = function
    | [] -> []
    | x :: xs -> if p x then dropWhileRec p xs else x :: xs
    
let dropWhile p str = 
    (str, ([], []))
    ||> List.foldBack (fun x (ys, xs)  -> (if p x then ys else x :: xs), x :: xs)
    |> fst

let dropWhile2 f (str: string) = 
    List.ofArray (str.ToCharArray())
    |> f (fun a -> a = 'a')

let constant a = function
    | _ -> a

let lambda1 x cont set: char list =
    match Set.contains x set with
    | true -> []
    | false -> x :: cont (Set.add x set)
    
let takeUntilDup (xs: char list): char list = 
    let g = List.foldBack lambda1 xs (constant [])
    g Set.empty

let lda2 xs = 
    let f = (xs, (fun a -> ([], a)))
            ||> Seq.foldBack (fun x cont set -> 
                match Set.contains x set with
                | true -> ([], set)
                | false ->
                    let (lst, s) = cont (Set.add x set)
                    (x :: lst, s)
                )
    f Set.empty
    
