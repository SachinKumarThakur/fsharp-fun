// Learn more about F# at http://fsharp.org

open System

let testing = [13; 19; 31; 37; 43; 61; 67; 73; 79; 97; 103; 109; 127; 139]

let inline remE a b = ((a % b) + b) % b
let inline remE' b a = ((a % b) + b) % b

let findPairsSumTo p =
    // Return pairs (a, b) s.t. a^2 - ab + b^2 = p
    // FACT: a, b are in the range (-p, p)
    // let remP a = remE a p
    [for a in [-p+1..p-1] do for b in [-p+1..p-1] -> (a,b)]
    |> List.filter (fun (a,b) -> a*a - a*b + b*b = p)

let findPrimary pairs = 
    let rem3 a = remE a 3
    pairs
    |> List.map (
        fun (a,b) -> 
            let s = String.Format("{0} + {1}w", a, b)
            (s, a, b))
    |> List.filter (fun (_, a, b) -> rem3 a = 2 && rem3 b = 0)

let run p = 
    let rem7 x = remE x 7
    findPairsSumTo p 
    |> findPrimary
    |> List.item 1
    |> (fun (s, a, b) -> 
        let a1 = a - (b / 3) |> rem7  // mod -1-3w
        let a2 = a - 2*(b / 3) |> rem7 // mod 2+3w
        printfn "%d: %s -- (%d, %d)" p s a1 a2
    )

let isACube p n = 
    // returns whether 7 is a cube mod p
    seq {for x in 2..p-1 -> remE' p (x * x * x)} |> set
    |> Set.contains n

let is7ACube p = isACube p 7

let findCubes l = 
    l
    |> List.map (fun i -> (i, is7ACube i))
    |> List.filter snd

[<EntryPoint>]
let main _argv =
    testing |> List.map run |> ignore
    0 // return an integer exit code
