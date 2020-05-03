// Learn more about F# at http://fsharp.org

open System

let testing = [13; 19; 31; 37; 43; 61; 67; 73; 79; 97; 103; 109; 127; 139]

let inline remE a b = ((a % b) + b) % b
let inline remE' b a = remE a b

let correspondingB a p = 
    // finds the two bs corresponding to p = a^2 - ab + b^2
    // eqn is now b^2 - ab + (a^2 - p), solve using quadratic fn
    // b = a +- sqrt(a^2 - 4a^2 + 4p)
    let disc = a*a - 4*a*a + 4*p
    let d = int (sqrt (float disc))
    if d*d = disc then
        let b1 = a + d
        let b2 = a - d
        if b1 % 2 = 0 
        then Some (b1/2, b2/2)
        else None
    else None

let findPairsSumTo p =
    // Return pairs (a, b) s.t. a^2 - ab + b^2 = p
    // FACT: a, b are in the range (-p, p)
    // let remP a = remE a p
    [for a in [-p+1..p-1] do
        match correspondingB a p with
        | Some(b1, b2) -> 
            yield (a, b1)
            yield (a, b2)
        | None -> ()
    ]
    // |> List.filter (fun (a,b) -> a*a - a*b + b*b = p)

let findPrimary pairs = 
    let rem3 = remE' 3
    pairs
    |> List.map (
        fun (a,b) -> 
            let s = String.Format("{0} + {1}w", a, b)
            (s, a, b))
    |> List.filter (fun (_, a, b) -> rem3 a = 2 && rem3 b = 0)

let decomposeIntoPrimes = 
    findPairsSumTo >> findPrimary

let run p = 
    let rem7 = remE' 7
    findPairsSumTo p 
    |> findPrimary
    |> List.item 1
    |> (fun (s, a, b) -> 
        let a1 = a - 2*(b / 3) |> rem7 // mod 2+3w
        let a2 = a - (b / 3) |> rem7  // mod -1-3w
        printfn "%d & $%s$ & (%d, %d)" p s a1 a2
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
let main argv =
    match Array.length argv with
    | 0 -> testing |> List.map run |> ignore
    | 1 -> 
        decomposeIntoPrimes (int argv.[0])
        |> List.map (fun (s, _, _) -> s)
        |> List.iter (printfn "%s")
    | _ -> failwith "Usage: factoring_primes_eisenstein [prime]"
    0 // return an integer exit code
