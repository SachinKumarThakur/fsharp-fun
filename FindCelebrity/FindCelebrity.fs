let m1 =
    array2D [ [1; 1; 1];
              [0; 1; 1];
              [0; 0; 1];
            ]
    |> Array2D.map (fun i -> i = 1)

let m2 =
    array2D [ [1; 1; 1; 1; 1];
              [1; 1; 1; 1; 1];
              [1; 1; 1; 1; 1];
              [1; 1; 1; 1; 1];
              [0; 0; 0; 0; 1]
            ]
    |> Array2D.map (fun i -> i = 1)

let m3 =
    array2D [ [1; 1; 1; 1; 1];
              [1; 1; 1; 0; 1];
              [0; 0; 1; 0; 0];
              [1; 1; 1; 1; 0];
              [1; 1; 1; 1; 1];
            ]
    |> Array2D.map (fun i -> i = 1)

let m4 =
    array2D [ [1; 0]
              [1; 1]
            ]
    |> Array2D.map (fun i -> i = 1)

let mCycle =
    array2D [ [1; 1; 0; 1]
              [0; 1; 1; 1]
              [1; 0; 1; 1]
              [0; 0; 0; 0]
            ]
    |> Array2D.map (fun i -> i = 1)

let makeKnows (m: bool[,]) =
    fun a b -> m.[a-1, b-1]

let findCeleb knows n =
    let mutable result = 1
    for i in 2..n do
        if knows result i then
            result <- i
    result

[<EntryPoint>]
let main _ =
    assert (findCeleb (makeKnows m1) 3 = 3)
    assert (findCeleb (makeKnows m2) 5 = 5)
    assert (findCeleb (makeKnows m3) 5 = 3)
    assert (findCeleb (makeKnows m4) 2 = 1)
    assert (findCeleb (makeKnows mCycle) 4 = 4)
    0 // return an integer exit code