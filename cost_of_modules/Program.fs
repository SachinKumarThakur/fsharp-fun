open System

type Digraph = 
    { nodes: string list;
    adjacency: Map<string, string list>; }



let createTransposeEdges sl = 
    let tailLength = List.length sl - 1
    let sink = 
        List.head sl
        |> List.replicate tailLength
    let sources = 
        List.tail sl
    List.zip sources sink

let createMapFromEdgeList el = 
    el
    |> List.groupBy fst
    |> List.map (fun (u,l) -> (u, List.map snd l))
    |> Map.ofList

let createTransposeGraph sll = 
    let graphNodes = List.map List.head sll
    let adj = 
        List.collect createTransposeEdges sll
        |> createMapFromEdgeList
    {nodes = graphNodes; adjacency = adj;}


let countNumChildren node costMap graph = 
    let mutable costMap = costMap
    let rec dfs node = 
        match Map.tryFind node costMap with
        | Some (s) -> s
        | None ->
            match Map.tryFind node graph.adjacency with
            | None -> 
                // Node is a leaf, so add to costMap with val 1
                let s = Set.add node Set.empty
                costMap <- Map.add node s costMap
                s
            | Some(lst) ->
                let s = 
                    lst
                    |> List.map dfs
                    |> List.reduce (Set.union)
                    |> Set.add node
                costMap <- Map.add node s costMap
                s
    dfs node |> ignore
    costMap


let costOfModules g = 
    let mutable costMap = Map.empty
    g.nodes
    |> List.iter (fun node ->
        match Map.containsKey node costMap with
        | true -> ()
        | false -> 
            costMap <- countNumChildren node costMap g
    )
    costMap
    |> Map.map (fun _ v -> Set.count v)


module Test = 
    let testing = 
        [
            ["A"; "S"; "E"; "N"];
            ["S"; "H"; "N"];
            ["E"; "N"];
            ["H"];
            ["N"];
        ]
    let getRandomBool p = 
        let randBoolStream = 
            let rnd = Random()
            Seq.initInfinite(fun _ -> rnd.NextDouble() < p)
        Seq.head randBoolStream

    let genRandomDag p size = 
        let m = 
            [1..size]
            |> List.collect (fun i ->
                [i+1..size]
                |> List.map (fun j ->
                    if getRandomBool p then Some (string i, string j) else None
                    )
                )
            |> List.choose id
            |> createMapFromEdgeList
        let n = [1..size] |> List.map string
        {nodes = n; adjacency = m;}

    let testRandom() = 
        genRandomDag 0.3 200
        |> costOfModules
        |> ignore

    let makeRandomList p size = 
        let allNodesEmpty = 
            List.init size (fun i -> string (i+1))
        [1..size]
        |> List.collect (fun i ->
            [i+1..size]
            |> List.map (fun j ->
                if getRandomBool p then Some (string i, string j) else None
                )
            )
        |> List.choose id
        |> createMapFromEdgeList
        |> (fun m -> 
            List.filter (fun elt -> not <| Map.containsKey elt m) allNodesEmpty
            |> List.fold (fun M elt -> Map.add elt [] M) m
        )
        |> Map.toList
        |> List.map (fun (u, l) -> [yield u; yield! l])
    
    let printStringList sll = 
        sll
        |> List.iter (String.concat " " >> printfn "%s")


        
let runOnInput() = 
    let numLines = Console.ReadLine() |> int
    [1..numLines]
    |> List.map (fun _ -> Console.ReadLine().Split(' ') |> List.ofArray)
    |> createTransposeGraph
    |> costOfModules
    |> Map.iter (fun k v -> printfn "%s, %d" k v)

let printRandomGraph p n = 
    printfn "%d" n
    Test.makeRandomList p n
    |> Test.printStringList


[<EntryPoint>]
let main argv =
    match Array.length argv with
    | 0 -> runOnInput()
    | 2 -> printRandomGraph(float argv.[0]) (int argv.[1])
    | _ -> failwith "Enter no arguments or 2 arguments" |> ignore
    0 // return an integer exit code
