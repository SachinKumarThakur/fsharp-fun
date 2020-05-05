open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

type Digraph = 
    { nodes: int list;
    adjacency: Map<int, int list>; }

let createMapFromEdgeList el = 
    el
    |> List.groupBy fst
    |> List.map (fun (u,l) -> (u, List.map snd l))
    |> Map.ofList

let getRandomBool p = 
    let randBoolStream = 
        let rnd = Random()
        Seq.initInfinite(fun _ -> rnd.NextDouble() < p)
    Seq.head randBoolStream

let makeRandomList p size = 
    let allNodesEmpty = 
        List.init size (fun i -> (i+1))
    [1..size]
    |> List.collect (fun i ->
        [i+1..size]
        |> List.map (fun j ->
            if getRandomBool p then Some (i, j) else None
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
    

// Benchmarking starts here

let createTransposeEdges sl = 
    let tailLength = List.length sl - 1
    let sink = 
        List.head sl
        |> List.replicate tailLength
    let sources = 
        List.tail sl
    List.zip sources sink

let createTransposeGraph sll = 
    let graphNodes = List.map List.head sll
    let adj = 
        List.collect createTransposeEdges sll
        |> createMapFromEdgeList
    {nodes = graphNodes; adjacency = adj;}

type TestTransposeCreating() =
    let mutable dg = {nodes=[0]; adjacency= Map.empty;}
    [<Params(256, 512, 1024)>]
    member val public listSize = 0 with get, set

    [<GlobalSetup>]
    member self.Setup() =
        dg <- makeRandomList 0.2 self.listSize
                |> createTransposeGraph

    [<Benchmark>]
    member self.BenchCreateGraph() =
        dg |> costOfModules

        
















(*


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


*)
[<EntryPoint>]
let main _ =
    let summary = BenchmarkRunner.Run<TestTransposeCreating>()
    0 // return an integer exit code
