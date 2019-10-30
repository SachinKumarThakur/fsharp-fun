module Digraph

type Digraph = 
    { n: int;
    adjacency: Map<int, int list>; }

let makeFromAdjacencyList n adjacency: Digraph = 
    {n = n; adjacency = adjacency;}

let makeFromEdges n (edgeList: (int * int) list): Digraph = 
    // for (u,v) in list do
    //     add v to u's adjacency list 
    let adjcency = 
        List.groupBy fst edgeList
        |> List.map (fun (u,l) -> (u, List.map snd l))
        |> Map.ofList
    {n = n; adjacency = adjcency}

// Returns the list of finish times
let dfs (graph: Digraph) = 
    // Recursive helper function
    let dummyFinishTime = -1
    let rec dfsVisit visited node time =
        let newVisited = Map.add node dummyFinishTime visited
        let visitedAfterFold, finishTime = 
            match Map.tryFind node graph.adjacency with
            | None -> (newVisited, time)
            | Some(lst) ->
                ((newVisited, time), lst)
                ||> List.fold (fun (visitedMap, timeStamp) n ->
                    match Map.containsKey n visitedMap with
                    | true -> (visitedMap, timeStamp)
                    | false -> dfsVisit visitedMap n timeStamp
                )
        let finishTime = finishTime + 1
        let finalMap = Map.add node finishTime visitedAfterFold
        (finalMap, finishTime)
    // DFS
    let startTime = 0
    ((Map.empty, startTime), [1..graph.n])
    ||> List.fold (fun (visited, timeStamp) node -> 
        match Map.containsKey node visited with
        | true -> (visited, timeStamp)
        | false -> dfsVisit visited node timeStamp
        )
    |> fst

let descFinishTimes graph = 
    let m = dfs graph
    [1..graph.n]
    |> List.sortBy (fun i -> Map.find i m)
    |> List.rev

makeFromEdges 5 [(1,2); (1,3); (1,4); (2,3); (4,5)]
|> descFinishTimes
|> printfn "%A"