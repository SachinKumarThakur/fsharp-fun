module Digraph

type Digraph = 
    { n: int;
    adjacency: (int * int list) list; }

let makeFromAdjacencyList (n: int) (adjacency: (int * int list) list): Digraph = 
    {n = n; adjacency = adjacency;}

let makeFromEdges (n: int) (edgeList: (int * int) list): Digraph = 
    // for (u,v) in list do
    //     add v to u's adjacency list 
    let adjcency = 
        List.groupBy fst edgeList
        |> List.map (fun (u,l) -> (u, List.map snd l))
    {n = n; adjacency = adjcency}
                
    
