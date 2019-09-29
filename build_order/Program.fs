// Learn more about F# at http://fsharp.org

open System
open LibTree

(* Given a char list of projects, and a list of pairs of dependencies,
    generate an order which the projects should execute in
    e..g
    Projects = [a;b;c;d;e;f;g]
    Deps     = [(a,b), (c,d), (e,f), (b,g), (d,g), (f,g)]

    Possible output = [a;c;e;b;d;f;g] *)

let buildOrder listOfProjects

[<EntryPoint>]
let main _argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
