module ImplementingABuilder
open System

type TraceBuilder() =
    member this.Bind(m, f) =
        match m with
        | None ->
            printfn "Binding with None. Exiting."
        | Some a ->
            printfn "Binding with Some(%A). Continuing" a
        Option.bind f m

    member this.Return(x) =
        printfn "Returning a unwrapped %A as an option" x
        Some x

    member this.ReturnFrom(m) =
        printfn "Returning an option (%A) directly" m
        m

    member this.Zero() =
        printfn "Zero"
        None

    member this.Yield(x) =
        printfn "Yield an unwrapped %A as an option" x
        Some x

    member this.YieldFrom(x) =
        printfn "Yield an option (%A) directly" x
        Some x

    member this.Combine (m,f) =
        printfn "Combine. Starting second param %A" f
        let y = f()
        printfn "Combine. Finished second param %A. Result is %A" f y

        match m,y with
        | Some a, Some b ->
            printfn "combining %A and %A" a b
            Some (a + b)
        | Some a, None ->
            printfn "combining %A with None" a
            Some a
        | None, Some b ->
            printfn "combining None with %A" b
            Some b
        | None, None ->
            printfn "combining None with None"
            None

    member this.Delay(funcToDelay) =
        let delayed = fun () ->
            printfn "%A - Starting Delayed Fn." funcToDelay
            let delayedResult = funcToDelay()
            printfn "%A - Finished Delayed Fn. Result is %A" funcToDelay delayedResult
            delayedResult  // return the result

        printfn "%A - Delaying using %A" funcToDelay delayed
        delayed // return the new function

    member this.Run(funcToRun) =
        printfn "%A - Run Start." funcToRun
        let runResult = funcToRun()
        printfn "%A - Run End. Result is %A" funcToRun runResult
        runResult // return the result of running the delayed function

let trace = TraceBuilder()

type IntOrBool = I of int | B of bool

let parseInt (s: string) =
    match Int32.TryParse(s) with
    | true, i -> Some (I i)
    | false, _ -> None

let parseBool (s: string) =
    match Boolean.TryParse(s) with
    | true, i -> Some (B i)
    | false, _ -> None

let map1 = [ ("1", "One"); ("2", "Two") ] |> Map.ofList
let map2 = [ ("A", "Alice"); ("B", "Bob") ] |> Map.ofList

trace {
    return! map1.TryFind "A"
    return! map2.TryFind "A"
    } |> printfn "Result for map lookup: %A"

// ---------- List builder
type ListBuilder() =
    member this.Bind(m, f) =
        printfn "Binding %A" m
        m |> List.collect f

    member this.Zero() =
        printfn "Zero"
        []

    member this.Yield(x) =
        printfn "Yield an unwrapped %A as a list" x
        [x]

    member this.YieldFrom(m) =
        printfn "Yield a list (%A) directly" m
        m

    member this.For(m,f) =
        printfn "For %A" m
        this.Bind(m,f)

    member this.Combine (a,b) =
        printfn "combining %A and %A" a b
        List.concat [a;b]

    member this.Delay(f) =
        printfn "Delay"
        f()

// make an instance of the workflow
let listbuilder = ListBuilder()

listbuilder {
    for i in ["go";"blue"] do
        yield i
        for j in ["Michigan";"Rocks"] do
            yield! [j; "yeahh"]
        yield "potato"
    } |> printfn "Result for for..in..do : %A"

// Maybe builder
module MaybeBuilder =
    type Maybe<'a> = Maybe of Lazy<'a option>

    // for MaybeBuilder
    let Bind m f = Option.bind f m
    let Return x = Some x
    let ReturnFrom (Maybe f) = f.Value
    let Zero = None
    let Combine a b =
        match a with
        | Some _ -> a // if a is good, skip b
        | None -> b()
    let Delay f = f
    let Run f = Maybe (lazy f())

    // Helper functions
    let runStep (Maybe f) = f.Value

type MaybeBuilder() =
    member _.Bind(m, f) = MaybeBuilder.Bind m f
    member _.Return(x) = MaybeBuilder.Return x
    member _.ReturnFrom(x) = MaybeBuilder.ReturnFrom x
    member _.Zero() = MaybeBuilder.Zero
    member _.Combine(a, b) = MaybeBuilder.Combine a b
    member _.Delay(f) = MaybeBuilder.Delay f
    member _.Run(f) = MaybeBuilder.Run f
// make an instance of the workflow
let maybe = MaybeBuilder()

let childWorkflow =
    maybe {printfn "Child workflow"}
maybe {
    printfn "Part 1: about to return 1"
    return 1
    return! childWorkflow
    } |> printfn "Result for Part1 but not childWorkflow: %A" 