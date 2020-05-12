// following the tutorial:
// https://fsharpforfunandprofit.com/posts/computation-expressions-bind/
open System
type LoggingBuilder() =
    let log p = printfn "expression is %A" p

    member this.Bind(x, f) =
        log x
        x |> f

    member this.Return(x) = x

type MaybeBuilder() =
    member this.Bind(x, f) = Option.bind f x
    member this.Return(x) = Some x

let divideBy b a =
    if b = 0 then None else Some(a/b)

let divideByWorkflow init x y z =
    let maybe = new MaybeBuilder()
    maybe
        {
        let! a = init |> divideBy x
        let! b = a |> divideBy y
        let! c = b |> divideBy z
        return c
        }

let loggingWorkflow() =
    let logger = new LoggingBuilder()
    logger {
        let! x = 42
        let! y = 43
        let! z = x + y
        return z
    }

let map1 = [ ("1","One"); ("2","Two") ] |> Map.ofList
let map2 = [ ("A","Alice"); ("B","Bob") ] |> Map.ofList
let map3 = [ ("CA","California"); ("NY","New York") ] |> Map.ofList

let multiLookupOld key =
    match map1.TryFind key with
    | Some result1 -> Some result1   // success
    | None ->   // failure
        match map2.TryFind key with
        | Some result2 -> Some result2 // success
        | None ->   // failure
            match map3.TryFind key with
            | Some result3 -> Some result3  // success
            | None -> None // failure

type OrElseBuilder() =
    member this.ReturnFrom(x) = x
    member this.Combine (a,b) =
        match a with
        | Some _ -> a  // a succeeds -- use it
        | None -> b    // a fails -- use b instead
    member this.Delay(f) = f()

let multiLookup key =
    let orElse = new OrElseBuilder()
    orElse {
        return! map1.TryFind key
        return! map2.TryFind key
        return! map3.TryFind key
    }

// ----------- LESSON 2 : continuations
let divide ifZero ifSuccess top bottom =
    if (bottom=0)
    then ifZero()
    else ifSuccess (top/bottom)

let isEven ifOdd ifEven aNumber =
    if (aNumber % 2 = 0)
    then aNumber |> ifEven
    else aNumber |> ifOdd

let ifZero1 () = printfn "bad"
let ifSuccess1 x = printfn "good %i" x
let divide1  = divide ifZero1 ifSuccess1

let ifZero2() = None
let ifSuccess2 x = Some x
let divide2  = divide ifZero2 ifSuccess2

let ifZero3() = invalidOp "div by 0"
let ifSuccess3 x = x
let divide3  = divide ifZero3 ifSuccess3

let pipeIntoLogging (someExpression,lambda) =
   printfn "expression is %A" someExpression
   someExpression |> lambda

let pipeIntoMaybe (someExpression,lambda) =
   match someExpression with
   | None -> None
   | Some x -> x |> lambda
let return' c = Some c

let divideByWorkflow2 x y w z =
    pipeIntoMaybe (x |> divideBy y, fun a ->
    pipeIntoMaybe (a |> divideBy w, fun b ->
    pipeIntoMaybe (b |> divideBy z, fun c ->
    return' c
    )))

/// ----------- Lesson 3: Bind ---------
//let (>>=) m f = pipeIntoMaybe(m,f)
//let divideByWorkflow3 x y w z =
//    x |> divideBy y >>= divideBy w >>= divideBy z

// hw
let strToInt (str: string) =
    match Int32.TryParse(str) with
    | (true, i) -> Some i
    | (false, _) -> None

let stringAddWorkflow x y z =
    let maybeBuilder = MaybeBuilder()
    maybeBuilder {
        let! a = strToInt x
        let! b = strToInt y
        let! c = strToInt z
        return a + b + c
        }
let strAdd str i =
    strToInt str
    |> Option.bind (fun j -> Some (i + j))

let (>>=) m f = Option.bind f m
// let good = strToInt "1" >>= strAdd "2" >>= strAdd "3"

[<EntryPoint>]
let main _ =
    0