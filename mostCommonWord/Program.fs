// Computes the most common word in a string
// see: https://leetcode.com/problems/most-common-word/

//   Functional version, easiest to understand.
//   Just splits the string by commas, trims whitespace,
// filters out the ones not in the banned dictionary,
// then does a max count.
let mostCommonWord (bannedWords: Set<string>) (str: string)  =
    str.Split(',')
    |> Array.map (fun s -> s.Trim())
    |> Array.filter (fun s -> Set.contains s bannedWords |> not)
    |> Array.countBy id
    |> Array.maxBy snd
    |> fst

// Just the alphabet. Nothing special.
let alphabet =
    let a = "abcdefghijklmnopqrstuvwxyz"
    let b = a.ToUpperInvariant()
    (a + b).ToCharArray()

let ignored = [|','; ' '|]

//   Imperative version, optimized so that every character in the string
// is only processed once.
//   Not the most optimized, since it still does countBy and maxBy
// to compute the most common word, instead of computing it
// on the fly, but the point is to show how parsing can be done recursively.
let mostCommonWordOnePass (bannedWords: Set<string>) (str: string) =
    // create a one-pass lexer to identify words
    // Lex grammar:
    //   [a-zA-Z]*    return Ident;
    //   [ ,]*        ;
    let n = String.length str
    let rec splitter i lst =
        if i <> -1 then
            let nextIgnored = str.IndexOfAny(ignored, i)
            let nextEndpt =
                if nextIgnored = -1 then n else nextIgnored
            let s = str.Substring(i, nextEndpt - i)
            let nextAlphabet = str.IndexOfAny(alphabet, nextEndpt)
            if Set.contains s bannedWords then
                splitter nextAlphabet lst  // don't include s, it's banned
            else
                splitter nextAlphabet (s :: lst)
        else lst
    splitter 0 []
    |> List.countBy id
    |> List.maxBy snd
    |> fst

[<EntryPoint>]
let main argv =
    let testing = "a, a,a,   a, b,  c,b"
    let banned = set ["c"]
    assert (mostCommonWord banned testing = "a")
    assert (mostCommonWordOnePass banned testing = "a")
    let banned = set ["a"]
    assert (mostCommonWord banned testing = "b")
    assert (mostCommonWordOnePass banned testing = "b")
    0   // return 0