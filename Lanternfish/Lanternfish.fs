module Lanternfish

open System

let input =
    IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let parse (c: char) (s: string) = s.Split c

let generation age =
    match age with
    | 0 -> seq [ 6; 8 ]
    | n -> seq [ n - 1 ]

let generation2 (age: int, count: int64) =
    match age with
    | 0 -> seq [ (6, count); (8, count) ]
    | n -> seq [ (n - 1, count) ]

let merge (counts: seq<int * int64>) =
    seq [ for age in [ 0 .. 8 ] do
              let n =
                  counts
                  |> Seq.filter (fst >> (=) age)
                  |> Seq.sumBy snd

              (age, n) ]

let day =
    let mutable fish =
        input |> Seq.head |> parse ',' |> Seq.map int

    [ 0 .. 79 ]
    |> Seq.iter (fun _ -> fish <- Seq.collect generation fish)

    printfn "%A" fish
    fish |> Seq.length

let days n =
    let mutable fish =
        input
        |> Seq.head
        |> parse ','
        |> Seq.map int
        |> Seq.countBy id
        |> Seq.map (fun (a, c) -> (a, int64 c))

    let runGeneration _ =
        // printfn "%A" fish
        fish <- Seq.collect generation2 fish |> merge

    [ 0 .. (n - 1) ] |> Seq.iter runGeneration
    printfn "%A" runGeneration
    Seq.sumBy snd fish

let solve s = days 80
solve input
let solve2 s = days 256
solve2 input
