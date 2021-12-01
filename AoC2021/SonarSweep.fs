module SonarSweep

open System

let input =
    IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"199
200
208
210
200
207
240
269
260
263"

let isIncreases (x, y) = x < y

let exp =
    example.Split("\n")
    |> Seq.map int
    |> Seq.pairwise
    |> Seq.filter isIncreases
    |> Seq.length

let res1 =
    input
    |> Seq.map int
    |> Seq.pairwise
    |> Seq.filter isIncreases
    |> Seq.length

let res2 =
    input
    |> Seq.map int
    |> Seq.windowed 3
    |> Seq.map Seq.sum
    |> Seq.pairwise
    |> Seq.filter isIncreases
    |> Seq.length
