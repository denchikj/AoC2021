module TheTreacheryofWhales

open System

let input =
    (IO.File.ReadAllText "input.txt").Split(',') |> Array.map int |> Array.sort

let solve = Array.sumBy(((-) input[input.Length / 2]) >> Math.Abs)
solve input

let avg = input |> Seq.averageBy decimal |> int 
let fuel n = n*(n+1)/2
let solve2 (x:int) = Array.sumBy(((-) x) >> Math.Abs >> fuel )
solve2 avg input