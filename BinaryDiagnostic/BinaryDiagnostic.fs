module BinaryDiagnostic

open System

let input =
    IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"
        .Split("\n")

let convert n = Convert.ToInt32(n, 2)

type Tally = { One: int; Zero: int }

let solve input =
    let aggregate tallies n =
        tallies
        |> Seq.zip n
        |> Seq.map
            (fun (digit, tally) ->
                if digit = '0' then
                    { tally with Zero = tally.Zero + 1 }
                else
                    { tally with One = tally.One + 1 })

    let tallies =
        input
        |> Seq.fold aggregate (List.replicate (example.Length) { One = 0; Zero = 0 })
        |> Seq.toList

    let gamma =
        tallies
        |> Seq.map (fun x -> if x.One > x.Zero then 1 else 0)
        |> Seq.map string
        |> String.concat ""
        |> convert

    let epsilon =
        tallies
        |> Seq.map (fun x -> if x.One > x.Zero then 0 else 1)
        |> Seq.map string
        |> String.concat ""
        |> convert

    let product = gamma * epsilon
    gamma, epsilon, product