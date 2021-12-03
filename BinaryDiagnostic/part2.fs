module part2

open System

let input =
    IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let convert n = Convert.ToInt32(n, 2)

let rec filter index chooser (numbers: string seq) =
    if numbers |> Seq.length = 1 then
        numbers |> Seq.head
    else
        let mostCommonBit =
            numbers
            |> Seq.map (Seq.item index)
            |> Seq.countBy id
            |> chooser

        let filtered =
            numbers
            |> Seq.filter (fun digit -> digit.[index] = mostCommonBit)
            |> Seq.toList

        filter (index + 1) chooser filtered

let filterOxygenGeneratorRating =
    filter 0 (Seq.sortDescending >> Seq.maxBy snd >> fst)

let filterCO2Rating =
    filter 0 (Seq.sort >> Seq.minBy snd >> fst)

let solve (input: string seq) =
    let oxygenGeneratorRating =
        input |> filterOxygenGeneratorRating |> convert

    let co2Rating = input |> filterCO2Rating |> convert

    oxygenGeneratorRating * co2Rating
