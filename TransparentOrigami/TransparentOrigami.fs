module TransparentOrigami

open System

let input =
    IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

type Paper = Set<int * int>

type FoldInstruction =
    | FoldX of int
    | FoldY of int

let parsePoint (line: string) =
    let [| x; y |] = line.Split(",")
    (int x, int y)

let parseInstruction (line: string) =
    printfn "parsing %A" line
    let [| dim; line |] = line.Split(" ").[2].Split("=")

    match dim with
    | "x" -> FoldX <| int line
    | "y" -> FoldY <| int line
    | unknown -> failwithf "Unknown fold instruction: <%s> in <%s>" unknown line

let parse input =
    let pointTexts = input |> Array.takeWhile ((<>) "")

    let instructionTexts =
        input
        |> Array.skip (pointTexts |> Seq.length |> (+) 1)

    let points =
        pointTexts |> Array.map parsePoint |> Set.ofSeq

    let instructions =
        instructionTexts |> Array.map parseInstruction

    (points, instructions)

let foldX length (x, y) =
    if x > length then
        let maxX = length * 2
        (maxX - x, y)
    else
        (x, y)

let foldY length (x, y) =
    if y > length then
        let maxY = length * 2
        (x, maxY - y)
    else
        (x, y)

let fold paper instruction =
    match instruction with
    | FoldX length -> paper |> Set.map (foldX length)
    | FoldY length -> paper |> Set.map (foldY length)

let (paper, instructions) = parse input

let part1 =
    fold paper (instructions.[0]) |> Seq.length
