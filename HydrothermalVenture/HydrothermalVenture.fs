module HydrothermalVenture

open System

let input =
    IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

type Point = int * int
type Line = { z1: Point; z2: Point }

let parseLine(text: string) : Line =    
    let parse (point: string) : Point =
        let [| x; y |] = point.Split(",")
        (int x, int y)

    let [| z1; z2 |] = text.Split(" -> ")
    { z1 = parse z1; z2 = parse z2 }

let coordinates { z1 = (x1, y1); z2 = (x2, y2) } = x1 = x2 || y1 = y2

let points ({ z1 = (x1, y1); z2 = (x2, y2) } as line) =
    let [ min_x; max_x ] = [ x1; x2 ] |> List.sort
    let [ min_y; max_y ] = [ y1; y2 ] |> List.sort

    if line |> coordinates then     
        [ for x in min_x .. max_x do
              for y in min_y .. max_y -> (x, y) ]
    else 
        let x = if x1 < x2 then 1 else -1
        let y = if y1 < y2 then 1 else -1
        [for z in 0..max_x - min_x -> (x1 + x * z), y1 + (y*z)]

let solve input =
    let lines = input |> Array.map parseLine
    let cardinal_lines = lines |> Array.filter coordinates
    
    let allPoints = 
        cardinal_lines
        |> Seq.collect points
        |> Seq.countBy id

    let overlaps =
        allPoints |> Seq.filter (fun (_, nb) -> nb > 1)

    overlaps |> Seq.length

let solve2 input =
    let lines = input |> Array.map parseLine
    
    let allPoints = 
        lines
        |> Seq.collect points
        |> Seq.countBy id

    let overlaps =
        allPoints |> Seq.filter (fun (_, nb) -> nb > 1)

    overlaps |> Seq.length

solve input
solve2 input