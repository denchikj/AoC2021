﻿module GiantSquid

open System
open System.Text.RegularExpressions

let input =
    IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let numbers = input.[0].Split ',' |> Seq.map int |> Seq.toArray

let allBoards = input.[2..]
                |> Array.toSeq
                |> Seq.map (fun s -> Regex.Replace (s,"^ ",""))
                |> Seq.map (fun s -> Regex.Split (s," +") |> Seq.toArray )
                |> Seq.filter (fun a -> a.Length > 1)
                |> Seq.map (fun nums -> nums |> Array.map int)
                |> Seq.toArray 
printfn $"Numbers {numbers |> Array.toList}"

let numberOfBoards = allBoards.Length / 5

printfn $"Board# {numberOfBoards}"

let getBoard (n:int) = allBoards.[n*5 .. n*5+4]

printfn $"{(getBoard 0).[0].[0]} {(getBoard 2).[4].[0]}"

let boards = {0..99} |> Seq.map getBoard |> Seq.toArray 

printfn $"{(boards.[0]).[0].[0]} {(boards.[2]).[4].[0]}"

let rows board = (board:int[][]) |> Array.toList
let cols (board:int[][]) =
    let column col =
        {0..4} |> Seq.map (fun row -> board.[row].[col])
    {0..4} |> Seq.map column |> Seq.map Seq.toArray |> Seq.toList 

let isWinner (draws:Set<int>) (board:int[][])  =
    let lines:List<int[]> = List.append (cols board) (rows board)
    let isMatch (line:int[]) = line |> Array.filter (fun c -> not (draws.Contains c)) |> Array.isEmpty
    let matchingLines = lines |> List.filter isMatch |> List.length
    matchingLines > 0

let getWinners (draw:Set<int>) =
    boards |> Seq.filter (isWinner draw) |> Seq.toList 

let getLosers (draw:Set<int>) =
    boards |> Seq.filter (fun board -> not(isWinner draw board)) |> Seq.toList 


let draws = {0.. numbers.Length-1}
            |> Seq.map (fun l -> numbers.[0..l] |> Set)
            |> Seq.toList

let score (board:int[][]) (draw:Set<int>) =
    let value (num:int) = if draw.Contains num then 0 else num 
    let rowScore (row:int[]) = row |> Array.map value |> Array.sum
    board |> Array.map rowScore |> Array.sum
    
let rec findWinner (draws: List<Set<int>>) (nums:List<int>) =
    let draw = draws.Head
    let lastNum = nums.Head
    let winners = getWinners draw 
    if winners.Length > 0 then
        let winner = winners.Head
        let boardScore = score winner draw
        let final = lastNum * boardScore
        printfn $"winner: {winners.Length} {winner} {final}"
    else
        findWinner draws.Tail nums.Tail 

let rec playOutLoser (board:int[][]) (draws:List<Set<int>>) (nums:List<int>) =
    let draw = draws.Head
    let lastNum = nums.Head 
    if isWinner draw board then
        let boardScore = score board draw
        let final = lastNum * boardScore
        printfn $"loser: {final}"
    else
        playOutLoser board draws.Tail nums.Tail 
        
let rec findLoser (draws: List<Set<int>>) (nums:List<int>) =
    let draw = draws.Head
    let lastNum = nums.Head
    let losers = getLosers draw 
    if losers.Length < 2 then
        let loser = losers.Head
        playOutLoser loser draws nums 
    else
        findLoser draws.Tail nums.Tail 

findWinner draws (numbers |> Array.toList)
findLoser draws (numbers |> Array.toList)