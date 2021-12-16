module Chiton

open System.IO

let fileName = File.ReadAllLines "input.txt" |> Seq.cast<string> |> Seq.map (fun l -> l.Trim())

let riskLevelArray =
    fileName |> Seq.map (
        fun l -> l.ToCharArray() |> Seq.map (fun c -> int c - int '0') )
    |> array2D

type Path = int * List<int * int>

let endOf (riskLevels : int[,]) = (riskLevels.GetLength(0)-1, riskLevels.GetLength(1)-1)

let isAtEnd (riskLevels : int[,]) (path : Path) =
    let (_, cs) = path
    let lc = Seq.head cs
    lc = endOf riskLevels

let generatePaths (riskLevels : int[,]) (path : Path) =
    let (s, cs) = path
    let lc = List.head cs
    let (lc0, lc1) = lc
    let ncs =
        seq {
           (lc0-1,lc1)
           (lc0+1,lc1)
           (lc0,lc1-1)
           (lc0,lc1+1) }
        |> Seq.filter (
            fun (c0, c1) ->
                c0 >= 0 && c0 < riskLevels.GetLength(0) &&
                c1 >= 0 && c1 < riskLevels.GetLength(1) )
    ncs
        |> Seq.map (fun nc ->
            Path(s + riskLevels[fst nc, snd nc], nc :: cs) )
        |> List.ofSeq

let keepBestPaths (riskLevels : int[,]) (bestScores : Map<(int * int),int>) (paths : List<Path>) =
    paths
        |> List.groupBy (fun (_, cs) -> cs |> List.head)
        |> List.map (fun (_, ps) -> ps |> List.sortBy fst |> List.head)
        |> List.filter (fun (s, cs) ->
            let last = cs |> List.head
            let maybeBest = Map.tryFind last bestScores
            match maybeBest with
            | Some best -> s < best
            | None -> true )

let updateBestScores (bestScores : Map<(int * int),int>) (keptPaths : List<Path>) =
    let betterScores =
        keptPaths
            |> Seq.map (fun (s, cs) -> (List.head cs, s))
            |> Map.ofSeq
    Map.fold (fun acc key value -> Map.add key value acc) bestScores betterScores

let rec search (riskLevels : int[,]) (currentPaths : List<Path>) (bestScores : Map<(int * int),int>) =
    if List.isEmpty currentPaths then
        match Map.tryFind (endOf riskLevels) bestScores with
        | Some score -> score
        | None -> failwith "ran out of paths before finding end"
    else
        let currentPaths' =
            currentPaths
                |> List.collect (generatePaths riskLevels)
                |> keepBestPaths riskLevels bestScores
        let bestScores' = updateBestScores bestScores currentPaths'
        search riskLevels currentPaths' bestScores'

let partOne =
    let currentPaths = [ Path(0, [(0,0)]) ]
    let bestScores = seq { ((0,0), 0) } |> Map.ofSeq
    search riskLevelArray currentPaths bestScores

printfn $"""partOne=%A{partOne}"""

let giantRiskLevelArray =
    let l0 = riskLevelArray.GetLength(0)
    let l1 = riskLevelArray.GetLength(1)
    Array2D.init (l0 * 5) (l1 * 5) (
        fun i j ->
            let ri = i % l0
            let rj = j % l1
            let di = i / l0
            let dj = j / l1
            let original = riskLevelArray[ri, rj]
            1 + (original + di + dj - 1) % 9 )

let partTwo =
    let currentPaths = [ Path(0, [(0,0)]) ]
    let bestScores = seq { ((0,0), 0) } |> Map.ofSeq
    search giantRiskLevelArray currentPaths bestScores

printfn $"""partTwo=%A{partTwo}"""
