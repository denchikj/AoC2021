module DiracDice

open System
open System.Collections.Generic

let input = IO.File.ReadAllLines "input.txt"

type Player = { Position: int; Score: int }

let parse (input: string array) =
    let p1 = input.[0].Split(": ").[1] |> int
    let p2 = input.[1].Split(": ").[1] |> int
    { Position = p1; Score = 0 }, { Position = p2; Score = 0 }

let deterministic =
    Seq.initInfinite (fun i -> (i % 100) + 1)

let move rollTotal player =
    let newPosition =
        ((player.Position - 1 + rollTotal) % 10) + 1

    { Position = newPosition
      Score = player.Score + newPosition }

let (p1, p2) = parse input

deterministic
|> Seq.chunkBySize 3
|> Seq.map Seq.sum
|> Seq.indexed
|> Seq.scan
    (fun (p1, p2, x) (i, rollTotal) ->
        if i % 2 = 0 then
            move rollTotal p1, p2, (i + 1) * 3
        else
            p1, move rollTotal p2, (i + 1) * 3)
    (p1, p2, 0)
|> Seq.find (fun (p1, p2, x) -> p1.Score >= 1000 || p2.Score >= 1000)
|> fun (p1, p2, x) -> ([ p1.Score; p2.Score ] |> List.min) * x

let memoize func =
    let cache = Dictionary<_, _>()

    fun key ->
        let exists, value = cache.TryGetValue key

        if exists then
            value
        else
            let value = func key
            cache.Add(key, value)
            value

let diracDiceRolls =
    [ [ 1; 1; 1 ]
      [ 1; 1; 2 ]
      [ 1; 1; 3 ]
      [ 1; 2; 1 ]
      [ 1; 2; 2 ]
      [ 1; 2; 3 ]
      [ 1; 3; 1 ]
      [ 1; 3; 2 ]
      [ 1; 3; 3 ]
      [ 2; 1; 1 ]
      [ 2; 1; 2 ]
      [ 2; 1; 3 ]
      [ 2; 2; 1 ]
      [ 2; 2; 2 ]
      [ 2; 2; 3 ]
      [ 2; 3; 1 ]
      [ 2; 3; 2 ]
      [ 2; 3; 3 ]
      [ 3; 1; 1 ]
      [ 3; 1; 2 ]
      [ 3; 1; 3 ]
      [ 3; 2; 1 ]
      [ 3; 2; 2 ]
      [ 3; 2; 3 ]
      [ 3; 3; 1 ]
      [ 3; 3; 2 ]
      [ 3; 3; 3 ] ]
    |> List.map List.sum
    |> List.countBy id

let rec play (player1, player2) =
    if player2.Score >= 21 then
        0L, 1L
    else
        diracDiceRolls
        |> Seq.fold
            (fun (wins1, wins2) (rollTotal, numberOfUniverses) ->
                let (w2, w1) = play (player2, move rollTotal player1)
                wins1 + (int64 numberOfUniverses) * w1, wins2 + (int64 numberOfUniverses) * w2)
            (0L, 0L)

let memoizedPlay = memoize play

memoizedPlay (p1, p2)
|> fun (win1, win2) -> [ win1; win2 ] |> List.max
