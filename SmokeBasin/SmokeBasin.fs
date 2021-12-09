module SmokeBasin

open System

let input = IO.File.ReadAllLines "input.txt"
let charToL (c: char) = int64 c - int64 '0'

let getNeighbours map x z =
    seq [ (x + 1, z)
          (x - 1, z)
          (x, z + 1)
          (x, z - 1) ]
    |> Seq.map
        (function
        | x, z ->
            let row = Array.tryItem x map

            match row with
            | None -> None
            | Some n ->
                match Array.tryItem z n with
                | None -> None
                | Some _ -> Some(x, z))
    |> Seq.choose id

let getAllPits map =
    let lenX = Array.length map - 1
    let lenY = Array.length (Array.head map) - 1

    seq [ [ 0 .. lenX ]
          |> Seq.map
              (fun x ->
                  [ 0 .. lenY ]
                  |> Seq.map
                      (fun y ->
                          let depth = map.[x].[y]
                          let neighbours = getNeighbours map x y

                          match neighbours
                                |> Seq.forall (fun (x, y) -> map.[x].[y] > depth)
                              with
                          | true -> Some(x, y)
                          | false -> None)) ]
    |> Seq.concat
    |> Seq.concat
    |> Seq.choose id

let solve =
    let map =
        input
        |> Array.ofSeq
        |> Array.map (Seq.map charToL >> Array.ofSeq)

    getAllPits map
    |> Seq.map (fun (x, y) -> map.[x].[y] + 1L)
    |> Seq.sum

solve

let rec getBasin map basin (x, y) =
    let basins = Set.add (x, y) basin

    let neighbours =
        getNeighbours map x y
        |> Seq.filter
            (fun (n, m) ->
                map.[n].[m] < 9L
                && map.[x].[y] < map.[n].[m]
                && not (Set.contains (n, m) basins))
        |> Set.ofSeq

    let basin'' = Set.union basins neighbours

    match Set.isEmpty neighbours with
    | true -> basins
    | false ->
        neighbours
        |> Seq.map (getBasin map basin'')
        |> Set.unionMany

let solve2 =
    let map =
        input
        |> Array.ofSeq
        |> Array.map (Seq.map charToL >> Array.ofSeq)

    let pits = getAllPits map

    let basins =
        pits
        |> Seq.map (getBasin map Set.empty<int * int>)
        |> Seq.map Set.count
        |> Seq.sortBy (~-)

    printfn "%A" basins

    basins
    |> Seq.take 3
    |> Seq.map int64
    |> Seq.reduce (*)

solve2
