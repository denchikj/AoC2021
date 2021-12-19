﻿module BeaconScanner

open System.IO

let file =
    File.ReadAllLines "input.txt" |> Array.toList

type Pos = int * int * int
type Scan = List<Pos>

type Scanner(id: int, scan: Scan) =
    member this.Id = id
    member this.Scan = scan
    override this.ToString() = $"Scanner({id} {scan})"

type InputType =
    | TScanner of int
    | TBeacon of Pos

let rec tokenize (input: list<string>) =
    match input with
    | [] -> []
    | "" :: scanner :: rest ->
        let scanner = (scanner.Split ' ').[2] |> int
        TScanner(scanner) :: tokenize rest
    | beacon :: rest ->
        let xyz = beacon.Split ',' |> Array.map int
        let pos = Pos(xyz.[0], xyz.[1], xyz.[2])
        (TBeacon pos) :: tokenize rest

let fileTokens = tokenize ("" :: file)
printfn $"fileTokens = {fileTokens}"

type PToken =
    | PBeacons of list<Pos>
    | PScanner of Scanner

let rec parseTokens (input: List<InputType>) =
    match input with
    | [] -> []
    | t :: rest ->
        match t, parseTokens rest with
        | TBeacon pos, [] -> [ PBeacons([ pos ]) ]
        | TBeacon pos, PBeacons (poss) :: rest -> PBeacons(pos :: poss) :: rest
        | TBeacon pos, rest -> PBeacons([ pos ]) :: rest
        | TScanner id, PBeacons (poss) :: rest -> PScanner(Scanner(id, poss)) :: rest
        | t, rest -> failwith $"Unknown input :: {t} {rest}"

let parse (input: list<string>) =
    let ptokens = tokenize ("" :: input) |> parseTokens

    ptokens
    |> List.map
        (fun token ->
            match token with
            | PScanner scanner -> scanner
            | beacon -> failwith $"beacon after parse {beacon}")

let inputScanners = parse file
printfn $"parsed: {inputScanners}"

let allRotations (x, y, z) : list<Pos> =
    let xyrots (x, y, z) =
        [ (x, y, z)
          (-y, x, z)
          (-x, -y, z)
          (y, -x, z) ]

    let yzrots (x, y, z) =
        [ (x, y, z)
          (x, -z, y)
          (x, -y, -z)
          (x, z, -y) ]

    let xzrots (x, y, z) =
        [ (x, y, z)
          (-z, y, x)
          (-x, y, -z)
          (z, y, -x) ]

    (x, y, z)
    |> xyrots
    |> List.map yzrots
    |> List.concat
    |> List.map xzrots
    |> List.concat

let allRot1 = allRotations (1, 2, 3)
let dRot1 = allRot1 |> List.sort |> List.distinct

let rotations ((x, y, z): Pos) =
    [ (-z, -y, -x)
      (-z, -x, y)
      (-z, x, -y)
      (-z, y, x)
      (-y, -z, x)
      (-y, -x, -z)
      (-y, x, z)
      (-y, z, -x)
      (-x, -z, -y)
      (-x, -y, z)
      (-x, y, -z)
      (-x, z, y)
      (x, -z, y)
      (x, -y, -z)
      (x, y, z)
      (x, z, -y)
      (y, -z, -x)
      (y, -x, z)
      (y, x, -z)
      (y, z, x)
      (z, -y, x)
      (z, -x, -y)
      (z, x, y)
      (z, y, -x) ]

type PotentialScanners(scanners: List<Scanner>) =
    member this.Scanners = scanners
    override this.ToString() = $"PotentialScanners({scanners})"

let toPotential (scanner: Scanner) : PotentialScanners =
    let posRots = scanner.Scan |> List.map rotations

    let rec toScans (posRots: list<list<Pos>>) : list<Scanner> =
        match posRots with
        | empties when empties.Head.IsEmpty -> []
        | lists ->
            let first =
                lists
                |> List.map List.head
                |> (fun beacons -> Scanner(scanner.Id, beacons))

            let rest = lists |> List.map List.tail |> toScans
            first :: rest

    PotentialScanners(toScans posRots)

let tScan1 = Scanner(1, [ (1, 2, 3); (2, 4, 6) ])
let pScan1 = toPotential tScan1

printfn $"pScan1 = {pScan1}"

let tryMatchSingle (scanner: Scanner) (other: Scanner) : list<Pos> =
    let offsets =
        Seq.allPairs (scanner.Scan |> List.toSeq) (other.Scan |> List.toSeq)
        |> Seq.map (fun ((x1, y1, z1), (x2, y2, z2)) -> (x2 - x1, y2 - y1, z2 - z1))
        |> Seq.groupBy id
        |> Seq.map (fun (pos, posr) -> (pos, posr |> Seq.length))
        |> Seq.filter (fun (offset, count) -> count > 11)
        |> Seq.map fst
        |> Seq.toList

    offsets

let tryMatchWithPots (scanner: Scanner) (pots: PotentialScanners) : list<(Scanner * Pos)> =
    pots.Scanners
    |> List.map
        (fun other ->
            let offsets: list<Pos> = tryMatchSingle scanner other
            offsets |> List.map (fun offset -> other, offset))
    |> List.concat

let merge (scanner: Scanner) (potentials: PotentialScanners) = 1

let mergeSingle (scanner1: Scanner) (scanner2: Scanner) ((dx, dy, dz): Pos) =
    let addOffSet (x, y, z) = (x - dx, y - dy, z - dz)
    let beacons1 = scanner1.Scan
    let beacons2 = scanner2.Scan |> List.map addOffSet

    let beacons =
        List.concat [ beacons1; beacons2 ]
        |> List.distinct
        |> List.sort

    Scanner(scanner1.Id, beacons)

type Candidate(scanner: Scanner, potentials: PotentialScanners) =
    member this.Scanner = scanner
    member this.Potentials = potentials

let toCandidate (scanner: Scanner) = Candidate(scanner, toPotential scanner)
let fileCandidates = inputScanners |> List.map toCandidate

let rec mergeWithList (first: Candidate) (candidates: list<Candidate>) : Candidate * List<Candidate> =
    match candidates with
    | [] -> first, []
    | candidate :: rest ->
        let matches: List<Scanner * Pos> =
            tryMatchWithPots first.Scanner candidate.Potentials

        if matches.Length > 1 then
            failwith "Not supported: multiple hits"

        if matches.IsEmpty then
            let first, rest = mergeWithList first rest
            first, candidate :: rest
        else
            let (hit, offset) = matches.Head
            let first = mergeSingle first.Scanner hit offset
            let first = toCandidate first
            mergeWithList first rest

let prodMergeRes =
    mergeWithList fileCandidates.Head fileCandidates.Tail

printfn $"prodMergeRes = {prodMergeRes} {(snd prodMergeRes |> List.length)} {(fileCandidates.Tail |> List.length)}"

printfn
    $"size: {(fst prodMergeRes).Scanner.Scan
             |> List.distinct
             |> List.length}"

let rec mergeList (candidates: list<Candidate>) =
    match candidates with
    | [] -> []
    | first :: rest ->
        let rest = mergeList rest
        let first, rest = mergeWithList first rest
        first :: rest

let prodM = mergeList fileCandidates
printfn $"prodM {prodM.Head.Scanner.Scan |> List.length}"

let pDist (x: int) (y: int) =
    if x > 0 && y > 0 then
        (max x y) - (min x y)
    else if x < 0 && y < 0 then
        let x = abs x
        let y = abs y
        (max x y) - (min x y)
    else
        (abs x) + (abs y)


let distance ((x1, y1, z1): Pos) ((x2, y2, z2): Pos) =
    let dist =
        (pDist x1 x2) + (pDist y1 y2) + (pDist z1 z2)

    if (dist = 7831) then
        printfn $"DIST {(x1, y1, z1)} {(x2, y2, z2)} = {dist}"

    dist

let maxDistance (positions: List<Pos>) =
    let toDistance (p1, p2) = distance p1 p2

    Seq.allPairs (positions |> List.toSeq) (positions |> List.toSeq)
    |> Seq.map toDistance
    |> Seq.max

printfn "Beacons: "
let prodScan0 = prodM.Head.Scanner

let prodPots =
    fileCandidates |> List.map (fun c -> c.Potentials)

let scansAbs (start: Scanner) (pots) =
    pots
    |> List.map (fun pot -> tryMatchWithPots start pot)
    |> List.map List.head
    |> List.map snd

let prodScansAbs = scansAbs prodScan0 prodPots

let prodMaxDist = maxDistance prodScansAbs
printfn $"prodMax = {prodMaxDist}"
