module TrenchMap

type Algorithm = char []
type Image = Map<int * int, char>

type State =
    { algorithm: Algorithm
      image: Image
      infinitePixelsState: char }

let shrink (s: State) : State =
    { s with
          image =
              s.image
              |> Map.filter (fun k v -> v <> s.infinitePixelsState) }

let parse (text: string []) : State =
    let algorithm = text.[0] |> Seq.toArray

    let image =
        [ for r, row in (text |> Array.skip 2 |> Array.indexed) do
              for c, pixel in row |> Seq.indexed -> ((r, c), pixel) ]
        |> Map.ofSeq

    { algorithm = algorithm
      image = image
      infinitePixelsState = '.' }
    |> shrink

let neighbourLocations (r, c) =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 0)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]
    |> List.map (fun (dr, dc) -> (r + dr, c + dc))

let pixelAt defaultInfinite (image: Image) location =
    image
    |> Map.tryFind location
    |> Option.defaultValue defaultInfinite

let toBinary (chars: char list) =
    chars
    |> List.map
        (function
        | '.' -> "0"
        | '#' -> "1"
        | u -> failwithf "Unknown pixel value: <%c>" u)
    |> String.concat ""

let toDecimal (binary: string) = System.Convert.ToInt32(binary, 2)
let apply (algorithm: Algorithm) binary = algorithm.[binary]
let applyUpdate image (loc, pixel) = image |> Map.add loc pixel

let enhancePixelAt state location =
    location
    |> neighbourLocations
    |> List.map (pixelAt state.infinitePixelsState state.image)
    |> (toBinary >> toDecimal >> (apply state.algorithm))

let nextInfinite (state: State) =
    if state.algorithm.[0] = '.' then
        '.'
    else
        match state.infinitePixelsState with
        | '.' -> '#'
        | _ -> '.'

let enhance state =
    let interestingLocations =
        state.image
        |> Map.keys
        |> Seq.toList
        |> Seq.collect neighbourLocations
        |> Seq.distinct

    let nextImage =
        interestingLocations
        |> Seq.map (fun loc -> loc, enhancePixelAt state loc)
        |> Seq.fold applyUpdate state.image

    { state with
          image = nextImage
          infinitePixelsState = nextInfinite state }
    |> shrink

let print (state: State) : string =
    let image = state.image

    let minRow =
        image |> Map.keys |> Seq.map fst |> Seq.min

    let maxRow =
        image |> Map.keys |> Seq.map fst |> Seq.max

    let minCol =
        image |> Map.keys |> Seq.map snd |> Seq.min

    let maxCol =
        image |> Map.keys |> Seq.map snd |> Seq.max

    [ for row in minRow .. maxRow do
          [ for col in minCol .. maxCol ->
                pixelAt state.infinitePixelsState image (row, col)
                |> string ]
          |> String.concat "" ]
    |> String.concat "\n"

let rec repeatEnhance n state =
    if n = 0 then
        state
    else
        repeatEnhance (n - 1) (enhance state)

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let state = parse input
let enhancedOnce = state |> enhance
let enhancedTwice = enhancedOnce |> enhance

let part1 =
    enhancedTwice.image
    |> Map.filter (fun k v -> v = '#')
    |> Map.count

let enhanced = state |> repeatEnhance 50

let part2 =
    enhanced.image
    |> Map.filter (fun k v -> v = '#')
    |> Map.count
