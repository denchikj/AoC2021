module SyntaxScoring

open System

let input = IO.File.ReadAllLines "input.txt"

let print s = List.map string s |> String.concat ""

type SyntaxEval =
    | Incomplete of string
    | Unexpected of char
    | Valid

let findError (s: string) =
    let rec loop acc str =
        match acc, str with
        | [], [] -> Valid
        | acc, [] -> Incomplete(print acc)
        | _, '[' :: stail -> loop (']' :: acc) stail
        | _, '(' :: stail -> loop (')' :: acc) stail
        | _, '{' :: stail -> loop ('}' :: acc) stail
        | _, '<' :: stail -> loop ('>' :: acc) stail
        | a :: atail, s :: stail when a = s -> loop atail stail
        | _ :: _, s :: _ -> Unexpected s
        | _ -> failwith "fail"

    loop [] (Seq.toList s)

let part1 =
    function
    | Unexpected ')' -> 3
    | Unexpected ']' -> 57
    | Unexpected '}' -> 1197
    | Unexpected '>' -> 25137
    | _ -> 0

let tryGetIncomplete =
    function
    | Incomplete s -> Some s
    | _ -> None

let solve =
    input |> Seq.map findError |> Seq.sumBy part1

let part2 =
    let scoreChar =
        function
        | ')' -> 1L
        | ']' -> 2L
        | '}' -> 3L
        | '>' -> 4L
        | _ -> failwith "fail"

    Seq.fold (fun acc c -> acc * 5L + scoreChar c) 0L

let middle x =
    let l = Seq.length x
    let i = l / 2
    Seq.item i x

let solve2 =
    input
    |> Seq.map findError
    |> Seq.choose tryGetIncomplete
    |> Seq.map part2
    |> Seq.sort
    |> middle
