module Dive

open System

let input =
    IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"forward 5
down 5
forward 8
up 3
down 8
forward 2"
        .Split("\n")

type Command =
    | Forward of int
    | Down of int
    | Up of int

type State =
    { Horizontal: int
      Depth: int
      Aim: int }

let initialState = { Horizontal = 0; Depth = 0; Aim = 0 }

let parse (commandText: string) =
    let c = commandText.Split(" ")
    let distance = c[1] |> int

    match c[0] with
    | "forward" -> Forward distance
    | "down" -> Down distance
    | "up" -> Up distance
    | _ -> failwith $"unknown command {commandText}"

let applyCommand state command =
    match command with
    | Forward x ->
        { state with
              Horizontal = state.Horizontal + x
              Depth = state.Depth + (state.Aim * x) }
    | Down x -> { state with Aim = state.Aim + x }
    | Up x -> { state with Aim = state.Aim - x }

let exp =
    example
    |> Seq.map parse
    |> Seq.fold applyCommand initialState
    |> fun state -> state.Horizontal * state.Depth

let solve =
    input
    |> Seq.map parse
    |> Seq.fold applyCommand initialState
    |> fun state -> state.Horizontal * state.Depth
