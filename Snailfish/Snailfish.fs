module Snailfish

open System.IO

type Snailfish =
  | Pair of Snailfish * Snailfish
  | Number of int

let rec stringFish = function 
  | Pair (l,r) -> $"[{stringFish l},{stringFish r}]"
  | Number n -> string n

type ReductionResult =
  | Changed of Snailfish
  | Explode of int * int
  | ExplodeLeft of int * Snailfish
  | ExplodeRight of Snailfish * int
  | Done

let (.+) s1 s2 = Pair (s1, s2)

let rec reduce snailfish = 
  let rec addRight n = function
  | Number m -> Number(n + m)
  | Pair (l, r) -> addRight n l .+ r

  let rec addLeft n = function
  | Number m -> Number(n + m)
  | Pair (l, r) -> l .+ addLeft n r

  let rec explode n = function
  | Number _ -> Done
  | Pair (Number l, Number r) when n = 4 -> Explode (l, r)
  | Pair (l, r) -> 
    match explode (n+1) l with
    | Changed f -> Changed(f .+ r)
    | Explode (a, b) -> ExplodeLeft (a, Number 0 .+ addRight b r)
    | ExplodeLeft (n, f) -> ExplodeLeft (n, f .+ r)
    | ExplodeRight (f, n) -> Changed(f .+ addRight n r)
    | Done -> 
      match explode (n+1) r with
      | Changed f -> Changed (l .+ f)
      | Explode (a, b) -> ExplodeRight (addLeft a l .+ Number 0, b)
      | ExplodeLeft (n, f) -> Changed (addLeft n l .+ f)
      | ExplodeRight (f, n) -> ExplodeRight(l .+ f, n)
      | Done -> Done
  
  let rec split = function
  | Number n when n >= 10 -> Changed (Number (n/2) .+ Number (n-n/2))
  | Number _ -> Done
  | Pair (l,r) -> 
    match split l with 
    | Changed f -> Changed (f .+ r)
    | Done -> 
      match split r with
      | Changed f -> Changed (l .+ f)
      | Done -> Done
  
  let reduceOnce fsh =
    match explode 0 fsh with
    | Done -> split fsh
    | x -> x

  match reduceOnce snailfish with
  | Explode (_, _) -> failwith "premature explosion"
  | ExplodeLeft (_, f) | ExplodeRight (f, _) | Changed f -> reduce f
  | Done -> snailfish

let parse (snailStr: string) = 
  let rec parseOne (s: string): Snailfish * string = 
    match s[0] with
    | '[' -> 
      let (left, mid) = parseOne s[1..]
      let (right, rest) = parseOne mid[1..]
      (Pair (left, right), rest[1..])
    | c -> (Number (int c - int '0'), s[1..])
  parseOne snailStr |> fst

let add a b = reduce (a .+ b)

let rec magnitude = function
  | Pair (l, r) -> (3 * magnitude l) + (2 * magnitude r)
  | Number n -> n

let lines = File.ReadAllLines "input.txt" |> Seq.map parse

lines
|> Seq.reduce add
|> magnitude

lines
|> Seq.allPairs lines
|> Seq.filter ((<||) (<>))
|> Seq.map (((<||) add) >> magnitude)
|> Seq.max