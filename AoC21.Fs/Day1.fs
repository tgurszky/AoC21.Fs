module Day1

open FParsec

let testInput = [199;200;208;210;200;207;240;269;260;263]

let measurementsParser = many (pint32 .>> spaces)

let run runner =
    Input.parseFromFile measurementsParser "Day1.txt"
    |> Option.map runner
    |> ignore

let countIncrements (measurements: int list) =
    let folder state m2 =
        match state with
        | (count, -1) -> (count, m2)
        | (count, m1) -> if m2 > m1 then (count + 1, m2) else (count, m2)
    List.fold folder (0, -1) measurements
    |> fst

let first input =
    input
    |> countIncrements
    |> printfn "First solution is: %i"

let testFirst () =
    first testInput

let runFirst () =
    run first

let solveSecond (measurements: int list) =
    List.windowed 3 measurements
    |> List.map List.sum
    |> countIncrements

let second input =
    input
    |> solveSecond
    |> printfn "Second solution is: %i"

let testSecond () =
    second testInput

let runSecond () =
    run second