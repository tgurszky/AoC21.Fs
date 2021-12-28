module Day1

open FParsec
open System.IO

let testInput = [199;200;208;210;200;207;240;269;260;263]

let measurementsParser = many (pint32 .>> spaces)

let parseMeasurementsFromFile parser inputPath =
    match runParserOnFile parser () inputPath System.Text.Encoding.UTF8 with
    | Success(result, _, _) -> result
    | Failure(errorMessage, _, _) -> printfn "Something went wrong during parsing: %s" errorMessage
                                     []

let parseInput () =
    let inputPath = Path.Combine(Directory.GetCurrentDirectory(), "Inputs", "Day1.txt")
    parseMeasurementsFromFile measurementsParser inputPath

let countIncrements state m2 =
    match state with
    | (count, -1) -> (count, m2)
    | (count, m1) -> if m2 > m1 then (count + 1, m2) else (count, m2)

let solveFirst (measurements: int list) =
    List.fold countIncrements (0, -1) measurements

let first input =
    input
    |> solveFirst
    |> fst
    |> printfn "First solution is: %i"

let testFirst () =
    testInput
    |> first

let runFirst () =
    parseInput ()
    |> first

let solveSecond (measurements: int list) =
    List.windowed 3 measurements
    |> List.map List.sum
    |> solveFirst

let second input =
    input
    |> solveSecond
    |> fst
    |> printfn "Second solution is: %i"

let testSecond () =
    testInput
    |> second

let runSecond () =
    parseInput ()
    |> second