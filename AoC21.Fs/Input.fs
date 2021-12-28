module Input

open FParsec
open System.IO

let parseFromFile parser fileName =
    let inputPath = Path.Combine(Directory.GetCurrentDirectory(), "Inputs", fileName)
    match runParserOnFile parser () inputPath System.Text.Encoding.UTF8 with
    | Success(result, _, _) -> Some result
    | Failure(errorMessage, _, _) -> printfn "%s" errorMessage; None
