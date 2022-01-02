module Day3

open System
open System.IO

let testInputPath = Path.Join(Directory.GetCurrentDirectory(), "Inputs", "Day3-test.txt")
let inputPath = Path.Join(Directory.GetCurrentDirectory(), "Inputs", "Day3.txt")

let first path =
    let countOnes sCount s =
        if s.Equals('1') then sCount + 1M else sCount
    let countSigmaBits (state: decimal * decimal[]) (next: string) =
        let (count, sigma) = state
        let bits = next.ToCharArray()
        let nextSigma = Array.map2 countOnes (if sigma.Length > 0 then sigma else Array.create bits.Length 0M) bits
        count + 1M, nextSigma
    let createDiagnostics (result: decimal * decimal[]) =
        let (count, sigmaCount) = result
        let sigmaMapper c s =
            let half = ceil(c / 2M)
            if s >= half then "1" else "0"
        let sigmaDigits = Array.map (sigmaMapper count) sigmaCount
        let sigmaStr = String.concat "" sigmaDigits
        let sigma2epsilon s =
            if s.Equals("1") then "0" else "1"
        let epsilonDigits = Array.map sigma2epsilon sigmaDigits
        let epsilonStr = String.concat "" epsilonDigits
        Convert.ToInt32(sigmaStr, 2), Convert.ToInt32(epsilonStr, 2)
    let calculateResult (s, e) =
        s * e

    File.ReadLines(path)
    |> Seq.fold countSigmaBits (0M, Array.empty)
    |> createDiagnostics
    |> calculateResult
    |> printfn "%i"

let second path =
    let folder position (state: int * int * string list * string list) (next: string) =
        let (zeroCount, oneCount, zeroNums, oneNums) = state
        match next[position] with
        | '0' -> zeroCount + 1, oneCount, next :: zeroNums, oneNums
        | '1' -> zeroCount, oneCount + 1, zeroNums, next :: oneNums
        | _ -> zeroCount, oneCount, zeroNums, oneNums
    let rec filterBase (selector: int * int * string list * string list -> string list) (position: int) (numbers: string seq) =
        let result = Seq.fold (folder position) (0, 0, [], []) numbers
        let boundNext = filterBase selector (position + 1)
        let nextNums = selector result
        if nextNums.Length = 1 then Convert.ToInt32((List.head nextNums), 2) else boundNext nextNums

    let filterO2 = filterBase (fun (zc, oc, zn, on) -> if oc >= zc then on else zn) 0
    let filterCO2 = filterBase (fun (zc, oc, zn, on) -> if zc <= oc then zn else on) 0

    let combine f1 f2 nums =
        (f1 nums, f2 nums)

    let calculateResult (s, e) =
        s * e
    
    File.ReadLines(path)
    |> combine filterO2 filterCO2
    |> calculateResult
    |> printfn "%i"

let testFirst () =
    first testInputPath
    
let solveFirst () =
    first inputPath

let testSecond () =
    second testInputPath

let solveSecond () =
    second inputPath