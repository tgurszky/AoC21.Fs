module Day2

open FParsec

let testInput = "forward 5
down 5
forward 8
up 3
down 8
forward 2"

type Submarine = int * int * int
type Instruction = 
    | Forward of int
    | Down of int
    | Up of int

let pInsBase str ins : Parser<Instruction, unit> = (skipString str .>> spaces) >>. pint32 |>> ins
let pForward = pInsBase "forward" Forward
let pDown =  pInsBase "down" Down
let pUp = pInsBase "up" Up
let pInstruction = pForward <|> pDown <|> pUp
let pCourse = many (pInstruction .>> spaces)

let runCourse (course: Instruction list) : Submarine =
    let calcNext (location: Submarine) (step: Instruction) : Submarine =
        let (x, y, a) = location
        match step with
        | Forward f -> x + f, y, a
        | Down d -> x, y + d, a
        | Up u -> x, y - u, a
    List.fold calcNext (0,0, 0) course

let runCourse2 (course: Instruction list) : Submarine =
    let calcNext (location: Submarine) (step: Instruction) : Submarine =
        let (x, y, a) = location
        match step with
        | Forward f -> x + f, y + (a * f), a
        | Down d -> x, y, a + d
        | Up u -> x, y, a - u
    List.fold calcNext (0,0, 0) course

let locationMultiplication (loc: Submarine) =
    let (x, y, a) = loc
    x * y

let printFinalLocationMultiplication runner course =
    runner course
    |> locationMultiplication
    |> printfn "%i"

let test part =
    match run pCourse testInput with
    | Success (c, _, _) -> part c
    | Failure (m, _, _) -> printfn "%s" m

let solve part =
    Input.parseFromFile pCourse "Day2.txt"
    |> Option.map part
    |> ignore

let first course =
    printFinalLocationMultiplication runCourse course

let testFirst () =
    test first

let solveFirst () =
    solve first

let second course =
    printFinalLocationMultiplication runCourse2 course

let testSecond () =
    test second
    
let solveSecond () =
    solve second