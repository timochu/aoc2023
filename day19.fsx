open System

type Verdict = Accept | Reject
type ComparisonResult = Dst of string | Verdict of Verdict
type Workflow = 
    | Comparison of char * (IComparable -> IComparable -> bool) * int * (string)
    | Destination of string
    | Verdict of Verdict

let input = IO.File.ReadAllText "inputs/day19.txt" |> _.Split("\r\n\r\n") |> Array.map _.Split("\r\n")

let toWorkflow =
    function
    | "A" -> Verdict Accept
    | "R" -> Verdict Reject
    | s when s.Contains(':') -> 
        let tmp = s.Split(':')
        let comparator = if s[1] = '>' then (>) else (<)
        Comparison (s[0], comparator, int (tmp[0][2..]), tmp[1] )
    | s -> Destination s

let workflows = 
    input 
    |> Array.head 
    |> Array.map _.Split([|"{";"}";","|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> line.[0], line.[1..] |> Array.map toWorkflow |> Array.toList)
    |> Map

let items = 
    input[1] 
    |> Array.map (_.Split([|"{";"}";"=";","|], StringSplitOptions.RemoveEmptyEntries) 
        >> Array.chunkBySize 2 
        >> Array.map (fun x -> x[0][0], int x[1])
        >> Map)


let rec solve (workflow::remaining) item = 
    match workflow with
    | Comparison (c, comparator, value, next) when comparator (Map.find c item) value ->
        match next with
        | "A" -> Accept
        | "R" -> Reject
        | next          -> solve (workflows |> Map.find next) item
    | Comparison _      -> solve remaining item
    | Destination next  -> solve (workflows |> Map.find next) item
    | Verdict verdict   -> verdict

items |> Array.choose (fun i -> 
    match solve (workflows |> Map.find "in") i with
    | Accept -> Some (Seq.sum (i.Values))
    | Reject -> None)
    |> Array.sum
    |> printfn "Part 1: %i"
