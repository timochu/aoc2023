open System

type Verdict = Accept | Reject

type Workflow = 
    | Comparison of char * (IComparable -> IComparable -> bool) * int * Workflow
    | Destination of string
    | Verdict of Verdict

let input = IO.File.ReadAllText "inputs/day19.txt" |> _.Split("\r\n\r\n") |> Array.map _.Split("\r\n")

let rec toWorkflow = function
    | "A" -> Verdict Accept
    | "R" -> Verdict Reject
    | s when s.Contains(':') -> 
        let tmp = s.Split(':')
        Comparison (s[0], (if s[1] = '>' then (>) else (<)), int (tmp[0][2..]), tmp[1] |> toWorkflow)
    | s -> Destination s

let workflows = 
    input.[0]
    |> Array.map _.Split([|"{";"}";","|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun line -> line.[0], line.[1..] |> Array.map toWorkflow |> Array.toList)
    |> Map

let items = 
    input.[1]
    |> Array.map (
        _.Split([|"{";"}";"=";","|], StringSplitOptions.RemoveEmptyEntries) 
        >> Array.chunkBySize 2 
        >> Array.map (fun x -> x[0][0], int x[1])
        >> Map)

let solver =
    let rec solve workflow (item : Map<char,int>) = 
        match workflow with
        | Comparison (c, comparator, value, Verdict v) :: _ when comparator item.[c] value       -> v
        | Comparison (c, comparator, value, Destination dst) :: _ when comparator item.[c] value -> solve (workflows.[dst]) item
        | Comparison _ :: remaining                                                              -> solve remaining item
        | Destination next :: _                                                                  -> solve (workflows |> Map.find next) item
        | Verdict v :: _                                                                         -> v
    solve workflows.["in"]

items |> Array.sumBy (fun i -> 
    match solver i with
    | Accept -> Seq.sum (i.Values)
    | Reject -> 0)
    |> printfn "Part 1: %i"