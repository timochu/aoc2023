open System ; open type System.StringSplitOptions

type Verdict = Accept | Reject

type Workflow = 
    | Comparison of char * (IComparable -> IComparable -> bool) * int * Workflow
    | Destination of string
    | Verdict of Verdict

let input = IO.File.ReadAllText "inputs/day19.txt" |> _.Split("\r\n\r\n") |> Array.map _.Split("\r\n")

let rec toWorkflow = function
    | "A" -> Verdict Accept
    | "R" -> Verdict Reject
    | step when step[1] = '>' -> step.Split ':' |> fun x -> Comparison (step[0], (>), int (x[0][2..]), toWorkflow x[1])
    | step when step[1] = '<' -> step.Split ':' |> fun x -> Comparison (step[0], (<), int (x[0][2..]), toWorkflow x[1])
    | dst -> Destination dst

let workflows = 
    input.[0]
    |> Array.map (_.Split([|'{';'}';','|], RemoveEmptyEntries) >> fun (line) -> line[0], [ for step in line[1..] -> toWorkflow step ])
    |> Map

let items = input.[1] |> Array.map (
    _.Split([|'{';'}';'=';','|], RemoveEmptyEntries) 
    >> Array.chunkBySize 2
    >> Array.map (fun line -> line[0][0], int line[1])
    >> Map)

let rec solve workflow (item : Map<char,int>) = 
    match workflow with
    | Comparison (c, comparator, value, Verdict v) :: _ when comparator item[c] value       -> v
    | Comparison (c, comparator, value, Destination dst) :: _ when comparator item[c] value -> solve (workflows[dst]) item
    | Comparison _ :: remaining                                                             -> solve remaining item
    | Destination next :: _                                                                 -> solve (workflows[next]) item
    | Verdict v :: _                                                                        -> v

items |> Array.sumBy (fun i ->
    match solve workflows["in"] i with
    | Accept -> Seq.sum i.Values
    | Reject -> 0) |> printfn "Part 1: %i"