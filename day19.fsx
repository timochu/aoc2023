open System ; open type System.StringSplitOptions

type Verdict = Accept | Reject

type Workflow = 
    | Comparison of char * (IComparable -> IComparable -> bool) * int * Workflow
    | Destination of string
    | Verdict of Verdict

let rec toWorkflow = function
    | "A" -> Verdict Accept
    | "R" -> Verdict Reject
    | step when step[1] = '>' -> step.Split ':' |> fun x -> Comparison (step[0], (>), int (x[0][2..]), toWorkflow x[1])
    | step when step[1] = '<' -> step.Split ':' |> fun x -> Comparison (step[0], (<), int (x[0][2..]), toWorkflow x[1])
    | dst -> Destination dst

let workflows, items = IO.File.ReadAllText "inputs/day19.txt" |> _.Split("\r\n\r\n") |> Array.map _.Split("\r\n") |> fun x -> 
    x.[0] |> Array.map (_.Split("{},".ToCharArray(), RemoveEmptyEntries) >> fun x -> x[0], [ for step in x[1..] -> toWorkflow step ]) |> Map,
    x.[1] |> Array.map (_.Split("{}=,".ToCharArray(), RemoveEmptyEntries) >> Array.chunkBySize 2 >> Array.map (fun x -> x[0][0], int x[1]) >> Map)

let rec solve workflow (item : Map<char,int>) = 
    match workflow with
    | Comparison (c, (><), value, Verdict v) :: _ when item[c] >< value       -> v
    | Comparison (c, (><), value, Destination dst) :: _ when item[c] >< value -> solve (workflows[dst]) item
    | Comparison _ :: remaining                                               -> solve remaining item
    | Destination next :: _                                                   -> solve (workflows[next]) item
    | Verdict v :: _                                                          -> v

items |> Array.sumBy (fun i ->
    match solve workflows["in"] i with
    | Accept -> Seq.sum i.Values
    | Reject -> 0) |> printfn "Part 1: %i"