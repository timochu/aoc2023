open System

let sequences = [ for s in IO.File.ReadAllLines "inputs/day09.txt" -> s |> _.Split(' ') |> Seq.map int |> Seq.toList ]

let rec differences (sequences : int list list) =
    match sequences |> List.last |> List.pairwise |> List.map (fun (a, b) -> b - a) with
    | s when s |> List.forall ((=) 0) -> sequences @ [s]
    | s -> differences (sequences @ [s])

printfn "Part 1: %A" (sequences |> List.sumBy (fun x -> differences [x] |> List.map List.last |> List.sum))
printfn "Part 2: %A" (sequences |> List.sumBy (fun x -> differences [List.rev x] |> List.map List.last |> List.sum))
