open System

let sequences = [ for s in IO.File.ReadAllLines "inputs/day09.txt" -> s |> _.Split(' ') |> Seq.map int |> Seq.toList ]

let rec solve (sequences : int list list) =
    match sequences |> List.last |> List.pairwise |> List.map (fun (a, b) -> b - a) with
    | s when s |> List.forall ((=) 0) -> sequences @ [s] |> List.map List.last |> List.sum
    | s -> solve (sequences @ [s])

printfn "Part 1: %i" (sequences |> List.sumBy (fun sequence -> solve [sequence]))
printfn "Part 2: %i" (sequences |> List.sumBy (fun sequence -> solve [List.rev sequence]))
