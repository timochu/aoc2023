open System

let sequences = [ for s in IO.File.ReadAllLines "inputs/day09.txt" -> s |> _.Split(' ') |> Seq.map int |> Seq.toList ]

let rec allDifferences (sequences : int list list) =
    match sequences |> List.last |> List.pairwise |> List.map (fun (a, b) -> b - a) with
    | s when s |> List.forall ((=) 0) -> sequences @ [s]
    | s -> allDifferences (sequences @ [s])

printfn "Part 1: %A" (sequences |> List.sumBy (fun x -> allDifferences [x] |> List.map List.last |> List.sum))
printfn "Part 2: %A" (sequences |> List.sumBy (fun x -> allDifferences [x |> List.rev] |> List.map List.last |> List.sum))
