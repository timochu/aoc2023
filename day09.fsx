let sequences = [ for s in System.IO.File.ReadAllLines "inputs/day09.txt" -> s |> _.Split(' ') |> Seq.map int |> Seq.toList ]

let rec solve (accumulator : int list list) =
    match accumulator |> List.last |> List.pairwise |> List.map (fun (a, b) -> b - a) with
    | s when s |> List.forall ((=) 0) -> accumulator @ [s] |> List.map List.last |> List.sum
    | s -> solve (accumulator @ [s])

sequences |> List.sumBy (fun sequence -> solve [sequence]) |> printfn "Part 1: %i"
sequences |> List.sumBy (fun sequence -> solve [List.rev sequence]) |> printfn "Part 2: %i"
