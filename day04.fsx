open System; open type StringSplitOptions

let cards = 
    [for line in IO.File.ReadAllLines "inputs/day04.txt" ->
        1, line[10..].Split(' ', RemoveEmptyEntries) |> fun (nr) -> Set.intersect (Set nr[..9]) (Set nr[10..]) |> Set.count]

let rec solver total =
    function
    | [] -> total
    | (count, head)::tail -> solver (total + count) ([for (n, c) in tail.[..head-1] -> (n + count, c)] @ tail.[head..])

cards |> List.sumBy (fun (_, count) -> pown 2 (count - 1)) |> printfn "Part 1: %i"
cards |> solver 0 |> printfn "Part 2: %i"
