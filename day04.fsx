open System ; open type StringSplitOptions
let input = IO.File.ReadAllLines "inputs/day04.txt"

let cards = 
    input |> Array.map (
        _.Split([|" "|], RemoveEmptyEntries ||| TrimEntries)
        >> Array.skip 2
        >> Array.splitAt 10
        >> fun (x,y) -> Set x |> Set.intersect (Set y)
        >> Set.count)

let rec solver acc =
    function
    | [] -> acc
    | (c, h)::t -> solver (acc + c) ((t.[..h-1] |> List.map (fun (n,x) -> n+c,x)) @ t.[h..])

cards |> Array.map (function 0 -> 0 | 1 -> 1 | n -> 1 <<< (n - 1)) |> Array.sum |> printfn "Part 1: %i"
solver 0 (cards |> Array.map (fun c -> 1,c) |> Array.toList) |> printfn "Part 2: %i"