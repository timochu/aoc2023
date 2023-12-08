open System

let input = IO.File.ReadAllLines "inputs/day08.txt"
let directions = input |> Array.head |> _.ToCharArray() |> Array.toList

let map =
    input
    |> Array.skip 2
    |> Array.map (fun x ->
        x.Split([| " = ("; ", "; ")" |], StringSplitOptions.RemoveEmptyEntries)
        |> (fun y -> (y[0], (y[1], y[2]))))
    |> Map


let starts =
    input
    |> Array.skip 2
    |> Array.map (fun x ->
        x.Split([| " = ("; ", "; ")" |], StringSplitOptions.RemoveEmptyEntries)
        |> (fun y -> (y[0], (y[1], y[2]))))
    |> Array.where (fun (x, _) -> x.EndsWith('A'))
    |> Array.map fst



let rec solve (steps: int) (dirs: char list) (currentNode: string) =
    if currentNode = "ZZZ" then
        steps
    else
        match (map |> Map.find currentNode), dirs with
        | (_, r), 'R' :: t -> solve (steps + 1) t r
        | (l, _), 'L' :: t -> solve (steps + 1) t l
        | _ -> solve (steps) directions currentNode

let rec solve2 (steps: int) (dirs: char list) (currentNodes: string array) =
    if currentNodes |> Array.forall _.EndsWith('Z') then
        steps
    else
        match currentNodes |> Array.map (fun x -> Map.find x map), dirs with
        | x, 'R' :: t -> solve2 (steps + 1) t (x |> Array.map snd)
        | x, 'L' :: t -> solve2 (steps + 1) t (x |> Array.map fst)
        | _ -> solve2 (steps) directions currentNodes

solve 0 directions "AAA" |> printfn "Part 1: %A"
// solve2 0 directions starts |> printfn "Part 2: %A" // Part 2 too slow
