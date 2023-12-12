let input =
    System.IO.File.ReadAllLines "inputs/day11.txt"
    |> Array.mapi (fun y row -> row.ToCharArray() |> Array.mapi (fun x item -> x, y, item))
    |> Array.collect id

let empties f = input |> f |> Array.filter (snd >> Array.forall (fun (_, _, o) -> o = '.')) |> Array.map fst |> Set
let rows = Array.groupBy (fun (_, y, _) -> y) |> empties
let cols = Array.groupBy (fun (x, _, _) -> x) |> empties

let combinations (p: 'a array) =
    [ for i in 0 .. p.Length-1 do
        for j in i+1 .. p.Length-1 do
            yield p[i], p[j] ]

let distance emptyCost ((x1, y1, _), (x2, y2, _)) =
    [ [ for x in (min x1 x2) + 1 .. (max x1 x2) -> if cols.Contains x then emptyCost else 1UL ]
      [ for y in (min y1 y2) + 1 .. (max y1 y2) -> if rows.Contains y then emptyCost else 1UL ] ]
    |> List.collect id
    |> List.sum

let galaxies = input |> Array.filter (fun (_, _, c) -> c = '#') |> combinations

galaxies |> List.map (distance 2UL) |> List.sum |> printfn "Part 1: %i"
galaxies |> List.map (distance 1000000UL) |> List.sum |> printfn "Part 2: %i"
