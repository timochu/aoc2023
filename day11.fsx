let input =
    System.IO.File.ReadAllLines "inputs/day11.txt"
    |> Array.map _.ToCharArray()
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x item -> x, y, item))
    |> Array.collect id

let empties f =
    input
    |> f
    |> Array.filter (fun (_, row) -> row |> Array.forall (fun (_, _, o) -> o = '.'))
    |> Array.map (fun (key, _) -> key)

let rows = Array.groupBy (fun (_, y, _) -> y) |> empties
let cols = Array.groupBy (fun (x, _, _) -> x) |> empties

let rec combinations n l =
    match n, l with
    | 0, _ -> [ [] ]
    | _, [] -> []
    | k, (x :: xs) -> List.map ((@) [ x ]) (combinations (k - 1) xs) @ combinations k xs

let distance emptyCost ((x1, y1, _), (x2, y2, _)) =
    [ [ for x in (min x1 x2) + 1 .. (max x1 x2) -> if cols |> Array.contains x then emptyCost else 1UL ]
      [ for y in (min y1 y2) + 1 .. (max y1 y2) -> if rows |> Array.contains y then emptyCost else 1UL ] ]
    |> List.collect id
    |> List.sum

let galaxies =
    input
    |> Array.filter (fun (_, _, c) -> c = '#')
    |> Array.toList
    |> combinations 2
    |> List.map (fun x -> x[0], x[1])

galaxies |> List.map (distance 2UL) |> List.sum |> printfn "Part 1: %A"

galaxies |> List.map (distance 1000000UL) |> List.sum |> printfn "Part 2: %A"
