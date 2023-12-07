open System

let input = IO.File.ReadAllText "inputs/day05.txt" |> _.Split("\r\n\r\n")
let seeds = input |> Array.head |> _.Split(' ') |> Array.tail |> Array.map uint
let maps = input |> Array.tail |> Array.map (_.Split("\r\n") >> Array.skip 1 >> Array.map (_.Split(' ') >> Array.map uint >> (fun x -> x[0], x[1], x[2])) >> Array.toList)

let rec mapper seed =
    function
    | [] -> seed
    | (destination, source, range)::_ when seed >= source && seed < source + range -> destination + seed - source 
    | _::t -> mapper seed t

seeds |> Array.map(fun seed -> maps |> Array.fold mapper seed) |> Array.min |> (printfn "Part 1: %i")