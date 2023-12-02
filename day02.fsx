open System
let toGame (line : string) =
    let items = line.Split([| ": "; ", "; "; " |], StringSplitOptions.RemoveEmptyEntries)
    let get (name : string) = items |> Array.filter (fun x -> x.Contains name) |> Array.map (Seq.filter Char.IsDigit >> String.Concat >> int)
    get "Game" |> Seq.head, get "red" |> Seq.max, get "green" |> Seq.max, get "blue" |> Seq.max

let games = "inputs/day02.txt" |> IO.File.ReadAllLines |> Array.map toGame
games |> Seq.sumBy (fun (id,r,g,b) -> if r < 13 && g < 14 && b < 15 then id else 0) |> printfn "Part 1: %A"
games |> Seq.sumBy (fun (_,r,g,b) -> r * g * b) |> printfn "Part 2: %A"