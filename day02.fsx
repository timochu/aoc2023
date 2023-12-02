open System
let toGame (line : string) =
    let items = line.Split([| ": "; ", "; "; " |], StringSplitOptions.RemoveEmptyEntries)
    let get (name : string) = items |> Array.filter (fun x -> x.Contains name) |> Array.map (Seq.filter Char.IsDigit >> String.Concat >> int)
    get "Game" |> Seq.head, get "red" |> Seq.max, get "green" |> Seq.max, get "blue" |> Seq.max

let games = "inputs/day02.txt" |> IO.File.ReadAllLines |> Array.map toGame
games |> Seq.filter (fun (_,r,g,b) -> r < 13 && g < 14 &&  b < 15) |> Seq.sumBy (fun (id,_,_,_) -> id) |> printfn "Part 1: %A"
games |> Seq.sumBy (fun (_,r,g,b) -> r * g * b) |> printfn "Part 2: %A"