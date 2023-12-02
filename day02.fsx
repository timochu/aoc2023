open System
let input = IO.File.ReadAllLines "inputs/day02.txt"

let lineToGame (line : string) =
    let parts = line.Split([| "Game "; ": "; ", "; "; " |], StringSplitOptions.RemoveEmptyEntries)
    let count (color : string) = 
        parts |> Array.filter (fun x -> x.Contains color) |> Array.map (Seq.filter Char.IsDigit >> String.Concat >> int)
    (parts |> Array.head |> int, count "red", count "green", count "blue")

let games = input |> Array.map lineToGame

games
|> Array.filter (fun (_,r,g,b) -> r |> Array.forall ((>) 13) && g |> Array.forall ((>) 14) && b |> Array.forall ((>) 15)) 
|> Array.sumBy (fun (id,_,_,_) -> id) |> printfn "Part 1: %A"

games 
|> Array.map (fun (_,r,g,b) -> Array.max r * Array.max g * Array.max b) |> Array.sum |> printfn "Part 2: %A"