open System
let toGame (line : string) =
    let items = line.Split([| ": "; ", "; "; " |], StringSplitOptions.RemoveEmptyEntries)
    let get (name : string) = items |> Array.filter (fun x -> x.Contains name) |> Array.map (Seq.filter Char.IsDigit >> String.Concat >> int)
    get "Game" |> Array.head, get "red", get "green", get "blue"

let games = "inputs/day02.txt" |> IO.File.ReadAllLines |> Array.map toGame
games |> Seq.filter (fun (_,r,g,b) -> Seq.forall ((>) 13) r && Seq.forall ((>) 14) g && Seq.forall ((>) 15) b) |> Seq.sumBy (fun (id,_,_,_) -> id) |> printfn "Part 1: %A"
games |> Seq.sumBy (fun (_,r,g,b) -> Seq.max r * Seq.max g * Seq.max b) |> printfn "Part 2: %A"