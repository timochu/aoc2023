let toGame (line : string) =
    let parts = line.Split([| "Game "; ": "; ", "; "; " |], System.StringSplitOptions.RemoveEmptyEntries)
    let count (color : string) = 
        parts |> Array.filter (fun x -> x.Contains color) |> Array.map (Seq.filter System.Char.IsDigit >> System.String.Concat >> int)
    parts |> Array.head |> int, count "red", count "green", count "blue"

let games = "inputs/day02.txt" |> System.IO.File.ReadAllLines |> Array.map toGame
games |> Seq.filter (fun (_,r,g,b) -> Seq.forall ((>) 13) r && Seq.forall ((>) 14) g && Seq.forall ((>) 15) b) |> Seq.sumBy (fun (id,_,_,_) -> id) |> printfn "Part 1: %A" 
games |> Seq.sumBy (fun (_,r,g,b) -> Seq.max r * Seq.max g * Seq.max b) |> printfn "Part 2: %A"