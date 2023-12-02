open System

let toGame (line : string) =
    let parts = line.Split([| "Game "; ": "; ", "; "; " |], StringSplitOptions.RemoveEmptyEntries)
    let count (color : string) = 
        parts |> Array.filter (fun x -> x.Contains color) |> Array.map (Seq.filter Char.IsDigit >> String.Concat >> int)
    (parts |> Array.head |> int, count "red", count "green", count "blue")

let games = IO.File.ReadAllLines "inputs/day02.txt" |> Array.map toGame

printfn "Part 1: %A" 
  (games |> Array.filter (fun (_,r,g,b) -> Array.forall ((>) 13) r &&  Array.forall ((>) 14) g && Array.forall ((>) 15) b) |> Array.sumBy (fun (id,_,_,_) -> id))

printfn "Part 2: %A"
  (games |> Array.sumBy (fun (_,r,g,b) -> Array.max r * Array.max g * Array.max b))