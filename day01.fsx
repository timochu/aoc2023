let input = System.IO.File.ReadAllLines "inputs/day01.txt"

let part1 = input |> Array.sumBy (Seq.filter System.Char.IsDigit >> (fun c -> $"{Seq.head c}{Seq.last c}") >> int)

let solver input =
    let rec foo acc (input : string) =
        match input with
        | input when input.StartsWith("one")   -> foo $"{acc}1" (input[1..])
        | input when input.StartsWith("two")   -> foo $"{acc}2" (input[1..])
        | input when input.StartsWith("three") -> foo $"{acc}3" (input[1..])
        | input when input.StartsWith("four")  -> foo $"{acc}4" (input[1..])
        | input when input.StartsWith("five")  -> foo $"{acc}5" (input[1..])
        | input when input.StartsWith("six")   -> foo $"{acc}6" (input[1..])
        | input when input.StartsWith("seven") -> foo $"{acc}7" (input[1..])
        | input when input.StartsWith("eight") -> foo $"{acc}8" (input[1..])
        | input when input.StartsWith("nine")  -> foo $"{acc}9" (input[1..])
        | input when input.Length > 0 && input[0] |> System.Char.IsDigit -> foo $"{acc}{input[0]}" (input[1..])
        | input when input.Length > 0 -> foo acc (input[1..])
        | _ -> acc
    input |> foo "" |> (fun c -> $"{Seq.head c}{Seq.last c}") |> int

printfn "Part 1: %i" part1
printfn "Part 2: %i" (input |> Array.sumBy solver)