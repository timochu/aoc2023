open System
let input = IO.File.ReadAllLines "inputs/day01.txt"

let solver input =
    let rec foo acc (input : string) =
        match input |> Seq.toList with
        | []                          -> acc
        | 'o'::'n'::'e'::_           -> foo $"{acc}1" input[1..]
        | 't'::'w'::'o'::_           -> foo $"{acc}2" input[1..]
        | 't'::'h'::'r'::'e'::'e'::_ -> foo $"{acc}3" input[1..]
        | 'f'::'o'::'u'::'r'::_      -> foo $"{acc}4" input[1..]
        | 'f'::'i'::'v'::'e'::_      -> foo $"{acc}5" input[1..]
        | 's'::'i'::'x'::_           -> foo $"{acc}6" input[1..]
        | 's'::'e'::'v'::'e'::'n'::_ -> foo $"{acc}7" input[1..]
        | 'e'::'i'::'g'::'h'::'t'::_ -> foo $"{acc}8" input[1..]
        | 'n'::'i'::'n'::'e'::_      -> foo $"{acc}9" input[1..]
        | c::_ when Char.IsDigit c   -> foo $"{acc}{c}" input[1..]
        | _::_                       -> foo acc input[1..]
    input |> foo "" |> (fun c -> $"{Seq.head c}{Seq.last c}") |> int

printfn "Part 1: %i" (input |> Array.sumBy (Seq.filter Char.IsDigit >> (fun c -> $"{Seq.head c}{Seq.last c}") >> int))
printfn "Part 2: %i" (input |> Array.sumBy solver)