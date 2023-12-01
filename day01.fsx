open System
let input = IO.File.ReadAllLines "inputs/day01.txt"

let rec solver acc (input : string) =
    match input |> Seq.toList with
    | []                          -> acc
    | 'o'::'n'::'e'::_           -> solver $"{acc}1" input[1..]
    | 't'::'w'::'o'::_           -> solver $"{acc}2" input[1..]
    | 't'::'h'::'r'::'e'::'e'::_ -> solver $"{acc}3" input[1..]
    | 'f'::'o'::'u'::'r'::_      -> solver $"{acc}4" input[1..]
    | 'f'::'i'::'v'::'e'::_      -> solver $"{acc}5" input[1..]
    | 's'::'i'::'x'::_           -> solver $"{acc}6" input[1..]
    | 's'::'e'::'v'::'e'::'n'::_ -> solver $"{acc}7" input[1..]
    | 'e'::'i'::'g'::'h'::'t'::_ -> solver $"{acc}8" input[1..]
    | 'n'::'i'::'n'::'e'::_      -> solver $"{acc}9" input[1..]
    | c::_ when Char.IsDigit c   -> solver $"{acc}{c}" input[1..]
    | _::_                       -> solver acc input[1..]

printfn "Part 1: %i" (input |> Array.sumBy (Seq.filter Char.IsDigit >> (fun c -> $"{Seq.head c}{Seq.last c}") >> int))
printfn "Part 2: %i" (input |> Array.sumBy (solver "" >> (fun c -> $"{Seq.head c}{Seq.last c}") >> int))
