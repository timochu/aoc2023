open System
let input = IO.File.ReadAllLines "inputs/day01.txt"

let rec solver acc (input : string) =
    match input |> Seq.toList with
    | []                         -> acc
    | 'o'::'n'::'e'::_           -> solver ('1' :: acc) input[1..]
    | 't'::'w'::'o'::_           -> solver ('2' :: acc) input[1..]
    | 't'::'h'::'r'::'e'::'e'::_ -> solver ('3' :: acc) input[1..]
    | 'f'::'o'::'u'::'r'::_      -> solver ('4' :: acc) input[1..]
    | 'f'::'i'::'v'::'e'::_      -> solver ('5' :: acc) input[1..]
    | 's'::'i'::'x'::_           -> solver ('6' :: acc) input[1..]
    | 's'::'e'::'v'::'e'::'n'::_ -> solver ('7' :: acc) input[1..]
    | 'e'::'i'::'g'::'h'::'t'::_ -> solver ('8' :: acc) input[1..]
    | 'n'::'i'::'n'::'e'::_      -> solver ('9' :: acc) input[1..]
    | c::_ when Char.IsDigit c   -> solver ( c  :: acc) input[1..]
    | _                          -> solver         acc  input[1..]

printfn "Part 1: %i" (input |> Array.sumBy (Seq.filter Char.IsDigit >> (fun c -> $"{Seq.head c}{Seq.last c}") >> int))
printfn "Part 2: %i" (input |> Array.sumBy (solver [] >> (fun c -> $"{Seq.last c}{Seq.head c}") >> int))
