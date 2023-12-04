open System ; open type StringSplitOptions
let input = IO.File.ReadAllLines "inputs/day04.txt"

input |> Array.map (
    _.Split([|":";"|"|], RemoveEmptyEntries ||| TrimEntries)
    >> Array.skip 1
    >> Array.map (_.Split([|' '|], RemoveEmptyEntries ||| TrimEntries) >> Set)
    >> Array.reduce Set.intersect
    >> Set.count
    >> function 0 -> 0 | 1 -> 1 | n -> 1 <<< (n - 1)
) |> Array.sum |> printfn "%i"

