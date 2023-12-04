open System ; open type StringSplitOptions
let input = IO.File.ReadAllLines "inputs/day04.txt"

let cards = 
    input |> Array.map (
        _.Split([|":";"|"|], RemoveEmptyEntries ||| TrimEntries)>> Array.skip 1
        >> Array.map (_.Split([|' '|], RemoveEmptyEntries ||| TrimEntries) >> Set)
        >> Array.reduce Set.intersect
        >> Set.count)

cards |> Array.map (function 0 -> 0 | 1 -> 1 | n -> 1 <<< (n - 1)) |> Array.sum |> printfn "Part 1: %i\n\n"

let rec solver total (cards : (int*int) list) =
    match cards with
    | [] -> total
    | (cardCount, cardsToAdd)::remainingCards ->
        let remaining = if cardsToAdd <= remainingCards.Length then cardsToAdd else remainingCards.Length
        let newTotal = total + cardCount
        let updatedNext = remainingCards.[..remaining-1 ] |> List.map (fun (n,x) -> (n+cardCount, x))
        let skippedNext = remainingCards.[updatedNext.Length..]
        solver newTotal (updatedNext @ skippedNext)

solver 0 (cards |> Array.map (fun c -> 1, c) |> Array.toList) |> printfn "Part 2: %i"