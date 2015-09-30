module BoardParser

open StringHelpers

let private allowedLengths = [1..100] |> List.map (fun x -> x, x * x)

let private to2D arr =
    let length = Array.length arr
    match allowedLengths |> List.tryFind (snd >> (=) length) with
    | None -> failwith "Array length unfit for transformation to uniform 2D array"
    | Some (n, _) ->
        let chars = Array.chunkBySize n arr
        Array2D.init n n (fun x y -> chars.[x].[y])

/// Takes all letters (System.Char.IsLetter) and forms a uniform Array2D
let boardFrom =
    String.toLower
    >> String.toCharArray
    >> Array.filter (System.Char.IsLetter)
    >> to2D
    >> Types.Board
