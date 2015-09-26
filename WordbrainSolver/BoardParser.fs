module BoardParser

open StringHelpers

let fromString str =
    let css =
        str
        |> String.splitWith "\n"
        |> List.ofArray
        |> List.map (fun x -> x.Trim().ToCharArray() |> List.ofArray)

    let board = Board.init ' ' (List.length css)
    css
    |> List.iteri (fun x -> List.iteri(fun y -> Board.set board (x,y)))
    board
