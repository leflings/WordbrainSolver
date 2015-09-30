module Board

open Types
open Directions

let tuple a b = (a,b)
let tuple3 a b c = (a,b,c)
let tuple4 a b c d = (a,b,c,d)

let private emptyChar = ' '

let init c n = Array2D.create n n c

//let get (board : 'a [,]) ((x,y) : Position) = board.[x,y]
//let set (board : 'a [,]) ((x,y) : Position) e = board.[x,y] <- e

let get (Board(b)) (Position(x,y)) = b.[x,y]
let set (Board(b)) (Position(x,y)) e = b.[x,y] <- e

let flatten (board : 'a [,]) = board |> Seq.cast<'a>

let isEmpty (Board(board)) = board |> flatten |> Seq.exists ((<>) emptyChar) |> not

let isValidPosition (Board(board)) (Position(x,y)) =
    let lx, ly = Array2D.length1 board, Array2D.length2 board
    not (y < 0 || x < 0 || y >= ly || x >= lx)

let swap b p1 p2 =
    let x = p1 ||> Array2D.get b
    (Array2D.set b <|| p1) <| (Array2D.get b <|| p2)
    (Array2D.set b <|| p2) x
    b

let private trickleDownColumns (Board(board)) =
    let shrinkColumn col =
        let n = Array.length col
        let validChars =  col |> Array.filter ((<>) ' ')
        let count = Array.length validChars
        let startIndex = n - count
        let newArray = Array.create n ' '
        Array.blit validChars 0 newArray startIndex count
        newArray
    let n = Array2D.length2 board
    for i in 0 .. n - 1 do
        let col = board.[*,i]
        board.[*,i] <- col |> shrinkColumn
    Board(board)

let copyBoardWithout (Branch(taken)) (Board(b)) =
    let newBoard = Array2D.copy b |> Board
    taken |> List.iter (fun x -> set newBoard x ' ')
    newBoard |> trickleDownColumns