module Board

open Directions

type Direction = int
type Board = char [,]
type Position = int * int
type State = Board * Position
type Move = Position * Direction -> Position
type IsValidMove = Position * Direction -> bool

let tuple a b = (a,b)
let tuple3 a b c = (a,b,c)
let tuple4 a b c d = (a,b,c,d)

let init c n = Array2D.init n n (sprintf "%i,%i")

let move pos dir =
    let x,y = pos
    let x',y' = vectorFromMove dir
    (x+x', y+y')

let isValidPosition board (posx,posy) =
    let lx, ly = Array2D.length1 board, Array2D.length2 board
    not (posy < 0 || posx < 0 || posy >= ly || posx >= lx)

let canMove board pos dir = move pos dir |> isValidPosition board
let swap b p1 p2 =
    let x = p1 ||> Array2D.get b
    (Array2D.set b <|| p1) <| (Array2D.get b <|| p2)
    (Array2D.set b <|| p2) x
    b

let validMoves board pos = moves |> List.filter (move pos >> isValidPosition board) 