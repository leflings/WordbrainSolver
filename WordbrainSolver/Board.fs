module Board

open Types
open Directions

let tuple a b = (a,b)
let tuple3 a b c = (a,b,c)
let tuple4 a b c d = (a,b,c,d)

let init c n = Array2D.create n n c

let get (board : 'a [,]) (x,y) = board.[x,y]
let set (board : 'a [,]) (x,y) e = board.[x,y] <- e

let isValidPosition board (posx,posy) =
    let lx, ly = Array2D.length1 board, Array2D.length2 board
    not (posy < 0 || posx < 0 || posy >= ly || posx >= lx)

//let canMove board pos dir = move pos dir |> isValidPosition board
//let swap b p1 p2 =
//    let x = p1 ||> Array2D.get b
//    (Array2D.set b <|| p1) <| (Array2D.get b <|| p2)
//    (Array2D.set b <|| p2) x
//    b
//
//let validMoves board pos = moves |> List.filter (move pos >> isValidPosition board) 