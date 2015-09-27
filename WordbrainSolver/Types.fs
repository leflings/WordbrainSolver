namespace Types

type Direction = int
type Board = char [,]
type Position = int * int
type State = Board * Position list * Tree.Tree
type MoveVector = int * int

module Directions =

    let SW : Direction = 0
    let S  : Direction = 1
    let SE : Direction = 2
    let W  : Direction = 3
    let E  : Direction = 5
    let NW : Direction = 6
    let N  : Direction = 7
    let NE : Direction = 8

    let vectorFromMove (m : Direction) : MoveVector =
        let x = (m / 3 - 1) * -1
        let y = m % 3 - 1
        x,y
    let move (dir : Direction) (pos : Position) : Position =
        let x,y = pos
        let x',y' = vectorFromMove dir
        (x+x', y+y')
    let moves = [SW; S; SE; W; E; NW; N ; NE]
