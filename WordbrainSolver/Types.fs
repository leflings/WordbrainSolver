namespace Types

type Direction = Direction of int
type MoveVector = MoveVector of int * int

type Board = Board of char [,]
type Position = Position of int * int
type Branch = Branch of Position list
type Solution = Solution of Branch list
[<StructuredFormatDisplayAttribute("State ({board}, {taken}, Tree)")>]
type State = State of board: Board * taken: Position list * tree: Tree.Tree

module Directions =

    let SW = Direction  0
    let S  = Direction  1
    let SE = Direction  2
    let W  = Direction  3
    let E  = Direction  5
    let NW = Direction  6
    let N  = Direction  7
    let NE = Direction  8

    let vectorFromMove (Direction(m)) =
        let x = (m / 3 - 1) * -1
        let y = m % 3 - 1
        MoveVector(x,y)
    let move (Direction(_) as dir) (Position(x,y)) =
        let x',y' = vectorFromMove dir |> function | MoveVector (a,b) -> (a,b)
        Position(x+x', y+y')
    let moves = [SW; S; SE; W; E; NW; N ; NE]
