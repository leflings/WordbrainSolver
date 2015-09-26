module Logic

open Types
open Directions

let canMove (state : State) dir =
    let board, taken, tree = state
    let pos = List.head taken
    let newPos = Board.move pos dir
    if not (Board.isValidPosition board newPos)
    then false
    else
        let isFree = List.forall ((<>) newPos) taken
        let isInTree = tree |> SuffixTree.treeFromChar (Board.get board newPos) |> Option.isSome
        isFree && isInTree

let availableMoves state =
    Directions.moves
    |> List.filter (canMove state)

//let tryMove (state : State) dir : State =
//    let (baord, taken, tree) = state
//    let pos = List.head taken

let move (state : State) dir : State =
    let (board, taken, tree) = state
    let pos = Board.move (List.head taken) dir
    let tree' =
        tree
        |> SuffixTree.treeFromChar (Board.get board pos)
        |> Option.get
    let taken' = pos :: taken
    (board, taken', tree')
