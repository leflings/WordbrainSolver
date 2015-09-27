#load "Scripts/load-project.fsx"
#time "on";;

open Types
open Tree
open StringHelpers

let movesFrom ((board, taken, _) : State) =
    Directions.moves
    |> List.filter (fun m ->
            let newPos = taken |> List.head |> Directions.move m
            Board.isValidPosition board newPos
            && not (List.exists ((=) newPos) taken))

let moveTo pos (state : State) =
    let board, taken, tree = state
    match tree |> treeFromChar (Board.get board pos) with
    | Some (tree) -> Some(board, pos :: taken, tree)
    | None -> None

let move (state : State) dir : State option =
    let _, taken, _ = state
    let newPos = taken |> List.head |> Directions.move dir
    moveTo newPos (state)

let branchesFrom maxDepth (state : State) =
    let rec recur acc ((_, taken, tree) as state) =
        match tree with
        | Word -> taken :: acc
        | Root (ts)
        | Node (_,ts) ->
            let newAcc =
                match ts|> List.exists (function | Word -> true | _ -> false) with
                | true -> taken :: acc
                | false -> acc
            match state |> movesFrom |> List.choose (move state), maxDepth with
            | [], _ -> newAcc
            | walks, None -> List.collect (recur newAcc) walks
            | walks, Some (md) ->
                if md = taken.Length then newAcc else List.collect (recur newAcc) walks
    recur [] state

let allBranches maxDepth ((board, taken, tree) as state : State) =
    if List.isEmpty taken |> not then [] else
    [ for x in 0..Array2D.length1 board-1 do
        for y in 0..Array2D.length2 board-1 do
            yield moveTo (x,y) state
            ]
    |> List.choose id
    |> List.collect (branchesFrom maxDepth)

let branchToStr board = List.map (Board.get board) >> List.rev >> String.fromCharList

//let findSolutions wordLengths state =
    

let wordLengths = [4;5]
let boardStr = "lselidlod"
let board = BoardParser.boardFrom boardStr
let tree = Words.englishWords |> Tree.buildTree
let state = (board, [], tree)

state
|> allBranches (Some 3)
|> List.map (branchToStr board)



