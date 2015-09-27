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

let moveTo pos (state : State) : State option =
    let board, taken, tree = state
    let ch = Board.get board pos
    match treeFromChar ch tree with
    | Some (tree) -> Some(board, pos :: taken, tree)
    | None -> None

let move dir (state : State) : State option =
    let _, taken, _ = state
    let newPos = taken |> List.head |> Directions.move dir
    moveTo newPos (state)

let branchesFrom (state : State) =
    let rec recur acc ((board, taken, tree) as state) =
        let moves = movesFrom state
        match tree with
        | Word -> taken :: acc
        | Root (ts)
        | Node (_,ts) ->
            let walks = moves |> List.choose (fun m -> move m state)
            let newAcc =
                match ts|> List.exists (function | Word -> true | _ -> false) with
                | true -> taken :: acc
                | false -> acc
            match walks with
            | [] -> newAcc
            | _ -> List.collect (recur newAcc) walks

    recur [] state

let allBranches ((board, taken, tree) as state : State) =
    if List.isEmpty taken |> not then [] else
    [ for x in 0..Array2D.length1 board-1 do
        for y in 0..Array2D.length2 board-1 do
            yield moveTo (x,y) state
            ]
    |> List.choose id
    |> List.collect (branchesFrom)

let branchToStr board = List.map (Board.get board) >> List.rev >> String.fromCharList

let boardStr = """
etrkk
ksiei
utbsk
rreuu
vkadm
"""
boardStr |> BoardParser.boardFrom

let board = BoardParser.boardFrom boardStr
let tree = ["abe"; "bade"; "abc"; "abcf"; "abcd"; "qaaa"; "qaab"; "qaba"; "qabb"; "qbaa"; "qbba"; "qabc"] |> Tree.buildTree

let bigTree = Words.englishWords |> Tree.buildTree

let charArray = Array.chunkBySize 3 ("abcdefghi".ToCharArray())
Array2D.init 3 3 (fun x y -> charArray.[x].[y])
    

let testString = """
sex
mot
asd"""

let testBoard = BoardParser.boardFrom testString
        
let testState = (testBoard, [], bigTree)

let firstTake = testState |> allBranches |> List.distinct |> List.head

let newBoard = Board.copyBoardWithout testBoard firstTake

let secondTake = (newBoard, [], bigTree) |> allBranches |> List.distinct |> List.map (branchToStr board)

testState
|> allBranches
|> List.distinct
|> List.map (branchToStr testBoard)



