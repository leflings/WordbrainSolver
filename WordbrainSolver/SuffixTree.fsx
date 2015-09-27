#load "Scripts/load-project.fsx"
#time "on";;

open Types
open Tree
open StringHelpers

let boardStr = """abc
def
ghi"""

let boardStr = """
etrkk
ksiei
utbsk
rreuu
vkadm
"""


let board = BoardParser.fromString boardStr
let tree = ["abe"; "bade"; "abc"; "abcf"; "abcd"; "qaaa"; "qaab"; "qaba"; "qabb"; "qbaa"; "qbba"; "qabc"] |> Tree.buildTree

let bigTree = Words.englishWords |> Tree.buildTree

tree 
|> treeFromChar 'a'
|> Option.bind (treeFromChar 'b')
|> Option.bind (treeFromChar 'e')
|> Option.bind (treeFromChar 'a')
|> Option.bind (treeFromChar 'a')

let movesFrom (state : State) =
    let board, taken, _ = state
    let pos = List.head taken
    Directions.moves
    |> List.filter (fun m ->
            let newPos = pos |> Directions.move m
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
    

bigTree
|> treeCollectedFrom "hie"
        
let initState = (board, [], bigTree)
initState
|> allBranches
|> List.map (List.map (Board.get board))
|> List.map (List.rev >> String.fromCharList)
|> List.distinct


let initialState = (board, [0,1;0,0], tree |> treeFrom "ab" |> Option.get)

let a = 
initialState
|> branchesFrom

initialState
|> Option.bind (move Directions.S)

let oneStep state =
    state |> movesFrom |> List.choose (fun m -> move m state)

initialState
|> oneStep
|> List.head
|> oneStep
|> List.map oneStep

|> List.map movesFrom

Logic.availableMoves initialState



open SuffixTree

let stringOfWords = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum"

let words =
    stringOfWords
    |> String.splitWith " "
    |> List.ofArray
    |> List.map (String.toLower)
    |> List.distinct
let t = words |> buildAndShrink
t |> treeFrom "pri"
|> Option.map collectBranches

let big = Words.englishSuffix

big
|> treeFrom "hackb"
|> Option.map collectBranches


t
|> treeFrom "pa"

