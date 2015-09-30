#load "Scripts/load-project.fsx"
#time "on";;

open Types
open Tree
open StringHelpers

let movesFrom (State(board, taken, _)) =
    Directions.moves
    |> List.filter (fun m ->
            let newPos = taken |> List.head |> Directions.move m
            Board.isValidPosition board newPos
            && not (List.exists ((=) newPos) taken))

let moveTo (Position(_) as pos) (State(board, taken, tree)) =
    match tree |> treeFromChar (Board.get board pos) with
    | Some (tree) -> Some(State(board, pos :: taken, tree))
    | None -> None

let move (State(_, taken, _) as state) dir : State option =
    let newPos = taken |> List.head |> Directions.move dir
    moveTo newPos (state)

let branchesFrom maxDepth state =
    let rec recur acc (State(_, taken, tree) as state) =
        match tree with
        | Word -> Branch(taken) :: acc
        | Root (ts)
        | Node (_,ts) ->
            let newAcc =
                match ts|> List.exists (function | Word -> true | _ -> false) with
                | true -> Branch(taken) :: acc
                | false -> acc
            match state |> movesFrom |> List.choose (move state), maxDepth with
            | [], _ -> newAcc
            | walks, None -> List.collect (recur newAcc) walks
            | walks, Some (md) ->
                if md = taken.Length then newAcc else List.collect (recur newAcc) walks
    recur [] state// |> List.map (fun (Branch(b)) -> Branch(List.rev b))

let allBranches maxDepth (State(Board(board), taken, tree) as state) =
    if List.isEmpty taken |> not then [] else
    [ for x in 0..Array2D.length1 board-1 do
        for y in 0..Array2D.length2 board-1 do
            yield moveTo (Position(x,y)) state
            ]
    |> List.choose id
    |> List.collect (branchesFrom maxDepth)

let branchToStr board (Branch(b)) = b |> (List.map (Board.get board) >> List.rev >> String.fromCharList)

/// Returns the list without the first occurence of x
let listWithoutFirst x xs =
    let rec f acc = function
        | [] -> acc |> List.rev
        | h::tail when h = x -> (List.rev acc) @ tail
        | h::tail -> f (h::acc) tail
    f [] xs

// Returns new state with an empty branch, a copied board without the accepted branch, and untouched tree
let acceptBranch branch (State(board, _, tree)) =
    let newBoard = Board.copyBoardWithout branch board
    State(newBoard, [], tree)
    

let findSolutions wordLengths state =
    let rec recurse acc tempAcc wordLengths state =
        if List.isEmpty wordLengths then Solution(tempAcc |> List.rev) :: acc else
        let maxDepth = wordLengths |> List.max
        allBranches (Some maxDepth) state
        |> List.groupBy (fun (Branch(ps)) -> List.length ps)
        |> List.filter (fun (n, _) -> List.contains n wordLengths)
        |> List.collect (fun (n, branches) ->
            let newWordLengths = listWithoutFirst n wordLengths
            branches
            |> List.collect (fun branch ->
                let newState = state |> acceptBranch branch
                recurse acc (branch :: tempAcc) newWordLengths newState))
    recurse [] [] wordLengths state
    
let solutionToStrings state (Solution(branches)) =
    ((state,[]), branches)
    ||> List.fold (fun ((State(board,_,_) as state),acc) branch ->
        let branchStr = branchToStr board branch
        let newAcc = branchStr :: acc
        (acceptBranch branch state), newAcc
        )
    |> snd |> List.rev

let tree = Words.englishWords |> Tree.buildTree

let wordLengths = [3;6]
let boardStr = "estgiegnn"
let board = BoardParser.boardFrom boardStr
let state = State(board, [], tree)

let solution = findSolutions wordLengths state |> List.head

let solve lengths board =
    let state = State(BoardParser.boardFrom board, [], tree)
    findSolutions lengths state
    |> List.iter (printfn "%A" << solutionToStrings state)
solve [5;4] "llasregsw"

solve [3;6] "gbytahrop"
solve [5;4;5;6;5] "+etrkk +ksiei +utbsk +rreuu +vkadm"

solve [3;5;8] "heocsonburmabtrp" 
solve [2;7] "ipvntsach"

(wordLengths,state)
||> findSolutions
|> List.map (solutionToStrings state)
|> 

solution
|> solutionToStrings state

state
|> allBranches None

let branch = Branch [Position (0,2); Position (1,1); Position (2,2)]




state
|> allBranches (Some 3)
|> List.map (branchToStr board)



