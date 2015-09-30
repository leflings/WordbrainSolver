#load "Scripts/load-project.fsx"
#time "on";;

open Types
open Tree
open StringHelpers

let movesFrom (board, taken, _) =
    Directions.moves
    |> List.filter (fun m ->
            let newPos = taken |> List.head |> Directions.move m
            Board.isValidPosition board newPos
            && not (List.exists ((=) newPos) taken))

let inline moveTo pos (board, taken, tree) =
    match tree |> treeFromChar (Board.get board pos) with
    | Some (tree) -> Some(board, pos :: taken, tree)
    | None -> None

let move ((_, taken, _) as state) dir : State option =
    let newPos = taken |> List.head |> Directions.move dir
    moveTo newPos (state)

let branchesFrom maxDepth state =
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
    recur [] state// |> List.map (fun (Branch(b)) -> Branch(List.rev b))

let allBranches maxDepth ((board, taken, tree) as state) =
    if List.isEmpty taken |> not then [] else
    [ for x in 0..Array2D.length1 board-1 do
        for y in 0..Array2D.length2 board-1 do
            yield moveTo (x,y) state
            ]
    |> List.choose id
    |> List.collect (branchesFrom maxDepth)

let branchToStr board branch = branch |> (List.map (Board.get board) >> List.rev >> String.fromCharList)

/// Returns the list without the first occurence of x
let listWithoutFirst x xs =
    let rec f acc = function
        | [] -> acc |> List.rev
        | h::tail when h = x -> (List.rev acc) @ tail
        | h::tail -> f (h::acc) tail
    f [] xs

// Returns new state with an empty branch, a copied board without the accepted branch, and untouched tree
let acceptBranch branch (board, taken, tree) =
    let newBoard = Board.copyBoardWithout board branch
    (newBoard, [], tree)
    

let findSolutions wordLengths state =
    let rec recurse acc tempAcc wordLengths state =
        if List.isEmpty wordLengths then (tempAcc |> List.rev) :: acc else
        let maxDepth = wordLengths |> List.max
        allBranches (Some maxDepth) state
        |> List.groupBy (List.length)
        |> List.filter (fun (n, _) -> List.contains n wordLengths)
        |> List.collect (fun (n, branches) ->
            let newWordLengths = listWithoutFirst n wordLengths
            branches
            |> List.collect (fun branch ->
                let newState = state |> acceptBranch branch
                recurse acc (branch :: tempAcc) newWordLengths newState))
    recurse [] [] wordLengths state
    
let solutionToStrings state branches =
    ((state,[]), branches)
    ||> List.fold (fun (((board,_,_) as state),acc) branch ->
        let branchStr = branchToStr board branch
        let newAcc = branchStr :: acc
        (acceptBranch branch state), newAcc
        )
    |> snd |> List.rev

let tree = Words.englishWords |> Tree.buildTree

let wordLengths = [3;6]
let boardStr = "estgiegnn"
let board = BoardParser.boardFrom boardStr
let state = (board, [], tree)

let solution = findSolutions wordLengths state |> List.head

let solve lengths board =
    let state = (BoardParser.boardFrom board, [], tree)
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

solution
|> solutionToStrings state

state
|> allBranches None


state
|> allBranches (Some 3)
|> List.map (branchToStr board)



