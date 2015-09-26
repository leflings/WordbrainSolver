#load "Scripts/load-project.fsx"
#time "on";;

open SuffixTree
open StringHelpers
open Types

let boardStr = """abc
def
ghi"""



let board = BoardParser.fromString boardStr
let tree = ["abe"; "bade"; "abc"; "abcd"; "qaaa"; "qaab"; "qaba"; "qabb"; "qbaa"; "qbba"; "qabc"] |> SuffixTree.buildAndShrink

tree 
|> treeFromChar 'a'
|> Option.bind (treeFromChar 'b')
|> Option.bind (treeFromChar 'e')
|> Option.bind (treeFromChar 'a')
|> Option.bind (treeFromChar 'a')

tree |> treeFrom "ab"

let initialState = (board, [0,0], tree |> treeFrom "a" |> Option.get)
Logic.availableMoves initialState

let rec walkFrom (state : State) : State list =
    let moves = Logic.availableMoves state
    let move = Logic.move state >> walkFrom
    List.collect (move) moves

let x = 
    initialState
    |> walkFrom

    




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

"con" |> isInTree t

let big = Words.englishSuffix

big
|> treeFrom "hackb"
|> Option.map collectBranches


t
|> treeFrom "pa"

