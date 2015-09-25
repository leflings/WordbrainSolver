#load "Scripts/load-project.fsx"
#time "on";;

open SuffixTree

let stringOfWords = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum"

let words =
    stringOfWords
    |> String.splitWith " "
    |> List.ofArray
    |> List.map (String.toLower)
    |> List.distinct

let t = words |> buildTree
t |> shrinkTree
t |> shrinkTree |> treeFrom "pri"
|> Option.map collectBranches

"con" |> isInTree t

let bigT = Words.englishSuffix
let bs = bigT |> shrinkTree

bs
|> treeFrom "wack"
|> Option.map collectBranches


t
|> treeFrom "pa"

