module Tree

open StringHelpers

type Tree =
    | Word of string
    | Node of char * Tree list

let buildTree (words : string list) : Tree =
    let rec recurse acc ws =
        let chars,suffixes = ws |> List.groupBy (String.nth 0) |> List.unzip
        let suffixes' = suffixes |> List.map (List.map (String.skipChars 1))
        (chars, suffixes')
        ||> List.map2 (fun ch words ->
            let newAcc = ch :: acc
            match words |> List.partition ((<>) "") with
            | words, [] -> Node(ch, recurse newAcc words)
            | words, [_] ->
                let word = Word (newAcc |> List.rev |> String.fromCharList)
                Node(ch, word :: recurse newAcc words)
            | _ -> failwith "Two words terminated at the same time. Is word list distinct?"
        )
    Node('%', recurse [] words)

["abe"; "bade"; "abc"; "abcf"; "qaaa"; "qaab"; "qaba"; "qabb"; "qbaa"; "qbba"; "qabc"] |> buildTree
["flem";  "flemse"; "flemster"; "flyve";"flemming"] |> buildTree;;

