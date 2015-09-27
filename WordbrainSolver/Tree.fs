module Tree

open StringHelpers

type Tree =
    | Word //of string
    | Node of char * Tree list
    | Root of Tree list

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
//                let word = Word (newAcc |> List.rev |> String.fromCharList)
//                Node(ch, word :: recurse newAcc words)
                Node(ch, Word :: recurse newAcc words)
            | _ -> failwith "Two words terminated at the same time. Is word list distinct?"
        )
    Root(recurse [] words)

/// Returns Some Tree if path (ch) is available else None
let rec treeFromChar ch tree =
    match tree with
    | Root ts -> ts |> List.tryPick (treeFromChar ch)
    | Word -> None
    | Node (c, ts) ->
        match c = ch, ts with
        | false, _ -> None
        | true, [Word] -> Some Word
        | true, _ -> Some (Root ts)

/// Returns Some Tree if path (str) is available None
let treeFrom str tree =
    let rec recurse chars tree =
        match chars with
        | [] -> Some tree
        | c :: cs ->
            treeFromChar c tree |> Option.bind (recurse cs)
    tree |> recurse (str |> String.toLower |> String.toCharList)

let rec private collectAcc acc = function
    | Root ts -> List.collect (collectAcc acc) ts
    | Word ->
        match acc with
        | [] -> []
        | _ -> [acc |> List.rev |> String.fromCharList]
    | Node (c, ts) -> ts |> List.collect (collectAcc (c::acc))

/// Collect the strings representing the branches
let collect tree = collectAcc [] tree
/// Collect the string representing the branches, with str prepended
let collectFrom str tree = collectAcc (str |> String.toCharList |> List.rev) tree

/// Searches and collects tree from str
/// Aka, find words begginning with str
let treeCollectedFrom str = treeFrom str >> Option.bind (collectFrom str >> Some)