module SuffixTree

type SuffixTree =
    | Root of SuffixTree list
    | Node of char *  SuffixTree list
    | LongLeaf of string
    | Leaf of char

let isInTree tree str =
    let rec search chars tree =
        match chars with
        | [] -> true
        | c::chars' ->
            match tree with
            | Root (ts) -> ts |> List.exists (search chars)
            | Leaf (ch) when c <> ch -> false
            | Leaf _ ->
                match chars' with
                | [] -> true
                | _ -> false
            | LongLeaf (str) -> str = String.fromCharList chars
            | Node (ch, _) when c <> ch -> false
            | Node(_,ts) ->
                 match chars' with
                 | [] -> false
                 | _ -> ts |> List.exists (search chars')
    let chars = str |> String.toLower |> fun x -> x.ToCharArray() |> List.ofArray
    search chars tree

let treeFrom str tree =
    let chars = str |> String.toLower |> String.toCharList
    let rec find s t =
        match s with
        | [] -> Some t
        | c::cs ->
            match t with
            | Root (ts) -> ts |> List.tryPick (find s)
            | Leaf (ch) -> None
            | LongLeaf (str) ->
                let remainingStr = String.fromCharList s
                match str.StartsWith(remainingStr) with
                | false -> None
                | true ->
                    match str |> String.skipChars (remainingStr.Length) with
                    | "" -> None
                    | str' -> Some (LongLeaf str')
            | Node (ch,ts) ->
                if c <> ch then None
                else if List.isEmpty cs then Some (Root ts)
                else ts |> List.tryPick (find cs)
    find chars tree
                
let buildTree words =
    let rec recurse ws =
        let chars,suffixes = ws |> List.groupBy (String.nth 0) |> List.unzip
        let suffixes' = suffixes |> List.map (List.map (String.skipChars 1) >> List.filter ((<>) ""))
        (chars, suffixes')
        ||> List.map2 (fun ch words ->
            match words with
            | [] -> Leaf ch
            | [x] -> LongLeaf (string ch + x)
            | _ -> Node(ch, recurse words)
            )
    Root (recurse words)

let collectBranches tree =
    let rec recurse acc tree =
        match tree with
        | Root(ts) -> List.collect (recurse acc) ts
        | Leaf(c) -> [c :: acc |> List.rev |> String.fromCharList]
        | LongLeaf(str) ->
            let prefix = acc |> List.rev |> String.fromCharList
            [prefix + str]
        | Node(c, ts) -> List.collect (recurse (c::acc)) ts
    recurse [] tree
