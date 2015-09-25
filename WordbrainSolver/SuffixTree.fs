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
            | LongLeaf (str) -> str = new System.String(chars |> Array.ofList)
            | Node (ch, _) when c <> ch -> false
            | Node(_,ts) ->
                 match chars' with
                 | [] -> false
                 | _ -> ts |> List.exists (search chars')
    let chars = str |> String.toLower |> fun x -> x.ToCharArray() |> List.ofArray
    search chars tree
    
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
