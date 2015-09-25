module SuffixTree

type SuffixTree =
    | Root of SuffixTree list
    | LongNode of string * SuffixTree List
    | Node of char *  SuffixTree list
    | LongLeaf of string
    | Leaf of char

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

let shrinkTree tree =
    let rec recurse s = function
        | Root (ts) -> ts |> List.map (recurse s) |> Root
        | LongNode (str, ts) -> LongNode(str, ts |> List.map (recurse s))
        | Node (c, [t]) -> recurse (c::s) t
        | Node (c, []) ->
            match s with
            | [] -> Leaf c
            | _ -> (c :: s) |> List.rev |> String.fromCharList |> LongLeaf
        | Node (c, ts) ->
            match s with
            | [] -> Node(c, ts |> List.map (recurse s))
            | _ ->
                let str = (c :: s) |> List.rev |> String.fromCharList
                LongNode(str, ts |> List.map (recurse []))
        | Leaf (c) ->
            match s with
            | [] -> Leaf c
            | _ -> (c :: s) |> List.rev |> String.fromCharList |> LongLeaf
        | LongLeaf(str) ->
            match s with
            | [] -> LongLeaf str
            | _ ->
                let newStr = s |> List.rev |> String.fromCharList
                LongLeaf(newStr + str)
    recurse [] tree

/// Removes common prefix, if any, from both lists
let rec eatEquals xs ys =
    match xs, ys with
    | [], _
    | _, []
    | [], [] -> xs, ys
    | x::xs', y::ys' ->
        if x = y
            then eatEquals xs' ys'
            else xs, ys

let isInTree tree str =
    let rec search chars tree =
        match chars with
        | [] -> true
        | c::chars' ->
            match tree with
            | Root (ts) -> ts |> List.exists (search chars)
            | Leaf (ch) -> c = ch && List.isEmpty chars'
            | LongLeaf (str) -> str = String.fromCharList chars
            | LongNode(str, ts) ->
                match eatEquals (chars) (str |> String.toCharList) with
                | [], _ -> true
                | cs, [] -> ts |> List.exists (search cs)
                | _ -> false
            | Node (ch, ts) ->
                match c = ch , List.isEmpty chars' with
                | false, _ 
                | true, true -> false
                | true, false -> ts |> List.exists (search chars')
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
                match eatEquals s (str |> String.toCharList) with
                | [], [] -> None
                | [], xs -> xs |> String.fromCharList |> LongLeaf |> Some
                | _ -> None
            | LongNode (str, ts) ->
                match eatEquals s (str |> String.toCharList) with
                | [], [] -> ts |> Root |> Some
                | [], xs ->
                    let newStr = xs |> String.fromCharList
                    Some (LongNode(newStr, ts))
                | _ -> None
            | Node (ch,ts) ->
                if c <> ch then None
                else if List.isEmpty cs then Some (Root ts)
                else ts |> List.tryPick (find cs)
    find chars tree
                
let collectBranches tree =
    let rec recurse acc tree =
        match tree with
        | Root(ts) -> List.collect (recurse acc) ts
        | Leaf(c) -> [c :: acc |> List.rev |> String.fromCharList]
        | LongLeaf(str) ->
            let prefix = acc |> List.rev |> String.fromCharList
            [prefix + str]
        | LongNode(str, ts) ->
            let cs = str |> String.toCharList |> List.rev
            List.collect (recurse (cs @ acc)) ts
        | Node(c, ts) -> List.collect (recurse (c::acc)) ts
    recurse [] tree
