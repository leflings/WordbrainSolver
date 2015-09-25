module Words

let englishWords =
    TNX.NamesAndPassword.WorldDictionary.GetWordsForCulture("en")
    |> Seq.map String.toLower
    |> Set.ofSeq
    |> Set.toList

let englishSuffix =
    englishWords
    |> SuffixTree.buildTree