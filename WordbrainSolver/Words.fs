module Words

let englishWords =
    TNX.NamesAndPassword.WorldDictionary.GetWordsForCulture("en")
    |> Seq.map StringHelpers.String.toLower
    |> Set.ofSeq
    |> Set.toList

let englishSuffix =
    englishWords
    |> SuffixTree.buildAndShrink