module String

let splitWith (delim : string) (str : string) = str.Split([|delim|], System.StringSplitOptions.RemoveEmptyEntries)
let takeChars n (str : string) = str.Substring(0, n)
let skipChars n (str : string) = str.Substring(n)
let toCharArray (str : string) = str.ToCharArray()
let toCharList = toCharArray >> List.ofArray
let fromCharList cs = System.String(cs |> List.toArray)
let nth n (str : string) = str.Chars n
let toLower (str : string) = str.ToLower()


