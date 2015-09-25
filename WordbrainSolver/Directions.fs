module Directions

let SW = 0
let S = 1
let SE = 2
let W = 3
let E = 5
let NW = 6
let N = 7
let NE = 8

let vectorFromMove m =
    let x = (m / 3 - 1) * -1
    let y = m % 3 - 1
    x,y
let moves = [SW; S; SE; W; E; NW; N ; NE]
