let swap (x,y) = (y,x)
let ifCondition (x,y) = if x > y then swap (x,y) else (x,y)

let rec helper (x,y) b =
    if x >= y then x
    else
      let x' = if b then x + 1 else x in
      let y' = if not b then y - 1 else y in
      let b' = not b in
      helper (x',y') b'

let foo x y b = helper (ifCondition (x,y)) b
