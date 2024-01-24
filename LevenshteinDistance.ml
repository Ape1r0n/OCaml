let len s = String.length s

let rec reverse_string s = match s with "" -> "" 
    | _ -> (reverse_string (String.sub s 1 (len s - 1))) ^ (String.sub s 0 1)

let head s = match s with "" -> "" | _ -> String.sub s 0 1
let tail s = reverse_string (String.sub (reverse_string s) 0 (len s - 1))

let min2 a b = if a < b then a else b

let min a b c = 
    if min2 a b = a && min2 a c = a then a 
    else if min2 a b = a && min2 a c = c then c 
    else if min2 a b = b && min2 a c = a then b 
    else min2 b c

let rec lev a b = 
    if len b = 0 then len a
    else if len a = 0 then len b
    else if head a = head b then lev (tail a) (tail b)
    else 1 + min (lev (tail a) b) (lev a (tail b)) (lev (tail a) (tail b))
