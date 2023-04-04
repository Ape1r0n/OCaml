let rec interleave2 x y = match x with [] -> y | h::t -> h :: interleave2 t y;;
let rec interleave3 a b c = match a with [] -> interleave2 b c | h::t -> h :: interleave3 t b c;;
