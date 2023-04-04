let rec interleave2 x y = match x with [] -> y | h::t -> h :: interleave2 y t
let rec interleave3 a b c = match a with [] -> interleave2 b c | h::t -> h :: interleave3 b c t

let rec interleave3_oneliner a b c = match a with [] -> (match b with [] -> c | hd::tl -> hd :: interleave3_oneliner [] c tl) | h::t -> h :: interleave3_oneliner b c t
