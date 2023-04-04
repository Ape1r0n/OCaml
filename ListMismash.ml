let rec interleave2 x y = match x with [] -> y | h::t -> h :: interleave2 t y;;
let rec interleave3 a b c = match a with [] -> interleave2 b c | h::t -> h :: interleave3 t b c;;

let rec interleave3_oneliner a b c = match a with [] -> (match b with [] -> c | hd::tl -> hd :: interleave3_oneliner [] tl c) | h::t -> h :: interleave3_oneliner t b c;;
