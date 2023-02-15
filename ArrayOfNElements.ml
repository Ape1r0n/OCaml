(* returns an array of n elements *)
let make_array n = let rec f x = function | 0 -> x | n -> [n] @ f x (n - 1) in f [] n

let () =
    let n = int_of_string (read_line ()) in
    let arr = make_array n in
    List.iter ( Printf.printf "%d " ) arr
