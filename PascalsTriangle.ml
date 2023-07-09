let pascals_triangle n = 
    let rec aux f l1 l2 =  match (l1, l2) with (x::xs, y::ys) -> f x y :: aux f xs ys | ([], ys) -> ys | (xs, []) -> xs in          
    let rec helper n acc = if n <= 0 then acc else match acc with (h::_) -> helper (n-1) (aux (+) (0::h) h::acc) | _ -> acc in 
    List.rev (helper (n - 1) [[1]])

(* I've been studying OCaml for 4 months, I still don't understand how input/output works on Hackerrank, so thanks, community *)
let print_strings   is = print_string (String.concat "\n" is)
let print_int_lists is = print_string (String.concat "\n" (List.map (String.concat " ") (List.map (List.map string_of_int) is))) 
let () = let n = int_of_string (read_line()) in print_int_lists (pascals_triangle n)
