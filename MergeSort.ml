let rec length lst = match lst with [] -> 0 | h::t -> 1 + length t

let rec daitrie n lst = if n = 0 then [] else match lst with [] -> [] | h::t -> h :: daitrie (n - 1) t;;

let rec daikide n lst = if n = 0 then lst else match lst with [] -> [] | h::t -> daikide (n - 1) t;;

let rec merge x y = match x, y with  lst, [] -> lst | [], lst -> lst | h_x::t_x, h_y::t_y ->  if h_x < h_y then h_x :: merge t_x (h_y :: t_y) else h_y :: merge (h_x :: t_x) t_y;;

let rec merge_sort lst = match lst with [] -> [] | [x] -> [x] | _ -> let l = daitrie (length lst/2) lst in let r = daikide (length lst/2) lst in merge (merge_sort l) (merge_sort r);;

(* ჩემი იმპლემენტაცია არ მოეწონათ და ამათებურად უნდა დავწერო*)

let init list = List.map (fun x -> [x]) list

let rec merge_kiu (t: 'a list * 'a list) : 'a list = match t with ([],[]) -> [] | (a,[]) -> a | ([],b) -> b | h::t, hd::tl -> if h < hd then h :: merge_kiu (t, hd::tl) else hd :: merge_kiu (h::t, tl);;

let rec merge_list (lst: 'a list list ) : 'a list list = match lst with [] -> [] | [x] -> [x] | l1::l2::ls -> merge_kiu (l1,l2) :: merge_list ls;;

let rec iter (l: 'a list list) : 'a list = match l with [] -> [] | [x] -> x | _ ->  iter (merge_list l);;

let sort list = iter (init list)
