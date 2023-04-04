let rec length lst = match lst with [] -> 0 | h::t -> 1 + length t

let rec daitrie n lst = if n = 0 then [] else match lst with [] -> [] | h::t -> h :: daitrie (n - 1) t;;

let rec daikide n lst = if n = 0 then lst else match lst with [] -> [] | h::t -> daikide (n - 1) t;;

let rec merge x y = match x, y with  lst, [] -> lst | [], lst -> lst | h_x::t_x, h_y::t_y ->  if h_x < h_y then h_x :: merge t_x (h_y :: t_y) else h_y :: merge (h_x :: t_x) t_y;;

let rec merge_sort lst = match lst with [] -> [] | [x] -> [x] | _ -> let l = daitrie (length lst/2) lst in let r = daikide (length lst/2) lst in merge (merge_sort l) (merge_sort r);;
