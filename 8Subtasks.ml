(* 1 *)
let rec member c t l = match l with [] -> false | x::xs -> if c t x = 0 then true else member c t xs

(* 2 *)
let rec filter p = function | [] -> [] | h :: t -> if p h then h :: filter p t else filter p t
let rec single_count e = function | [] -> 0 | h::t -> if h = e then 1 + single_count e t else single_count e t
let count_comparison (e1, c1) (e2, c2) = match compare c2 c1 with 0 -> compare e1 e2 | c -> c
let filter_predicate x y = y <> x

let count_occurrences l =
  let rec helper list1 list2 = match list1 with [] -> List.sort count_comparison list2
    | h :: t -> let please_be_final_count = single_count h l in
        let list2' = (h, please_be_final_count) :: list2 in
        let list1' = filter (filter_predicate h) list1 in
        helper list1' list2' in
  helper l []


(* 3 *)
let rec drop_last l = match l with [] -> failwith "Empty list has no last element" | element::[] -> [] | x::xs -> x :: (drop_last xs)

(* 4 *)
let rec drop_last_opt l = match l with [] -> None | [x] -> Some [] | h::t ->( match drop_last_opt t with | Some x -> Some (h :: x) | None -> None )

(* 5 *)
let rec zip_with f list1 list2 = match (list1, list2) with ([],[]) -> [] | ([], _) | (_, []) -> [] | (l1 :: l1s, l2 :: l2s) -> (f l1 l2) :: (zip_with f l1s l2s)

(* 6 *)
let rec fold_right f l acc = match l with | [] -> acc | h::t -> f h (fold_right f t acc)
let unzip lst = let f (a,b) (list1,list2) = (a::list1, b::list2) in fold_right f lst ([],[])

(* 7 *)
(*
 fold_right f l c -> takes a binary function "f", list "l", and accumulator "acc", and applies "f" to the elements of the list. It does this in reverse order(as opposed to fold_left's, well... straight/normal order) with "acc"(which gets updated every time). Final value of "acc" is returned.

 unzip lst -> takes list of pairs and intends to separate pairs into two list, one with first values of pairs and second with other values of pairs.

Start:
lst = [('m',0);('e',1);('d',2);('e',3);('a',4)]
acc = ([], [])

Iteration 1:
h = ('a',4)
t = [('e',3);('d',2);('e',1);('m',0)]
f h acc = (['a'], [4])
fold_right f t (['a'], [4]) = (['e';'d';'e';'m'], [3;2;1;0])

Iteration 2:
h = ('e',3)
t = [('d',2);('e',1);('m',0)]
f h acc = (['e';'a'], [3;4])
fold_right f t (['e';'a'], [3;4]) = (['d';'e';'m'], [2;1;0])

Iteration 3:
h = ('d',2)
t = [('e',1);('m',0)]
f h acc = (['d';'e';'a'], [2;3;4])
fold_right f t (['d';'e';'a'], [2;3;4]) = (['e';'m'], [1;0])

Iteration 4:
h = ('e',1)
t = [('m',0)]
f h acc = (['e';'d';'e';'a'], [1;2;3;4])
fold_right f t (['e';'d';'e';'a'], [1;2;3;4]) = (['m'], [0])

Iteration 5:
h = ('m',0)
t = []
f h acc = (['m';'e';'d';'e';'a'], [0;1;2;3;4])
fold_right f t (['m';'e';'d';'e';'a'], [0;1;2;3;4]) = (['m';'e';'d';'e';'a'], [0;1;2;3;4])

Result -> (['m';'e';'d';'e';'a'], [0;1;2;3;4])
*)

(* 8 *)
type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha | NONE

(* Basic functions, which are probably more efficient in List library*) 
let rec length l = match l with [] -> 0 | h::t -> 1 + length t 
let rec filter p = function | [] -> [] | h :: t -> if p h then h :: filter p t else filter p t
let rec fold_left f acc lst = match lst with | [] -> acc | h :: t -> fold_left f (f acc h) t 
let rec map f = function | [] -> [] | h :: t -> f h :: map f t


(* Auxiliary methods for getting (t,g,w,d,l,gf,ga,p) tuple in (a) *)
let countries l = 
  let nations = fold_left (fun acc (t1, _, t2, _) -> t1 :: t2 :: acc) [] l in
  let rec unique lst = match lst with| [] -> [] | h :: t -> h :: unique (filter (fun x -> x <> h) t)
  in unique nations

let rec game_count country l = match l with [] -> 0 | (t1,_,t2,_)::tl -> if t1 = country || t2 = country then 1 + game_count country tl else game_count country tl

let rec win_count country l = match l with [] -> 0 | (t1,g1,t2,g2)::tl -> if t1 = country && length g1 > length g2 then 1 + win_count country tl else if t2 = country && length g2 >  length g1 then 1 + win_count country tl else win_count country tl

let rec draw_count country l = match l with [] -> 0 | (t1,g1,t2,g2)::tl -> if t1 = country && length g1 = length g2 then 1 + draw_count country tl else if t2 = country && length g2 =  length g1 then 1 + draw_count country tl else draw_count country tl

let rec loss_count country l = match l with [] -> 0 | (t1,g1,t2,g2)::tl -> if t1 = country && length g1 < length g2 then 1 + loss_count country tl else if t2 = country && length g2 <  length g1 then 1 + loss_count country tl else loss_count country tl

let rec scored_goals_count country l = match l with [] -> 0 | (t1,g1,t2,g2)::tl -> if t1 = country then length g1 + scored_goals_count country tl else if t2 = country then length g2 + scored_goals_count country tl else scored_goals_count country tl

let rec conseded_goals_count country l = match l with [] -> 0 | (t1,g1,t2,g2)::tl -> if t1 = country then length g2 + conseded_goals_count country tl else if t2 = country then length g1 + conseded_goals_count country tl else conseded_goals_count country tl

let points country l = 3 * (win_count country l) + (draw_count country l)

(* Actually getting the tuple *)
let get_big_tuple country  l = (country, game_count country l, win_count country l, draw_count country l, loss_count country l, scored_goals_count country l, conseded_goals_count country l, points country l) 

(* Comparing tuples *)
let country_comparator (t1, g1, w1, d1, l1, gf1, ga1, p1) (t2, g2, w2, d2, l2, gf2, ga2, p2) =
  match compare p2 p1 with
  | 0 -> (match compare (gf2 - ga2) (gf1 - ga1) with
          | 0 -> (match compare gf2 gf1 with
                  | 0 -> if Random.bool () then 1 else -1
                  | c -> c)
          | c -> c)
  | c -> c

(* Clustering countries as tuples *)
let unordered_country_list l = let country_names = countries l in map (fun x -> get_big_tuple x l) country_names
(* Sorting... *)
let sorted_country_list l = List.sort country_comparator (unordered_country_list l)
(* (a) done ! *)

(* Here we go with (b) *)
let count_goals player l = let goals = List.concat_map (fun (_, g1, _, g2) -> List.concat [g1; g2]) l in let num_goals = length (filter (fun p -> p = player) goals) in num_goals


let rec is_in_list player l = match l with [] -> false | h::t -> if h = player then true else is_in_list player t

let rec find_country player l = match l with [] -> failwith "Not Found" | (t1,g1,t2,g2)::tl -> if is_in_list player g1 then t1 else if is_in_list player g2 then t2 else find_country player tl
(* Contains duplicates *)
let rec get_all_players l = match l with [] -> [] | (t1,g1,t2,g2)::tl -> List.concat [g1;g2] @ get_all_players tl

(* Doesn't contain duplicates *)
let rec unique_players l = match l with  [] -> [] | h::t -> h :: unique_players (filter (fun x -> x <> h) t)

(* Getting triplets *)
let rec get_tuples player_list l = match player_list with [] -> [] | p::ps -> (p, find_country p l, count_goals p l)::get_tuples ps l

let rec unordered_player_list l = get_tuples (unique_players (get_all_players l)) l

(* Player comparator *)
let player_comparator (p1,_,g1) (p2,_,g2) = if g1 > g2 then -1 else if g1 < g2 then 1 else String.compare p1 p2

(* We are almost done *)
let sorted_player_list l = List.sort player_comparator (unordered_player_list l) 
(* (b) done! *)

(* Now is the time for final list *)
let table_and_scorers l = (sorted_country_list l, sorted_player_list l)
