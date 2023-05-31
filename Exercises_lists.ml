(* Tail of a List *)
let rec last l = match l with [] -> None | x::[] -> Some x | _::t -> last t


(* Last Two Elements of a List *)
let rec last_two l = match l with [] -> None | a::[] -> None | a::b::[] -> Some (a,b) | a::b::c -> last_two c


(* N'th Element of a List *)
let rec length l = match l  with [] -> 0 | x::xs -> 1 + length xs
let rec find l acc = match l with [] -> failwith "find: Element not found" | h::t -> if acc = 0 then h else find t (acc-1) 
let nth l index = if index >= length l then failwith "nth: index out of bounds" else find l index


(* Length of a List *)
let rec length l = match l  with [] -> 0 | x::xs -> 1 + length xs


(* Reverse a List *)
let rev l = 
    let rec rev_helper l acc = match l with [] -> acc | h::t -> rev_helper t (h :: acc)
in rev_helper l []


(* Palindrome *)
let rec palindrome_helper l1 l2 =
  match l1, l2 with | [], [] -> true
  | x::xs, y::ys ->
      if x = y then palindrome_helper xs ys
      else false
  | _,_ -> false

let palindrome l = palindrome_helper l (rev l)


(* Flatten a List *)
type 'a node = One of 'a | Many of 'a node list
let flatten l = let rec helper acc lst = match lst with [] -> acc | One h :: t -> helper (h::acc) t | Many h :: t -> helper (helper acc h) t in List.rev (helper [] l)


(* Eliminate Duplicates *)
let rec compress l = match l with [] -> [] | x::[] -> x::[] | a::b::c -> if a = b then compress (b::c) else a :: compress (b::c)


(* Pack Consecutive Duplicates *)
let pack l =
    let rec helper current acc lst = match lst with [] -> [] | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) -> if a = b then helper (a :: current) acc t else helper [] ((a :: current) :: acc) t  
    in List.rev (helper [] [] l);;


(* Run-Length Encoding *)
let encode l =
    let rec helper current acc lst = match lst with [] -> []
        | x :: [] -> (current+1, x) :: acc
        | a :: (b :: _ as t) -> if a = b then helper (current+1) acc t else helper 0 ((current+1,a)::acc) t 
    in List.rev (helper 0 [] l)


(* Modified Run-Length Encoding *)
type 'a rle = One of 'a | Many of int * 'a
let modified_encode lst =
    let rec helper counter acc l = match l with [] -> []
        | [x] -> if counter > 0 then Many (counter+1, x) :: acc else One x :: acc
        | x :: (y :: _ as t) -> if x = y then helper (counter+1) acc t
                                else if counter > 0 then helper 0 (Many (counter+1, x) :: acc) t
                                else helper 0 (One x :: acc) t
    in
    List.rev (helper 0 [] lst)


(* Decode a Run-Length Encoded List *)
let decode l =
    let rec aux acc n x = if n = 0 then acc else aux (x :: acc) (n - 1) x
    in
    let rec helper acc = function
      | [] -> acc
      | One x :: t -> helper (x :: acc) t
      | Many (n, x) :: t -> helper (aux acc n x) t
    in
    helper [] (List.rev l);;


(* Run-Length Encoding of a List (Direct Solution) *)
let direct_encode l =
    let aux counter x = if counter = 0 then One x else Many (counter + 1, x) in
    let rec helper count acc lst = match lst with [] -> [] | [x] -> aux count x :: acc
                                                | a :: (b :: _ as t) -> if a = b then helper (count + 1) acc t else helper 0 (aux count a :: acc) t
    in List.rev (helper 0 [] l);;


(* Duplicate the Elements of a List *)
let rec duplicate l = match l with [] -> [] | h::t ->  h::h::duplicate t


(* Replicate the Elements of a List a Given Number of Times *)
let replicate l n = 
    let rec aux counter acc e = if counter = 0 then acc else aux (counter-1) (e::acc) e in
    List.fold_left (aux n) [] (List.rev l)


(* Drop Every N'th Element From a List *)
let drop l n = let rec helper i lst = match lst with [] -> [] | h :: t -> if i = n then helper 1 t else h :: helper (i + 1) t  in helper 1 l;;


(* Split a List Into Two Parts; The Length of the First Part Is Given *)
let split lst n = let rec aux i acc l = match l with [] -> (List.rev acc, []) | h::t -> if i = 0 then (List.rev acc, (h::t)) else aux (i-1) (h::acc) t in aux n [] lst


(* Extract a Slice From a List *)
let slice lst ith kth = 
    let rec aux acc i k = function [] -> List.rev acc | h::t -> if i > 0 then aux acc (i-1) (k-1) t else if k >= 0 then aux (h::acc) 0 (k-1) t else List.rev acc
    in aux [] ith kth lst


(* Rotate a List N Places to the Left *)
let rotate lst n = let rec aux acc l k = match k with 0 -> l @ (List.rev acc) | x -> (match l with [] -> [] | h::t -> aux (h::acc) t (k-1)) in aux [] lst n


(* Remove the K'th Element From a List *)
let remove_at n lst = let rec aux acc k l = match l with [] -> List.rev acc | h::t -> if k = 0 then (List.rev acc) @ t else aux (h::acc) (k-1) t in aux [] n lst


(* Insert an Element at a Given Position Into a List *)
let rec insert_at e i l = match l with [] -> [] | h::t -> if i = 0 then e :: h :: t else h :: insert_at e (i-1) t


(* Create a List Containing All Integers Within a Given Range *)
let rec range i k = if i > k then range k i else if i = k then i::[] else i :: range (i+1) k


(* Extract a Given Number of Randomly Selected Elements From a List *)
let rand_select lst n =
    let rec helper acc n = function [] -> raise Not_found | h :: t -> if n = 0 then (h, acc @ t) else helper (h :: acc) (n - 1) t in
    let randomizer lst len = helper [] (Random.int len) lst in
    let rec aux n acc lst len = if n = 0 then acc else let a, b = randomizer lst len in aux (n - 1) (a :: acc) b (len - 1) in 
    aux (min n (List.length lst)) [] lst (List.length lst)


(* Lotto: Draw N Different Random Numbers From the Set 1..M *)
let lotto_select n m = rand_select (range 1 m) n


(* Generate a Random Permutation of the Elements of a List *)
let permutation l n =
    let rec helper acc n = function [] -> raise Not_found| h :: t -> if n = 0 then (h, acc @ t) else helper (h :: acc) (n - 1) t in
    let randomizer l len = helper [] (Random.int len) l in
    let rec aux n acc l len = if n = 0 then acc else
        let a, b = randomizer l len in aux (n - 1) (a :: acc) b (len - 1)
    in
    let l_length = List.length l in
    aux (min n l_length) [] l l_length


(* Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List *)
let rec extract k l = if k <= 0 then [[]] else match l with [] -> [] | h :: t -> List.map (fun e -> h :: e) (extract (k - 1) t) @ (extract k t)


(* Group the Elements of a Set Into Disjoint Subsets *)
let group l sizes =
    let start = List.map (fun size -> size, []) sizes in
    let prepend p l =
    let f l acc = l :: acc in
    let rec aux f acc = function [] -> f [] acc | (n, l) as h :: t -> let acc = if n > 0 then f ((n - 1, p :: l) :: t) acc else acc in aux (fun l acc -> f (h :: l) acc) acc t
    in aux f [] l in
    let rec aux = function [] -> [start] | h :: t -> List.concat_map (prepend h) (aux t)
    in List.map (List.map snd) (List.filter (List.for_all (fun (x, _) -> x = 0)) (aux l))


(* Sorting a List of Lists According to Length of Sublists *)
let length_sort l = List.sort (fun l1 l2 -> compare (List.length l1) (List.length l2)) l;;

let frequency_sort l =
  let f = List.fold_left (fun acc l -> let len = List.length l in let c = try List.assoc len acc with Not_found -> 0 in (len, c + 1) :: List.remove_assoc len acc) [] l
  in List.sort (fun l1 l2 -> let len1 = List.length l1 in let len2 = List.length l2 in let c1 = List.assoc len1 f in let c2 = List.assoc len2 f in 
    if c1 <> c2 then compare c1 c2 else compare len1 len2) l
