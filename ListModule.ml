(* List Module Part 1 *)
let hd l = match l with [] -> failwith "invalid" | h::t -> h

let tl l = match l with [] -> failwith "invalid" | h::t -> t

let rec length l = match l with [] -> 0 | h::t -> 1 + length t

let rec append l1 l2 = match l1 with [] -> l2 | h::t -> h :: (append t l2);;

let rev l = 
    let rec helper acc lst = match lst with [] -> acc | h::t -> helper (h::acc) t
in helper l []

let rec find l acc = match l with [] -> raise (Invalid_argument "invalid") | h::t -> if acc = 0 then h else find t (acc-1) 
let nth l index = if index >= length l then failwith "invalid" else find l index


(* List Module Part 2 *)
let squaresum l = List.fold_left (fun acc x -> (x * x) + acc ) 0 l

let float_list l = List.map float_of_int l

let to_string l = match l with
  | [] -> "[]"
  | hd::tl -> List.fold_left (fun acc x -> acc ^ "; " ^ string_of_int x) ("[" ^ string_of_int hd) tl ^ "]"

let part_even l =
  let evens = List.filter (fun n -> n mod 2 = 0) l in
  let odds = List.fold_left (fun acc n -> if n mod 2 = 1 then n::acc else acc) [] l in
  List.fold_right (fun n acc -> n::acc) evens odds

