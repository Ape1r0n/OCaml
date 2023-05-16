(* Typedefs *)
type 'a custom_llist = (unit -> 'a custom_cell) and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)
type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)

(* Problem 1 *)
let f1 acc (x,y) = acc @ [(y,x)]

let f2 acc x = if (List.length acc) mod 2 = 0 then x::acc else acc @ [x] 

let f3 g (k, v) = fun element -> if element = k then v else g element


(* Problem 2 *)
let map_tr f l = let rec helper f acc l = match l with [] -> acc | h::t -> helper f ((f h) :: acc) t in List.rev (helper f [] l) 

let rec replicate_tr n x = let rec helper acc n x = match n > 0 with false -> acc | true -> helper (x::acc) (n-1) x in helper [] n x


(* Problem 3 *)
let rec map_over_custom_llist f l = match l () with NilC -> fun () -> NilC | ConsC (h, t) -> fun () -> ConsC (f h, map_over_custom_llist f t)

let rec map_over_ocaml_llist f l = match Lazy.force l with NilO -> lazy NilO | ConsO (h, t) -> lazy (ConsO (f h, map_over_ocaml_llist f t))


(* Problem 4 *)
let rec merge_custom_llists l1 l2 =
  match (l1 (), l2 ()) with (NilC, _) -> l2 | (_, NilC) -> l1
  | (ConsC (x1, lst1), ConsC (x2, lst2)) ->
    if x1 <= x2 then fun () -> ConsC (x1, merge_custom_llists lst1 l2)
    else fun () -> ConsC (x2, merge_custom_llists l1 lst2)

let rec merge_ocaml_llists l1 l2 = lazy (
  match Lazy.force l1, Lazy.force l2 with
  | (NilO, _) -> Lazy.force l2
  | (_, NilO) -> Lazy.force l1
  | ConsO (x1, lst1), ConsO (x2, lst2) ->
      if x1 <= x2 then ConsO (x1, merge_ocaml_llists lst1 l2)
      else ConsO (x2, merge_ocaml_llists l1 lst2))


(* Problem 5 *)
let rec drop_dupl_custom_llist l = match (l()) with
  | NilC -> (fun () -> NilC)
  | ConsC (h, t) ->
    let lst = drop_dupl_custom_llist t in
    match (lst ()) with
    | NilC -> (fun () -> ConsC (h, lst))
    | ConsC (x, _) -> if h = x then lst else (fun () -> ConsC (h, lst))

let rec drop_dupl_ocaml_llist l =
  match (Lazy.force l) with
  | NilO -> lazy NilO
  | ConsO (h, t) ->
    let lst = drop_dupl_ocaml_llist t in
    match (Lazy.force lst) with
    | NilO -> lazy (ConsO (h, lst))
    | ConsO (x, _) -> if h = x then lst else lazy (ConsO (h, lst))


(* Problem 6 *)
let rec ham_checker n = match (n mod 2 == 0 , n mod 3 == 0, n mod 5 == 0) with
    | false, false, false -> n == 1 
    | false, false, true -> ham_checker (n / 5)
    | false, true, _ -> ham_checker (n / 3) 
    | true, _, _ -> ham_checker (n / 2) 

let hamming_custom_llist = let rec helper n = if ham_checker n then fun () -> ConsC (n, helper (n + 1)) else helper (n + 1) in fun () -> helper 1

let hamming_ocaml_llist = let rec helper n = if ham_checker n then lazy (ConsO (n, helper (n + 1))) else helper (n + 1) in fun () -> helper 1
