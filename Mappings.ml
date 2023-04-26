let is_empty l = match List.length l with 0 -> true | x -> false

let rec get k l = match l with [] -> None | (k', v)::t -> if k = k' then Some v else get k t

let rec put k v l = match l with [] -> [(k, v)] | (k', v')::t -> if k = k' then (k, v)::t else (k', v')::(put k v t)

let contains_key k l = List.exists (fun (k', _) -> k = k') l

let rec remove k l = match l with [] -> [] | (k', v)::t -> if k = k' then remove k t else (k', v)::(remove k t)

let rec keys l = match l with [] -> [] | (k, _)::t -> k::(keys t)

let rec values l = match l with [] -> [] | (_, v)::t -> v::(values t)

let put_Medea_Version k v lst = (k,v)::(remove k lst)

(* An alternative to associative lists is to use functions of type 'k -> 'v option directly. So for example, the function fun x -> x * x + 1 respresents a very efficient mapping from any number to the successor of its square *)

type ('k, 'v) mapping = 'k -> 'v option

(* is_empty, keys and values can't be written in mappings based approach *)

let get k m = m k

let put k v m = fun k' -> if k = k' then Some v else m k'

let contains_key k m = m k <> None

let remove k m = fun k' -> if k = k' then None else m k'
