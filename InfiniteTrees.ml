type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)
type 'a tree = Empty | Node of 'a * 'a tree * 'a tree


let rec layer_tree r = LNode (r, (fun () -> layer_tree (r+1)), (fun () -> layer_tree (r+1))) 

let rec interval_tree (l, h) = LNode ((l, h), (fun () -> interval_tree (l, (l +. h) /. 2.)), (fun () -> interval_tree ((l +. h) /. 2., h)))

let rational_tree () = let rec aux (n,d) = LNode ((n,d), (fun () -> aux (n, d+1)), (fun () -> aux (n+1, d))) in aux (0, 0)

let rec top n t = if n <= 0 then Empty else let LNode (v, l, r) = t in Node (v, top (n-1) (l ()), top (n-1) (r ()))

let rec map f t = let LNode (data, l, r) = t in LNode (f data, (fun () -> map f (l ())), (fun () -> map f (r ())))

let find f t = let rec aux lst = match lst with [] -> failwith "Kra-ka-ka-koom!" | h::t -> let LNode (data, l, r) = h in if f data then h else aux (List.rev (r () :: l () :: List.rev t)) in aux [t]
