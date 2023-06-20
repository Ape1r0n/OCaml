(* Gray Code exercise from official OCaml  website *)
let gray n =
  let rec aux i l =
    if i < n then
      let (h, t) = List.fold_left (fun (acc1, acc2) k -> (("0" ^ k) :: acc1, ("1" ^ k) :: acc2)) ([], []) l
      in
      let new_list = List.append (List.rev h) t in aux (i + 1) new_list
    else l
  in aux 1 ["0"; "1"]


(* One of the previous final exam assignments: Streams Of Dreams*)
type 'a pair = Null | Pair of 'a * 'a stream
and 'a stream = unit -> 'a pair

let rec stream_of_list (l : 'a list) () : 'a stream = match l with [] -> fun () -> Null | h::t -> fun () -> Pair (h, stream_of_list t ())

let rec stream_of_fun f = let h = f 0 in let t = stream_of_fun (fun n -> f (n + 1)) in Pair (h, fun () -> t )

let rec map f s = fun () -> match s () with Null -> Null | Pair (h, t) -> Pair (f h, map f t)

let rec concat s1 s2 () = fun () -> match s1 () with Null -> s2 () | Pair (h, t) -> Pair (h, concat t s2 ())

let rec reduce f acc s = match s () with Null -> acc | Pair (h, t) -> reduce f (f acc h) t

let avg l f = if l = [] then failwith "empty list" else (reduce (fun acc x -> acc +. f x) 0.0 (map f (stream_of_list l ()))) /. (reduce (fun acc _ -> acc +. 1.0) 0.0 (map f (stream_of_list l ())))


(* Testing *)
let rec print_stream n stream = if n = 0 then () else match stream () with Null -> () | Pair (h, t) -> print_int h; print_string " "; print_stream (n - 1) t;;
