type 'a llist = Cons of 'a  * (unit -> 'a llist)

let rec lnat n = Cons (n, fun () -> lnat (n + 1))

let lfib () =
  let rec lfib' a b = Cons (a, fun () -> lfib' b (a+b)) in lfib' 0 1

let rec ltake n (Cons (h, tl)) = if n = 0 then [] else h :: ltake (n-1) (tl ())

let rec lfilter p (Cons (h, tl)) =
  if p h then Cons (h, fun () -> lfilter p (tl ()))
  else lfilter p (tl ())
