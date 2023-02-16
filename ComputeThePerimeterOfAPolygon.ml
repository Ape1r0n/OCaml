(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
open Printf

let read_int () = Scanf.scanf "%d " (fun x -> x)

let sqr x = x *. x

let dist (x, y) (x', y') = sqrt (sqr (x -. x') +. sqr (y -. y'))

let rec calc_distance v n i acc =
  if i >= n-1 then acc
  else let acc' = acc +. dist v.(i) v.((i+1) mod n) in calc_distance v n (i+1) acc'

let main : unit =
  let n = read_int () in
  let v = Array.make n (0., 0.) in
  for i = 0 to n-1 do
    let x = read_int () in
    let y = read_int () in
    v.(i) <- (float x, float y);
  done;
  let s = ref (dist v.(n-1) v.(0)) in
  s := calc_distance v n 0 !s;
  printf "%.6f\n" !s
