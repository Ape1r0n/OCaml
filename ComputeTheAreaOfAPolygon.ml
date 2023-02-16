(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
open Printf

let read_int () = Scanf.scanf "%d " (fun x -> x)
let triangle (x, y) (x', y') = x *. y' -. x' *. y

let rec sum_triangles v n i acc =
  if i = n - 1 then acc +. triangle v.(i) v.(0)
  else sum_triangles v n (i + 1) (acc +. triangle v.(i) v.(i + 1))

let main : unit =
  let n = read_int () in
  let v = Array.make n (0., 0.) in
  let rec read_points i =
    if i = n then ()
    else
      let x = read_int () in
      let y = read_int () in
      v.(i) <- (float x, float y);
      read_points (i + 1)
  in
  read_points 0;
  let s = sum_triangles v n 0 0.0 in
  printf "%.6f\n" (abs_float (s /. 2.))
