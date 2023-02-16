(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
open Printf

let rec read_n_ints n acc =
  if n = 0 then List.rev acc
  else read_n_ints (n - 1) (read_int () :: acc)
and read_int () = Scanf.scanf "%d " (fun x -> x)

let rec check_pairs n v i =
  if i = n then true
  else
    let x = read_int () in
    let y = read_int () in
    if v.(x) = -1 then (
      v.(x) <- y;
      check_pairs n v (i + 1)
    ) else if v.(x) = y then (
      check_pairs n v (i + 1)
    ) else false

let rec handle_case t =
  if t = 0 then ()
  else
    let n = read_int () in
    let v = Array.make 501 (-1) in
    let ok = check_pairs n v 0 in
    printf "%s\n" (if ok then "YES" else "NO");
    handle_case (t - 1)

let () = handle_case (read_int ())
