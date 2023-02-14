(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec solution n = if n <= 0 then () else( print_endline "Hello World"; solution (n-1) );;
let () = let n = read_int() in solution n;;
