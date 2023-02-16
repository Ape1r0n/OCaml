(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec f lst = match lst with | [] -> 0 | h :: t -> if h mod 2 <> 0 then h + f t else f t;; 

let rec read_lines acc =
  try
    let line = read_line () in
    let x = int_of_string line in
    read_lines (x :: acc)
  with
    End_of_file -> List.rev acc;;

let () =
  let lst = read_lines [] in
  let result = f lst in
  print_int result;
  print_newline ();;
