(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec listLength lst = match lst with | [] -> 0 | h :: t -> 1 + listLength t;;

let len arr = listLength arr;;

let rec read_lines () =
  try let line = read_line () in
      int_of_string (line) :: read_lines()
  with
      End_of_file -> []

let () =
  let arr = read_lines () in
  print_int (len arr);
  print_newline ();;
