let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let rec repeat n x =
  match n with
  | 0 -> []
  | _ -> x :: repeat (n-1) x

let rec the_great_repetition n arr =
  match arr with
  | [] -> []
  | h :: t -> repeat n h @ the_great_repetition n t

let f n arr = the_great_repetition n arr
  
let () =
    let n::arr = read_lines() in
    let ans = f n arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;;
