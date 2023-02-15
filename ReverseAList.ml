(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let reverseList lst = let rec rev = function | [] -> [] | [e] -> [e] | h::t -> (rev t) @ [h] in rev lst

let () = let lst = read_lines() in lst |> reverseList |> List.iter (fun x -> print_int x; print_newline ());;
