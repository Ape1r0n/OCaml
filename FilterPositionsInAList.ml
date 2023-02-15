(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let f lst = let rec aux n = function | [] -> [] | h::t -> if n mod 2 == 0 then aux (n+1) t else [h] @ aux (n+1) t in aux 0 lst

let () = let lst = read_lines() in lst |> f |>List.iter (fun x -> print_int x; print_newline ()) ;;
