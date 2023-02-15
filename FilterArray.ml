(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let f n arr = 
   let rec filter_arr n = function
        | [] -> []
        | h::t -> if h < n then h :: (filter_arr n t) else (filter_arr n t)
    in filter_arr n arr

let () =
    let n::arr = read_lines() in
    let ans = f n arr in
    List.iter (fun x -> print_int x; print_newline ()) ans
