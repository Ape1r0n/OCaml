(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
let e_power_x x n = 
    let rec power n p = if n = 0 then 1.0 else p *. power (n-1) p in 
    let rec factorial n = if n = 0 then 1.0 else (float_of_int n) *. factorial (n-1) in
    let rec term x n = (power n x) /. (factorial n) in
    let rec sum x n = if n = 0 then 1.0 else (term x n) +. (sum x (n-1)) in
    sum x n;;

let () =
    let t = read_int () in
    for i = 1 to t do
        let x = read_float () in
        let result = e_power_x x 9 in
        Printf.printf "%.4f\n" result
    done;;
