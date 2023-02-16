(* Enter your code here. Read input from STDIN. Print output to STDOUT *)
open Core.Std
let pi = 3.1415926535

let calc_area_and_volume a0 b0 l r step =
    let rec loop area volume x = if x >= r then area, volume else
            let y = List.fold2_exn a0 b0 ~init:0.0 ~f:(fun sum a1 b1 -> sum +. a1 *. x ** b1) in
            let area' = y *. step in
            loop (area +. area') (volume +. pi *. y *. area') (x +. step)
    in
    loop 0.0 0.0 l

let () = let a0 = read_line () |> String.split ~on:' ' |> List.map ~f:Float.of_string in let b0 = read_line () |> String.split ~on:' ' |> List.map ~f:Float.of_string in Scanf.sscanf (read_line ()) "%f %f" (fun l r -> let area, volume = calc_area_and_volume a0 b0 l r 0.001 in printf "%f\n%f\n" area volume)
