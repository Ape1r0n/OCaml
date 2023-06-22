(* Input shit I copied from HackerRank *)
let _ = read_line ()
let rec liste () =
  try let line = read_line () in
      let liste_line = String.split_on_char ' ' line in
      match liste_line with
      | [a;b] -> (int_of_string a, int_of_string b) :: liste ()
      | _ -> assert false
  with _ -> []
let l = liste ()

let compare (x0, y1) (x1, y2) = y2 - y1
let scal (x0, y1) (x1, y2) = x0 * x1 + y1 * y2
let rotate_left (x, y) = (-y, x)
let substract (x0, y1) (x1, y2) = x0 - x1 , y1 - y2

                         
let rec hull lh rh l =
    match l with
    | [] -> lh @ List.rev rh
    | next :: rest ->
         hull (place_left next lh) (place_right next rh) rest

and place_right p l = match l with [] -> assert false
    | [_] -> p::l
    | x0::x1::t -> if orientation p x1 x0 >= 0 then place_right p (x1::t) else p::l

and orientation p x1 x0 =
    let direction = rotate_left (substract x0 x1) in
    let vector = substract p x1 in
    scal direction vector

and place_left p l = match l with [] -> assert false | [_] -> p::l
    | x0::x1::t -> if orientation p x1 x0 <= 0 then place_left p (x1::t) else p::l
               
let compute_hull l = let l = List.sort compare l in match l with  [] -> assert false | h::t -> hull [h] [h] t

let dist (x0,y1) (x1,y2) = sqrt ( (float x0 -. float x1)**2. +. (float y1 -. float y2)**2.)   
                 
let perimeter l = match l with [] -> assert false
    | h::t -> let l = l @ [h] in
            let rec aux l acc = match l with
              | [] | [_] -> acc
              | x0::x1::t -> aux (x1::t) (acc +. dist x0 x1) in
            aux l 0.
                
                
(* Output shit I copied from HackerRank *)
let _ = print_float (0.01 +. (perimeter (compute_hull l)))  
