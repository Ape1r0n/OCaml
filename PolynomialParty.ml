let pow (x: float) (n: int) : float =
  let rec binpow (acc: float) (x: float) (n: int) : float =
    if n = 0 then acc
    else if n mod 2 = 0 then binpow acc (x *. x) (n / 2)
    else binpow (acc *. x) (x *. x) (n / 2)
  in
  if n < 0 then 1.0 /. (binpow 1.0 x (-n))
  else binpow 1.0 x n

let rec eval_poly x l = match l with [] -> 0.0 | h::t -> h *. (pow x (List.length t)) +. eval_poly x t

let rec float_length l = match l with [] -> 0.0 | h::t -> 1.0 +. float_length t
let rec derive_poly (l: float list) = match l with [] -> [] | x::[] -> [] | h::t -> (h *. (float_length t) ):: derive_poly t
