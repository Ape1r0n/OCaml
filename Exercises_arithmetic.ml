let rec pow a b = if b = 0 then 1 else if b mod 2 = 0 then let res = pow a (b / 2) in res * res else let res = pow a (b / 2) in a * res * res


(* Determine Whether a Given Integer Number Is Prime *)
let is_prime_naive n = let rec aux n i = if i < 2 then true else match n mod i with 0 -> false |  _ -> aux n (i-1) in aux n (n/2)

let is_prime n = let rec aux i = i * i > n || (n mod i <> 0 && aux (i + 1)) in n <> 1 && aux 2


(* Determine the Greatest Common Divisor of Two Positive Integer Numbers *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)


(* Determine Whether Two Positive Integer Numbers Are Coprime  *)
let coprime a b = gcd a b = 1


(* Calculate Euler's Totient Function Φ(m) *)
let phi n = let rec aux acc i = if i < n then aux (if coprime n i then (acc+1) else acc) (i+1) else acc in if n = 1 then 1 else aux 0 1


(* Determine the Prime Factors of a Given Positive Integer *)
let factors n = let rec aux n i = if n = 1 then [] else if n mod i = 0 then i :: aux (n/i) i else aux n (i+1) in aux n 2

let factors_2 n = let rec aux n i = if n = 1 then [] else if n mod i = 0 then match aux (n / i) i with (h, n) :: t when h = i -> (h, n + 1) :: t | l -> (i, 1) :: l else aux n (i + 1) in aux n 2


(* Calculate Euler's Totient Function Φ(m) (Improved) *)
let phi_improved n = let rec aux acc l = match l with [] -> acc | (p,m)::t -> aux ((p-1) * pow p (m-1) * acc) t in aux 1 (factors_2 n) 


(* A List of Prime Numbers *)
let rec all_primes a b = if a > b then [] else let t = all_primes (a+1) b in if is_prime a then a::t else t

(* Goldbach's Conjecture *)
let rec goldbach n = let rec aux i = if is_prime i && is_prime (n-i) then (i,n-i) else aux (i+1) in aux 2

(* A List of Goldbach Compositions *)
let rec goldbach_list a b = if a > b then [] else if a mod 2 = 1 then goldbach_list (a + 1) b else (a, goldbach a) :: goldbach_list (a + 2) b
