type nat = Zero | Succ of nat

let rec int_to_nat x = if x<=0 then Zero else Succ (int_to_nat (x-1))
let rec nat_to_int x = match x with Zero -> 0 | Succ smth -> 1 + nat_to_int smth
let rec add x y = match x with Zero -> y | Succ n -> add n (Succ y)
let rec mul n1 n2 = match n1 with Zero -> Zero | Succ n -> add n2 (mul n n2)
let rec pow a b = match a with Zero -> Zero | ( match b with Zero -> Succ Zero | Succ x -> mul a (pow a x) )
let rec leq n1 n2 = match n1, n2 with Succ n_1, Succ n_2 -> leq n_1 n_2 | _ -> n1 = 0 
