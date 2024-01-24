type 'a lambda = Var of 'a | Fun of 'a * 'a lambda | App of 'a lambda * 'a lambda

let rec add_apostrophe x l = let rnm = x ^ "'" in if List.mem rnm l then add_apostrophe (rnm ^ "'") l else rnm

let rec free e = match e with
                | Var v -> v::[]
                | Fun (h, t) -> List.filter (fun x -> h <> x) (free t)
                | App (h, t) -> List.concat [free h; free t]

let rec used e = match e with
                | Var v -> v::[]
                | Fun (h, t) -> List.concat [used t; List.filter (fun x -> h <> x) (used t)]
                | App (h, t) -> List.concat [used h; used t]

let rec rename e a b = match e with
                      | Var v -> if v = a then Var b else Var v
                      | Fun (h, t) -> if h = a then Fun (b, rename t a b) else Fun (h, rename t a b)
                      | App (h, t) -> App (rename h a b, rename t a b)

let rec substitution_jutsu e a b = match e with
                                  | Var v -> if v = a then b else Var v
                                  | Fun (h, t) ->
                                      if h = a then Fun (h, t)
                                      else if List.mem h (free b) then
                                        let new_var = add_apostrophe h (used e) in
                                        Fun (new_var, substitution_jutsu (rename t h new_var) a b)
                                      else Fun (h, substitution_jutsu t a b)
                                  | App (h, t) -> App (substitution_jutsu h a b, substitution_jutsu t a b)

let rec beta e = match e with
                | Var v -> Var v
                | Fun (h, t) -> Fun (h, beta t)
                | App (Fun (h, t), fs) -> substitution_jutsu t h fs
                | App (h, t) -> App (beta h, beta t)

let rec normal_form test = let test' = beta test in if test' = test then test else normal_form test'


(* Tests *)
let test1 = App (Fun ("x", App (Var "x", Var "x")), Var "y")
let test2 = App (App (Fun ("x", Fun ("y", Var "x")), Var "y"), Var "z")
let test3 = App (Fun ("x", App (Fun ("y", App (Var "x", Var "y")), Var "z")), Var "l")
