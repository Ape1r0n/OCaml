let find1 x l = 
                let rec aux acc x l = match l with [] -> (None, (List.rev acc))
                        | (a,b)::t -> if x = a then (Some b, (a,b)::(List.rev acc @ t)) else aux ((a,b)::acc) x t
                in aux [] x l


type ('a, 'b) mfu = (int * ('a * 'b)) list

let init l = let rec aux acc l = match l with [] -> List.rev acc | h::t -> aux ((0,h)::acc) t in aux [] l

let find2 x l =
                let rec aux acc x l = match l with [] -> (None, List.rev acc)
                        | (c, (a, b)) :: t -> if x = a then (Some b, (c+1, (a, b)) :: (List.rev acc @ t)) else aux ((c, (a, b)) :: acc) x t
                in aux [] x l

let string_to_list s = let rec aux i acc = if i < 0 then acc else aux (i-1) (s.[i] :: acc) in aux (String.length s - 1) []

let is_balanced s = 
                    let rec aux i l = match l with [] -> if i = 0 then true else false
                                            | h::t -> if i < 0 then false else (if h = '(' then aux (i+1) t else if h = ')' then aux (i-1) t else aux i t)
                    in aux 0 (string_to_list s)


module type Iter = sig
    type 'a t
    type 'a s
    val init : 'a t -> 'a s
    val next : 'a s -> 'a option * 'a s
end

module ListIter : Iter = struct
    type 'a t = 'a list
    type 'a s = 'a list
    let init l = l
    let next = function [] -> (None, []) | h::t -> (Some h, t)
end

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

module TreeIter : Iter = struct
    type 'a t = 'a tree
    type 'a s = 'a tree list
    let rec init = function Leaf -> [] | Node (x, l, r) -> init l @ [Node (x, l, r)] @ init r
    let rec next = function [] -> (None, []) | Leaf::t -> next t | Node (x, l, r) :: t -> (Some x, init l @ init r @ t)
end

module ExtIter (I : Iter) = struct
    type 'a t = 'a I.t
    type 'a s = 'a I.s

    let init = I.init

    let rec next_filtered p state =
        match I.next state with
        | Some x, rest -> if p x then next_filtered p rest else (Some x, rest)
        | None, rest -> (None, rest)
    
    let next_mapped f state =
        let (v, rest) = I.next state in
            match v with
                | Some x -> (Some (f x), rest)
                | None -> (None, rest)
end
