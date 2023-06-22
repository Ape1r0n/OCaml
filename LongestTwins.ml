(* This sequences overlap *)

let rec all_sequences l = match l with [] -> [] 
    | hd::tl -> 
            let rec sequence acc = function [] -> [] | h::t -> (List.rev (h :: acc)) :: sequence (h :: acc) t 
            in sequence [] l @ all_sequences tl

let lt_seq_overlapping lst =
    let sequences = all_sequences lst in
        let rec longest acc = function [] -> acc
            | h::t ->
                if List.mem h t then
                        if (List.length h) > (List.length acc) then longest h t
                        else longest acc t
                else longest acc t
    in longest [] sequences


(* This does not overlap *)

let lt_seq l =
    let rec longest_sequnce n l1 l2 =
        if n <= 0 then ([],0) else
            match (l1,l2) with
            |(h::t,hd::tl) -> if h <> hd then ([],0) else let (l,n) = longest_sequnce (n-1) t tl in (h::l,n+1) 
            | _ -> ([],0)
    in
    let rec aux1 n l1 l2 (a,b) = match l2 with [] -> (a,b) | hd::tl -> let (s1,s2) = longest_sequnce n l1 l2 in aux1 (n+1) l1 tl (if s2 > b then (s1,s2) else (a,b))
    in
    let rec aux2 l1 (a,b) = match l1 with [] -> a | h::t -> aux2 t (aux1 1 l1 t (a,b)) in aux2 l ([],0)
