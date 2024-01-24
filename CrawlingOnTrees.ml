type tree = Empty | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop

let rec crawl cl tr = 
    match cl with [] -> tr
    | h::t ->  match h with Left -> (match tr with Empty -> failwith "Invalid Command" | Node (v,l,r) -> crawl t l)
                        | Right -> (match tr with Empty -> failwith "Invalid Command" | Node (v,l,r) -> crawl t r)
                        | Up -> failwith "todo"
                        | New x -> crawl t (Node (x, Empty, Empty))
                        | Delete -> crawl t Empty
                        | Push -> crawl t tr
                        | Pop -> (match tr with Empty -> failwith "Invalid Command" | Node (v,l,r) -> crawl t (Node (14000605, Empty, Empty)))


let rec remove x tr = 
    match tr with Empty -> failwith "Nothing to remove"
                | Node (v,l,r) -> if v = x then Empty  
                                  else if x < v then Node (v, remove x l, r)
                                  else Node (v, l, remove x r)

let crawl cl tr =
        let rec aux stack cl tr = match tr with
            | [] -> tr
            | hd :: tl -> match hd with
                            | Left -> (match tr with Empty -> failwith "Invalid Command" | Node (v , l, r) -> aux stack tl l)
                            | Right -> (match tr with Empty -> failwith "Invalid Command" | Node (v, l, r) -> aux stack tl r)
                            | Up -> failwith "TODO"
                            | New x -> aux stack tl (Node (x, Empty, Empty)
                            | Delete -> aux stack tl Empty
                            | Push -> aux (Stack.push tr stack) tl tr
                            | Pop ->let stack' = (Stack.peek stack) in aux (Stack.pop tr stack) tl stack'
        in aux (Stack.create ()) cl tr
