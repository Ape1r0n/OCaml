module type Set = sig
    type t
    val to_string : t -> string
end


module type Map = sig
    type key
    type value
    type t
    val empty : t
    val set : key -> value -> t -> t
    val get : key -> t -> value
    val get_opt : key -> t -> value option
    val to_string : t -> string
end


module StringSet : Set with type t = string = struct 
    type t  = string
    let to_string s = "\"" ^ s ^ "\""
end


module type OrderedSet = sig
    include Set
    val compare : t -> t -> int
end


module BTreeMap (K : OrderedSet) (V: Set)  : Map with type key = K.t and type value = V.t = struct
    type key = K.t
    type value = V.t
    type t = Empty | Node of key * value * t * t

    let empty = Empty

    let rec set k v tree = match tree with Empty -> Node (k, v, Empty, Empty)
        | Node (k', v', left, right) ->
            if K.compare k k' = 0 then Node (k', v, left, right) 
            else if K.compare k k' > 0 then Node (k', v', left, set k v right) 
            else Node (k', v', set k v left, right)

    let rec get k tree = match tree with Empty -> raise Not_found
        | Node (k', v', left, right) ->
            if K.compare k k' = 0 then v'
            else if K.compare k k' > 0 then get k right
            else get k left

    let rec get_opt k tree = match tree with Empty -> None
        | Node (k', v', left, right) ->
            if K.compare k k' = 0 then Some v'
            else if K.compare k k' > 0 then get_opt k right
            else get_opt k left


    let rec to_string tree = match tree with Empty -> "" 
        | Node (k, v, left, right) -> 
                "{" ^ K.to_string k ^ ", " ^ V.to_string v ^ "}" ^ (if left = Empty && right = Empty then "" else " " ^ to_string left ^ " " ^ to_string right)

end
