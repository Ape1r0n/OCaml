module type StackSig = sig
    type 'a stack
    val create_stack : unit -> 'a stack
    val push : 'a -> 'a stack -> 'a stack
    val pop : 'a stack -> ('a * 'a stack) option
    val is_empty : 'a stack -> bool
end

module ListStack : StackSig = struct
    type 'a stack = 'a list
    let create_stack () = []
    let push x st = x :: st
    let pop = function [] -> None | h::t -> Some (h,t)
    let is_empty st = st = []
end


module type QueueSig = sig
    type 'a queue
    val create_queue : unit -> 'a queue
    val enqueue : 'a -> 'a queue -> 'a queue
    val dequeue : 'a queue -> ('a * 'a queue) option
    val is_empty : 'a queue -> bool
end

module ListQueue : QueueSig = struct
    type 'a queue = 'a list * 'a list
    let create_queue () = ([], [])
    let enqueue x (h, t) = (h, x :: t)
    let rec dequeue = function ([],[]) -> None | (x::h,t) -> Some (x, (h,t)) | ([],t) -> dequeue (List.rev t, [])
    let is_empty (h,t) = h = [] && t = []
end


module type SetSig = sig
    type 'a set
    val empty_set : unit -> 'a set
    val add : 'a -> 'a set -> 'a set
    val remove : 'a -> 'a set -> 'a set
    val is_empty : 'a set -> bool
    val contains : 'a -> 'a set -> bool
end

module ListSet = struct
    type 'a set = 'a list
    let empty_set () = []
    let contains x set = List.mem x set
    let add x set = if contains x set then set else (x::set)
    let rec remove x set = if not (contains x set) then set else match set with [] -> [] | h::t -> if h = x then remove x t else h :: remove x t
    let is_empty set = set = []
end


module type MapSig = sig
    type ('a, 'b) map
    val empty_map : unit -> ('a, 'b) map
    val add_pair : 'a -> 'b -> ('a, 'b) map -> ('a, 'b) map
    val remove_key : 'a -> ('a, 'b) map -> ('a, 'b) map
    val is_empty : ('a, 'b) map -> bool
    val find_value : 'a -> ('a, 'b) map -> 'b option
end

module ListMap : MapSig = struct
    type ('a, 'b) map = ('a * 'b) list
    let empty_map () = []
    let rec add_pair key value map = match map with [] -> [(key, value)] | (a, b) :: t -> if a = key then (a,b) :: t else (a,b) :: add_pair key value t
    let rec remove_key key map = match map with [] -> [] | (a,b) :: t -> if a = key then t else (a,b) :: remove_key key t
    let is_empty map = map = []
    let rec find_value key map = match map with [] -> None | (a,b) :: t -> if a = key then Some b else find_value key t
end
