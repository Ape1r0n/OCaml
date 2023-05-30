(* Following 4 lines are needed to test OCaml threads in utop *)
#thread;;
#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;

open Thread
open Event

type blog = string list
type user = string
type pass = string
type message = Post of user * pass * string | Read of user * blog channel
type t = message channel

let start_server (l : (user * pass) list) =
    let server = new_channel () in
    let rec loop (blogs : (user * blog) list) =
        let get_post user = match List.assoc_opt user blogs with None -> [] | Some b -> b in 
            match sync (receive server) with
                    | Post (u, p, txt) -> if List.assoc_opt u l = Some p then loop ((u, get_post u @ [txt]) :: List.remove_assoc u blogs) else loop blogs
                    | Read (u, reply) -> sync (send reply (get_post u)); loop blogs
        in
    let _ = create loop [] in server

let post (server : t) (u : user) (pswd : pass) (text: string) = sync (send server (Post (u, pswd, text)))

let read (server : t) (u : user) = let reply = new_channel () in sync (send server (Read (u, reply))); sync (receive reply)


let test =
    let s = start_server [("userA", "passA"); ("userB", "passB")] in
    post s "userB" "passB" "Welcome to my OCaml blog.";
    post s "userA" "passA" "My name is A and I'm starting my own blog!";
    post s "userB" "12345" "I am a hacker attacking B's blog now!";
    post s "userB" "passB" "You can have threads in OCaml!";
    read s "userB"
(* ["Welcome to my OCaml blog."; "You can have threads in OCaml!"] *)
