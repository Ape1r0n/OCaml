(* For utop:
#thread;;
#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;
 *)


module Future = struct open Thread open Event
    type 'a msg = Result of 'a | Ex of exn
    type 'a t = 'a msg channel

    let create f a = 
        let ch = new_channel () in
            let task () = 
                let r = try (Result (f a)) with e -> Ex e
        in sync (send ch r)
        in
        let _ = create task () in ch
    
    let get c = match sync (receive c) with Result r -> r | Ex e -> raise e

    let then_ f x =
        let c = new_channel () in
        let task () =
            let r = match sync (receive x) with
                | Result r -> Result (f r)
                | Ex e -> Ex e
        in
        sync (send c r)
        in
        let _ = create task () in
        c

    let when_any l =
        let c = new_channel () in
        let task () =
            let r = select (List.map receive l) in
              sync (send c r)
        in
        let _ = create task () in
        c

    let when_all l =
        let ch = new_channel () in
        let tast () =
            let r = List.fold_left (fun acc c -> sync (receive c) :: acc) [] l |> List.rev in
            match List.find_opt (function (Ex e) -> true | _ -> false) r with
                | Some (Ex e) -> sync (send ch (Ex e))
                | _ -> sync (send ch (Result (List.map (function Result r -> r | _ -> failwith "unreachable") r))) 
        inq
        let _ create task () in
        ch

end
