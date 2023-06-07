#thread;;
#directory "+threads";;
#load "unix.cma";;
#load "threads.cma";;

module type RW = sig
    type ack
    type rwlock

    val acquire_read  : rwlock -> ack
    val acquire_write : rwlock -> ack
    val release       : ack -> unit
    val new_rwlock    : unit -> rwlock
end

module RW : RW = struct open Thread open Event
    type ack = unit channel
    type rwlock = ack channel * ack channel

    let acquire_read (rch,_) = let ack = new_channel () in (send rch ack); ack
    
    let acquire_write (_,wch) = let ack = new_channel () in (send wch ack); ack    

    let release ack = sync (receive ack); ()

    let new_rwlock () =
        let rch = new_channel () in
        let wch = new_channel () in
        let release_ch = new_channel () in
        let rec serv x = 
            if x = 0 then select [
                wrap (receive wch) (fun ch -> sync (receive ch); serv 0);
                wrap (receive rch) (fun ch -> let _ = create read_serv ch in serv 1)
            ] else select [
                wrap (receive rch) (fun ch -> let _ = create read_serv ch in serv (x+1));
                wrap (receive release_ch) (fun () -> serv (x-1))
            ]
        and read_serv c = sync (receive c); sync (send release_ch ())
        in let _ = create serv 0 in
        (rch, wch)
end
