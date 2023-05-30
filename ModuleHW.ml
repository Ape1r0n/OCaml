module type Readable = sig
    type t
    type arg
    val begin_read : arg -> t
    val end_read : t -> unit
    val at_end : t -> bool
    val read_line : t -> (t * string)
end


module ReadableString : Readable with type arg = string = struct
    type t = string
    type arg = string
    
    let begin_read input = input
    let end_read _ = ()
    let at_end input = String.length input = 0
    let read_line input =
      let lines = String.split_on_char '\n' input in
      match lines with [] -> (input, "")
      | h::t -> let remainder = String.concat "\n" t in (remainder, h)
end


module ReadableFile : Readable with type arg = string = struct
    type t = in_channel
    type arg = string
    
    let begin_read filename = open_in filename
    let end_read channel = close_in channel
    let at_end channel = pos_in channel >= in_channel_length channel
    let read_line channel = match input_line channel with not_end -> (channel, not_end)
        | exception End_of_file -> close_in channel; raise End_of_file
end


module type Reader = sig
    include Readable
    val read_all : t -> t * string
end

module Reader (R : Readable) : Reader with type t = R.t and type arg = R.arg = struct
    include R

    let rec read_all_helper (input, acc) =
        if at_end input then (input, acc)
        else let (next_input, line) = read_line input in read_all_helper (next_input, acc ^ line ^ "\n")

    let read_all input =
        let rec remove_last_char str =
            let length = String.length str in
                if length <= 1 then "" else String.sub str 0 (length - 1)
            in
        let rec read_all_helper (input, acc) =
            if at_end input then (input, remove_last_char acc) 
            else 
                let (next_input, line) = read_line input in read_all_helper (next_input, acc ^ line ^ "\n")
            in read_all_helper (input, "")
end

(*
Example:
let rs = ReadableString.begin_read "A multiline\ntext"
let e = ReadableString.at_end rs (* e = false *)
let rs,l1 = ReadableString.read_line rs (* l1 = "A multiline" *) 
let rs,l2 = ReadableString.read_line rs (* 12 = "text" *)
let e = ReadableString.at_end rs (* e = true *)
let _ = ReadableString.end_read rs
(* similarly for ReadableFile and RemoteReader(ReadableString) *)
module R = Reader(ReadableString)
let r = R.begin_read "A multiline\ntext"
let r,t = R.read_all r (* t = "A multiline\ntext" *)
let _ = R.end_read r
Hint: String. split_on_char and String.concat can be useful here.
 *)
