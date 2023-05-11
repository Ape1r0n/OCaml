(* Rings *)
module type Ring = sig
    type t
    val zero : t
    val one : t
    val add : t -> t -> t
    val mul : t -> t -> t
    val compare : t -> t -> int
    val to_string : t -> string
end

module type FiniteRing = sig
    include Ring
    val elems : t list
end

module IntRing : Ring with type t = int = struct
  type t = int
  let zero = 0
  let one = 1
  let add = ( + )
  let mul = ( * )
  let compare x y = if x < y then -1 else if x > y then 1 else 0
  let to_string i = string_of_int i
end

module FloatRing : Ring with type t = float = struct
  type t = float
  let zero = 0.0
  let one = 1.0
  let add = ( +. )
  let mul = ( *. )
  let compare x y = compare x y
  let to_string f = string_of_float f
end

module BoolRing : FiniteRing with type t = bool = struct
    type t = bool
    let zero = false
    let one = true
    let add = ( || )
    let mul = ( && )
    let compare x y = compare x y
    let to_string b = string_of_bool b
    let elems = [true; false]
end

module SetRing (R : FiniteRing) : Ring with type t = R.t list = struct
    type t = R.t list
    let zero = []
    let one = R.elems
    let compare a b =
    let sort_and_compare x y = R.compare x y in
        let sort_1, sort_2 = List.sort sort_and_compare a, List.sort sort_and_compare b in
        let rec helper l1 l2 = match l1, l2 with
            | [],[] -> 0
            | [],_ -> -1
            | _,[] -> 1
            | x::xs, y::ys -> let temp = sort_and_compare x y in
            if temp <> 0 then temp else helper xs ys
        in helper sort_1 sort_2
    let to_string l = "{" ^ String.concat ", " (List.map R.to_string l) ^ "}"
    let add a b = List.sort_uniq R.compare (a @ b)
    let mul a b = List.filter (fun x -> List.find_opt ((=) x) b <> None) a
end


(* Matrices *)
module type Matrix = sig
    type elem
    type t
    val create : int -> int -> t
    val identity : int -> t
    val from_rows : elem list list -> t
    val set : int -> int -> elem -> t -> t
    val get : int -> int -> t -> elem
    val transpose : t -> t
    val add : t -> t -> t
    val mul : t -> t -> t
    val to_string : t -> string
end

module DenseMatrix (R : Ring) : Matrix with type elem = R.t = struct
    type elem = R.t
    type t = elem list list  
    let create n m = List.init n (fun _ -> List.init m (fun _ -> R.zero))
    let identity n = List.init n (fun i -> List.init n (fun j -> if i = j then R.one else R.zero))
    let from_rows l = l
    let set r c v m = List.mapi (fun i row -> if i = r then List.mapi (fun j column -> if j = c then v else column) row else row) m
    let get r c m = List.nth (List.nth m r) c
    let transpose m = match m with [] -> [] | hd::tl -> List.init (List.length hd) (fun i -> List.init (List.length m) (fun j -> get j i m))
    let add a b = List.map2 (List.map2 R.add) a b
    let mul a b = List.map (fun row -> List.map (fun col -> List.fold_left R.add R.zero (List.map2 R.mul row col)) (transpose b) ) a
    let to_string m = String.concat "\n" (List.map (fun i -> String.concat " " (List.map R.to_string i)) m)
end

module SparseMatrix (R : Ring) : Matrix with type elem = R.t = struct
    type elem = R.t
    type t = { cells : (int * int * R.t) list; nrows : int; ncols : int }
    let create nrows ncols = { cells = []; nrows; ncols }
    let identity size = { cells = List.init size (fun i -> (i, i, R.one)); nrows = size; ncols = size }

    let from_rows rows =
        let nrows = List.length rows in
        let ncols = List.length (List.hd rows) in
        let cells = List.concat @@
            List.mapi (fun r row ->
                List.mapi (fun c v -> (r, c, v)) row
                |> List.filter (fun (_, _, x) -> R.compare x R.zero <> 0)
            ) rows in { cells; nrows; ncols }

    let set row col value m =
        let rec helper acc = function
            | [] -> List.rev ((row, col, value) :: acc)
            | (r, c, v) :: cs ->
                if r < row then helper ((r, c, v) :: acc) cs
                else if r > row then List.rev_append ((row, col, value) :: acc) ((r, c, v) :: cs)
                else if c < col then helper ((r, c, v) :: acc) cs
                else if c > col then List.rev_append ((row, col, value) :: acc) ((r, c, v) :: cs)
                else List.rev_append ((row, col, value) :: acc) cs
            in { m with cells = helper [] m.cells }

    let get row col m =
        if row < 0 || row >= m.nrows || col < 0 || col >= m.ncols then failwith "get failure";
        let rec helper = function
            | [] -> R.zero
            | (r, c, v) :: cs ->
                if r < row || (r = row && c < col) then helper cs
                else if r = row && c = col then v
                else R.zero
        in helper m.cells

    let transpose m = let transposed_cells = List.map (fun (r, c, v) -> (c, r, v)) m.cells |> List.sort (fun (i1, j1, k1) (i2, j2, k2) -> if i1 = i2 then compare j1 j2 else compare i1 i2) in { cells = transposed_cells; nrows = m.ncols; ncols = m.nrows }

    let add a b =
        if a.nrows <> b.nrows || a.ncols <> b.ncols then failwith "add failure";
            let rec merge acc m1 m2 = match m1, m2 with
                | [], m | m, [] -> List.rev_append acc m
                | (r1, c1, v1) :: t1, (r2, c2, v2) :: t2 ->
                    if r1 < r2 || (r1 = r2 && c1 < c2) then merge ((r1, c1, v1) :: acc) t1 m2
                    else if r1 = r2 && c1 = c2 then merge ((r1, c1, R.add v1 v2) :: acc) t1 t2
                    else merge ((r2, c2, v2) :: acc) m1 t2
        in { cells = merge [] a.cells b.cells; nrows = a.nrows; ncols = a.ncols }

    let mul a b =
        if a.ncols <> b.nrows then failwith "mul failure";
        let rec compute r c n = if n >= a.ncols then R.zero else R.add (R.mul (get r n a) (get n c b)) (compute r c (n+1))
        in let rec helper r n = if n >= b.ncols then [] else let v = compute r n 0 in if v = R.zero then helper r (n+1) else (r,n,v)::helper r (n+1)
        in let rec aux n = if n >= a.nrows then [] else (helper n 0) @ aux (n+1)
        in { cells = aux 0; nrows = a.nrows; ncols = b.ncols }

    let to_string m = let row_to_string r = List.init m.ncols (fun c -> get r c m) |> List.map R.to_string |> String.concat " " in List.init m.nrows row_to_string |> String.concat "\n"

end
