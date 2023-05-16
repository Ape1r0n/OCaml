(* -----------------------------------------------------------------------------
 *  TESTING: Some simple tests for functions f1, f2 and f3. 
 *  If testing_fs () does not succeed, please check the line numbers in the 
 *  returend list to see which test failed, and then check again your solution.
 *  The tests should not be modified.
 * -----------------------------------------------------------------------------
 *)                                    

let testing_fs () =
   let l =
     [
       __LINE_OF__ ((List.fold_left f1 [] [(1,2); (3,4); (5,6)]) =
                      [(2,1); (4,3); (6,5)]);
       __LINE_OF__ ((List.fold_left f2 [] ['a';'b';'c';'d';'e';'f';'g']) =
                      ['g';'e';'c';'a';'b';'d';'f']);
       __LINE_OF__ (let g = List.fold_left f3 (fun _ -> 0)
                              [('a',3); ('z', -9); ('d', 18)] in
                    (g 'a' = 3) && (g 'd' = 18) && (g 'z' = -9))
    ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The f1, f2, f3 test succeeds.\n"; [])
   else (Printf.printf "The f1, f2, f3 test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)




(* -----------------------------------------------------------------------------
 *  TESTING: Simple tests for map_tr and replicate_tr. 
 *  If test_tr_llist () says that the test did not succeed
 *  please check the returned line numbers to see which tests failed, 
 *  and then check again your solution.
 *  The tests should not be modified.
 * -----------------------------------------------------------------------------
 *)     

let test_tr_llist () =
  let l =
    [
      __LINE_OF__ (map_tr succ [1;2;3] = [2;3;4]);
      __LINE_OF__ (map_tr (fun x -> x^x) ["a";"b";"c"] = ["aa";"bb";"cc"]);
      __LINE_OF__ (replicate_tr 5 "a" = ["a";"a";"a";"a";"a"]);
      __LINE_OF__ (replicate_tr (-3) "a" = [])
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The tests for map and replicate succeed.\n"; [])
  else (Printf.printf "The test for tests for map and replicate fail.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

(* -----------------------------------------------------------------------------
 *  TESTING: Helper functions used for testing lazy lists
 * -----------------------------------------------------------------------------
 *)    

let rec from_to_custom from to_ step =
      if from <= to_
      then fun () -> ConsC (from, from_to_custom (from + step) to_ step)
      else fun () -> NilC

let rec print_custom_llist n c_list =
  if n != 0
  then match c_list () with
       | NilC -> print_string "Nil\n"
       | ConsC (h, t) ->
          Printf.printf "%d, " h;
          print_custom_llist (n-1) t
  else print_string "...\n"

let rec custom_llist_to_string n c_list =
  if n != 0
  then match c_list () with
    | NilC -> "Nil"
    | ConsC (h, t) ->
       string_of_int h ^ ", " ^
         custom_llist_to_string (n-1) t
  else "..."

let rec from_to_ocaml from to_ step =
      if from <= to_
      then lazy (ConsO (from, from_to_ocaml (from + step) to_ step))
      else lazy NilO

let rec print_ocaml_llist n o_list =
  if n != 0
  then match Lazy.force o_list with
    | NilO -> print_string "Nil\n"
    | ConsO (h, t) ->
       Printf.printf "%d, " h;
       print_ocaml_llist (n-1) t
  else print_string "...\n"

let rec ocaml_llist_to_string n o_list =
  if n != 0
  then match Lazy.force o_list with
    | NilO -> "Nil"
    | ConsO (h, t) ->
       string_of_int h ^ ", " ^
         ocaml_llist_to_string (n-1) t
  else "..."

(* -----------------------------------------------------------------------------
 *  TESTING: Simple tests for map_over_custom_llist and map_over_ocaml_llist. 
 *  If test_map_llist () says that the test did not succeed
 *  please check the returned line numbers to see which tests failed, 
 *  and then check again your solution.
 * The tests should not be modified.
 * -----------------------------------------------------------------------------
 *)     

let test_map_llist () =
  let l =
    [
      __LINE_OF__ (custom_llist_to_string 10
        (map_over_custom_llist (fun x -> x+1) (from_to_custom 0 5 1)) =
                     "1, 2, 3, 4, 5, 6, Nil");
      __LINE_OF__ (custom_llist_to_string 10
        (map_over_custom_llist (fun x -> x+1) (from_to_custom 6 5 1)) =
                     "Nil");
       __LINE_OF__ (ocaml_llist_to_string 10
        (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 0 5 1)) =
                      "1, 2, 3, 4, 5, 6, Nil");
        __LINE_OF__ (ocaml_llist_to_string 10
        (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 6 5 1)) =
                       "Nil")
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for mapping over lazy lists succeeds.\n"; [])
  else (Printf.printf "The test for mapping over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

(* -----------------------------------------------------------------------------
 *  TESTING: Simple tests for merge_custom_llists and merge_ocaml_llists. 
 *  If test_merge_llists () says that the test did not succeed
 *  please check the returned line numbers to see which tests failed, 
 *  and then check again your solution.
 *  The tests should not be modified.
 * -----------------------------------------------------------------------------
 *)     

let test_merge_llists () =
  let l =
    [
      __LINE_OF__ (custom_llist_to_string 13
        (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 0 5 1)) =
                     "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
      __LINE_OF__ (custom_llist_to_string 13
                     (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 6 5 1)) =
                     "0, 1, 2, 3, 4, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
        (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 0 5 1)) =
                     "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
                     (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 6 5 1)) =
                     "0, 1, 2, 3, 4, 5, Nil")
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for merging over lazy lists succeeds.\n"; [])
  else (Printf.printf "The test for merging over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


(* -----------------------------------------------------------------------------
 *  TESTING: Simple tests for drop_dupl_custom_llist and drop_dupl_ocaml_llist. 
 *  If test_drop_dupl_llists () says that the test did not succeed
 *  please check the returned line numbers to see which tests failed, 
 *  and then check again your solution.
 *  The tests should not be modified.
 * -----------------------------------------------------------------------------
 *)     

let test_drop_dupl_llists () =
  let l =
    [
      __LINE_OF__ (custom_llist_to_string 13
                     (drop_dupl_custom_llist
                        (merge_custom_llists (from_to_custom 0 5 1)
                           (from_to_custom 0 5 2))) =
                     "0, 1, 2, 3, 4, 5, Nil");
      __LINE_OF__ (custom_llist_to_string 13
                     (drop_dupl_custom_llist
                        (merge_custom_llists (from_to_custom 0 5 1)
                           (from_to_custom 6 5 1))) =
                     "0, 1, 2, 3, 4, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
                     (drop_dupl_ocaml_llist
                        (merge_ocaml_llists (from_to_ocaml 0 5 1)
                           (from_to_ocaml 0 5 1))) =
                     "0, 1, 2, 3, 4, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
                     (drop_dupl_ocaml_llist
                        (merge_ocaml_llists (from_to_ocaml 0 5 1)
                           (from_to_ocaml 6 5 1))) =
                     "0, 1, 2, 3, 4, 5, Nil")
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for dropping duplicates from  lazy lists succeeds.\n"; [])
  else (Printf.printf "The test for dropping duplicates from lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



 (* -----------------------------------------------------------------------------
 *  TESTING: Simple tests for hamming_custom and hamming_ocaml. 
 *  If test_hamming_llists () says that the test did not succeed
 *  please check the returned line numbers to see which tests failed, 
 *  and then check again your solution.
 *  The tests should not be modified.
 * -----------------------------------------------------------------------------
 *)     

let test_hamming_llists () =
  let l =
    [
      __LINE_OF__ (custom_llist_to_string 14 (hamming_custom_llist ()) =
                     "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
      __LINE_OF__ (custom_llist_to_string 20 (hamming_custom_llist ()) = 
                     "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...");
      __LINE_OF__ (ocaml_llist_to_string 14 (hamming_ocaml_llist ()) =
                     "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
      __LINE_OF__ (ocaml_llist_to_string 20 (hamming_ocaml_llist ()) = 
                     "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...")
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for Hamming lists succeeds.\n"; [])
  else (Printf.printf "The test for hamming lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
 
