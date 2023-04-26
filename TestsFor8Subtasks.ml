type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha

let equal_second_components (_, x) (_, y) = compare x y
let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2)

let wc22_C = 
  [(Arg, ["Messi"], Sau, ["Al-Shehri"; "Al-Dawsari"]);
   (Mex, [], Pol, []);
   (Pol, ["Zielinski"; "Lewandowski"], Sau, []);
   (Arg, ["Messi"; "Fernandez"], Mex, []);
   (Pol, [], Arg, ["Mac Allister"; "Alvarez"]);
   (Sau, ["Al-Dawsari"], Mex, ["Martin"; "Chavez"])
  ]

let wc22_H = 
  [(Uru, [], Kor, []);
   (Por, ["Ronaldo"; "Felix"; "Leao"], Gha, ["Ayew"; "Bukari"]);
   (Kor, ["Cho Gue-sung"; "Cho Gue-sung"], Gha, ["Salisu"; "Kudus"; "Kudus"]);
   (Por, ["Fernandes"; "Fernandes"], Uru, []);
   (Kor, ["Kim Young-gwon"; "Hwang Hee-chan"], Por, ["Horta"]);
   (Gha, [], Uru, ["De Arrascaeta"; "De Arrascaeta"])
  ]

let testing_member () =
   let l =
     [
       __LINE_OF__ ((member compare 3 [1; 2; 3]) = true);
       __LINE_OF__ ((member compare 4 [1; 2; 3]) = false);
       __LINE_OF__ ((member compare 'a' ['a'; 'b'; 'c']) = true);
       __LINE_OF__ ((member equal_second_components ('a',5) [(1,2); (3,4); (5,6)]) = false);
       __LINE_OF__ ((member equal_second_components ('a',6) [(1,2); (3,4); (5,6)]) = true);
       __LINE_OF__ ((member equal_second_components (42, 6) [(1,2); (3,4); (5,6)]) = true);
       __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 2; 3]) = true);
       __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 3; 5]) = false);
    ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The member test succeeds.\n"; [])
   else (Printf.printf "The member test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

let testing_count_occurrences () =
   let l =
     [
       __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
       __LINE_OF__ ((count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd']) = [('a', 3); ('c', 2); ('b', 1); ('d', 1)]);
       __LINE_OF__ ((count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0]) = [(0, 4); (3, 3); (-1, 2); (-2, 1)]);
       __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The count_occurrences test succeeds.\n"; [])
   else (Printf.printf "The count_occurrences test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

let testing_drop_last () =
   let l =
     [
       __LINE_OF__ ((drop_last [1; 2; 3; 4]) = [1; 2; 3]);
       __LINE_OF__ ((drop_last [1]) = []);
       __LINE_OF__ ((try Some (drop_last []) with (Failure _) -> None) = None) (* If this line is reported during testing, you have an rrror in raising Failure *)
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The drop_last test succeeds.\n"; [])
   else (Printf.printf "The drop_last test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


let testing_drop_last_opt () =
   let l =
     [
       __LINE_OF__ ((drop_last_opt []) = None);
       __LINE_OF__ ((drop_last_opt [1]) = Some []);
       __LINE_OF__ ((drop_last_opt [1;2;3]) = Some [1;2])
     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The drop_last_opt test succeeds.\n"; [])
   else (Printf.printf "The drop_last_opt test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



let testing_zip_with () =
   let l =
     [
       __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6]) = [[1; 5]; [2; 6]]);
       __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6;7;8]) = [[1; 5]; [2; 6]; [3; 7]]);
       __LINE_OF__ ((zip_with (fun x y -> (x,y)) [1;2;3] ['a';'b']) = [(1, 'a'); (2, 'b')]);
       __LINE_OF__ ((zip_with (+) [1;2;3] [5;6]) =[6; 8]);
       __LINE_OF__ ((zip_with (^) ["aa";"bb";"cc"] ["1";"2"]) = ["aa1"; "bb2"]);

     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The zip_with test succeeds.\n"; [])
   else (Printf.printf "The zip_with test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


let testing_unzip () =
   let l =
     [
       __LINE_OF__ ((unzip [('a',1); ('b',2)]) = (['a';'b'], [1;2]));
       __LINE_OF__ ((unzip []) = ([], []));
       __LINE_OF__ ((unzip [('a',1)]) = (['a'], [1]));

     ] in
   let result = List.fold_left (&&) true (List.map snd l) in
   if result then (Printf.printf "The unzip test succeeds.\n"; [])
   else (Printf.printf "The unzip test fails.\n Check the corresponding line numbers in the list below.\n";
         (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

let testing_table_and_scorers () =
  let l =
    [
      __LINE_OF__ (table_and_scorers wc22_H =
                     ([(Por, 3, 2, 0, 1, 6, 4, 6);
                       (Kor, 3, 1, 1, 1, 4, 4, 4);
                       (Uru, 3, 1, 1, 1, 2, 2, 4);
                       (Gha, 3, 1, 0, 2, 5, 7, 3)],
                      [("Cho Gue-sung", Kor, 2);
                       ("De Arrascaeta", Uru, 2);
                       ("Fernandes", Por, 2);
                       ("Kudus", Gha, 2);
                       ("Ayew", Gha, 1);
                       ("Bukari", Gha, 1);
                       ("Felix", Por, 1);
                       ("Horta", Por, 1);
                       ("Hwang Hee-chan", Kor, 1);
                       ("Kim Young-gwon", Kor, 1);
                       ("Leao", Por, 1);
                       ("Ronaldo", Por, 1);
                       ("Salisu", Gha, 1)]));
      __LINE_OF__ (table_and_scorers wc22_C =
                     ([(Arg, 3, 2, 0, 1, 5, 2, 6);
                       (Pol, 3, 1, 1, 1, 2, 2, 4);
                       (Mex, 3, 1, 1, 1, 2, 3, 4);
                       (Sau, 3, 1, 0, 2, 3, 5, 3)],
                      [("Al-Dawsari", Sau, 2);
                       ("Messi", Arg, 2);
                       ("Al-Shehri", Sau, 1);
                       ("Alvarez", Arg, 1);
                       ("Chavez", Mex, 1);
                       ("Fernandez", Arg, 1);
                       ("Lewandowski", Pol, 1);
                       ("Mac Allister", Arg, 1);
                       ("Martin", Mex, 1);
                       ("Zielinski", Pol, 1)]))
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The table_and_scorers test succeeds.\n"; [])
  else (Printf.printf "The table_and_scorers test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
