type student = {fn : string; ln : string; id : int; semester : int; grades : (int * float) list;}
type database = student list  (* Single elemented types do not need curly brackets *)

let insert stud db = stud :: db
let rec find_by_id id db = match db with [] -> [] | current_stud :: next_stud -> if current_stud.id = id then [current_stud] else  find_by_id id next_stud 
let rec find_by_last_name ln db = match db with [] -> [] | current_stud :: next_stud -> if current_stud.ln = ln then current_stud :: find_by_last_name ln next_stud else find_by_last_name ln next_stud

(* Extended Student Database *)
let rec remove_by_id id db = match db with [] -> [] | h :: t -> if h.id <> id then h :: remove_by_id id t else remove_by_id id t
let rec count_in_semester semester db = match db with [] -> 0 | h :: t -> if h.semester = semester then 1 + count_in_semester semester t else count_in_semester semester t

(* Helper functions *)
let rec count_courses grades = match grades with | [] -> 0 | _ :: t -> 1 + count_courses t
let rec sum_grades grades = match grades with | [] -> 0.0 | (x, y) :: t -> y +. sum_grades t 
let avg_grade s = let sum_of_grades = sum_grades s.grades in let course_count = count_courses s.grades in if course_count = 0 then 0.0 else sum_of_grades /. float course_count
let rec find_course id grades = match grades with | [] -> false | (x, y) :: t -> if x = id then true else find_course id t
let rec course_count_by_id id db = match db with | [] -> 0 | s :: t -> (if find_course id s.grades then 1 else 0) + course_count_by_id id t
let rec sum_for_course id db = match db with | [] -> 0.0 | s :: t -> match s.grades with | [] -> sum_for_course id t | (x, score) :: tail -> if x = id then score +. sum_for_course id t else sum_for_course id ({s with grades = tail} :: t)

(* For student_avg_grade and course_avg_grade *)

let student_avg_grade id db = match find_by_id id db with | [] -> 0.0 | [s] -> avg_grade s | _ -> 0.0
let course_avg_grade id db = let count = course_count_by_id id db in let sum = sum_for_course id db in if count = 0 then 0.0 else sum /. float count
