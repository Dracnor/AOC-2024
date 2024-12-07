(* Day 7 : Bridge Repair *)

(** input_file name is read from argv *)
let input_file = 
  if Array.length Sys.argv < 2 then 
    failwith ("Usage : " ^ Sys.argv.(0) ^ " input_file")
  else
    Sys.argv.(1)

(** Returns the list of the lines.
  * Each lines is a (test value, number list) pair. *)
let read_input () =
  let i_c = open_in input_file in
  let rec loop () =
    try
      let line = input_line i_c in
      let [@warning "-8"] [left; right] = String.split_on_char ':' line in
      let test_value = int_of_string left in
      let numbers = String.split_on_char ' ' right 
                    |> List.tl (* right starts with a space, so "" in split *)
                    |> List.map int_of_string  in
      (test_value, numbers) :: (loop ())
    with 
      End_of_file -> close_in i_c; []
  in
  loop ()




(* Part 1 *)

(* Just check each equation. Parsing was the hardest thing. *)

(** Checks if an equation is solvable according to part1 ruleset.
    total is the sum of previous part1 good calibrations.
    Returns the new value of total. *)
let part1_solvable total (test_value, numbers) =
  
  (** Goes through numbers left to right.
    * acc is what has already been computed from the left *)
  let rec loop acc = function
    | _ when acc > test_value -> false
    | [] -> acc = test_value
    | h :: t -> loop (acc * h) t || loop (acc + h) t
  in
  total + (if loop (List.hd numbers) (List.tl numbers) then test_value else 0)

let part1 eq_list =
  List.fold_left part1_solvable 0 eq_list




(* Part 2 *)

(* Same but with a new operator.
   I could do both part at once, but I find it cleaner this way. *)

(** Concatenates two int. Easy. *)
let slow_int_cat x y = 
  (string_of_int x) ^ (string_of_int y)
  |> int_of_string


(* For fun : let's do a faster in_cat *)
(** Returns the smallest power of 10 > a *)
let bigger_p10 a =
  let rec loop p10 =
    if p10 > a then p10 else loop (10*p10)
  in
  loop 1


(** Concatenates two ints. Efficient.
    Makes the whole program 33% faster. Not that I needed it. *)
let int_cat x y =
  x * (bigger_p10 y) + y
  

(** Same as part1 but with part2 ruleset. *)
let part2_solvable total (test_value, numbers) =
  
  (** Same as in part1 *)
  let rec loop acc = function
    | _ when acc > test_value -> false
    | [] -> acc = test_value
    | h :: t -> loop (acc * h) t || loop (acc + h) t || loop (int_cat acc h) t
  in
  total + (if loop (List.hd numbers) (List.tl numbers) then test_value else 0)

let part2 eq_list =
  List.fold_left part2_solvable 0 eq_list  
  



(** main *)
let () =
  let eq_list = read_input () in
  Printf.printf "%d\n%d\n" (part1 eq_list) (part2 eq_list)
