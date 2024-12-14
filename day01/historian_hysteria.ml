(* Day1 : historian hysteria *)

let input_file = "input.in"


(** Dans un dictionaire, réalise tbl[key]+=1 .
  * Si la clef n'était pas présente, réalise tbl[key] = 1 *)
let hash_incr tbl key =
  let nb = 
    try Hashtbl.find tbl key 
    with Not_found -> 0
  in
  Hashtbl.replace tbl key (nb +1)


(** Reads the input and returns it as two int lists,
    and a counting hashtbl for the second list *)
let read_input () =
  let input = open_in input_file in
  let count_right = Hashtbl.create 42 in
  let rec loop left right =
    try 
      let [@warning "-8"] [l; _; _; r] = input_line input |> String.split_on_char ' ' in
      hash_incr count_right (int_of_string r);
      loop (int_of_string l ::left) (int_of_string r ::right)
    with End_of_file -> 
      close_in input;
			left, right, count_right
  in
  loop [] []
  

(** Computes part1 solution given the left and rigth list.
  * Sorts both lists to do it -> nlogn *)
let part1 left right =

  (** Computes |x-y| *)
  let apart x y =
    if x < y then y-x else x-y
  in
  
  let sorted_left  = List.sort Int.compare left  in
  let sorted_right = List.sort Int.compare right in
  List.fold_left2 (fun sum l r -> sum + (apart l r)) 0 sorted_left sorted_right


(** Computes part 2 solution given the list 
  * and the counting for the right list.
  * also nlogn *)
let part2 left right count_right =

  (** Computes the similarity score for l and r *)
  let similarity l r =
    l * (try Hashtbl.find count_right l with Not_found -> 0)
  in

  let sorted_left  = List.sort Int.compare left  in
  let sorted_right = List.sort Int.compare right in
  List.fold_left2 (fun sum l r -> sum + similarity l r) 0 sorted_left sorted_right


(** main *)
let () =
  let left, right, count_right = read_input () in
  part1 left right |> print_int; 
  print_newline ();
  part2 left right count_right |> print_int; 
  print_newline ();
