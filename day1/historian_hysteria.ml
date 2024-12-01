(* Day1 : historian hysteria *)

let input_file = "input.in"

(** Reads the input and returns it as two int lists *)
let read_input () =
  let input = open_in input_file in
  let rec loop left right =
    try 
      let [@warning "-8"] [l; _; _; r] = input_line input |> String.split_on_char ' ' in
      loop (int_of_string l ::left) (int_of_string r ::right)
    with End_of_file -> 
      left,right
  in
  loop [] []
  

(** Computes |x-y| *)
let apart x y =
  if x < y then y-x else x-y


(** Computes day1 solution given the left and rigth list.
  * Sorts both lists to do it -> nlogn *)
let day1 left right =
  
  let sorted_left  = List.sort Int.compare left  in
  let sorted_right = List.sort Int.compare right in
  List.fold_left2 (fun sum l r -> sum + (apart l r)) 0 sorted_left sorted_right


(** main *)
let () =
  let left, right = read_input () in
  day1 left right
  |> print_int; print_newline ()
