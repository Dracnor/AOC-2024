(* Day 7 : Bridge Repair *)

(** input_file name is read from argv *)
let input_file = 
  if Array.length Sys.argv < 2 then 
    failwith ("Usage : " ^ Sys.argv.(0) ^ " input_file")
  else
    Sys.argv.(1)

(** Returns an iterator of the lines.
  * Each lines is a (test value, number list) pair *)
let read_input () =
  let i_c = open_in input_file in
  let get_equation () =
    try 
      let line = input_line i_c in
      let [@warning "-8"] [left; right] = String.split_on_char ':' line in
      let test_value = int_of_string left in
      let numbers = String.split_on_char ' ' right 
                    |> List.tl (* right starts with a space, so "" in split *)
                    |> List.map int_of_string  in
      Some ((test_value, numbers), () )
    with 
      End_of_file -> close_in i_c; None
  in
  Seq.unfold (fun () -> get_equation ()) ()


(* Part 1 *)

(* Explain here if necessary *)

(** Checks if an equation is solvable.
    total is the sum of previous good calibrations.
    Returns the new value of total. *)
let solvable total (test_value, numbers) =
  
  (** Goes through numbers left to right.
    * acc is what has already been computed from the left *)
  let rec loop acc = function
    | _ when acc > test_value -> false
    | [] -> acc = test_value
    | h :: t -> loop (acc * h) t || loop (acc + h) t
  in
  total + (if loop (List.hd numbers) (List.tl numbers) then test_value else 0)

let part1 iterator =
  Seq.fold_left solvable 0 iterator



(* Part 2 *)

(* Explain (very smartly) here if necessary *)

let part2 input =
  -666
  
  


(** main *)
let () =
  let input = read_input () in
  Printf.printf "%d\n%d\n" (part1 input) (part2 input)


