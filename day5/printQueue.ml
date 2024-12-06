(* Day 5 : Print Queue. *)

(* ASSUMPTION : EACH PAGE ONLY APPEAR ONCE IN AN UPDATE. *)
(* I'll stores rules as a (x,y) Hashtbl, and each line as a int array *)


(** input_file name is read from argv *)
let input_file = 
  if Array.length Sys.argv < 2 then 
    failwith ("Usage : " ^ Sys.argv.(0) ^ " input_file")
  else
    Sys.argv.(1)



(* For part1 *)

(** Checks if update is well ordered
  * Quadratic complexity (it's ok, updates are small) *)
let is_well_ordered rules update =
  let exception Bad in
  try
    for i = 0 to Array.length update -1 do
      for j = i+1 to Array.length update -1 do
        if Hashtbl.mem rules (update.(j), update.(i)) then raise Bad
      done
    done;
    true
  with Bad -> false
    


(* For part2 *)

(** Compare function for pages. Will be used to sort. *)
let cmp_page rules x y =
  if Hashtbl.mem rules (x,y) then -1
  else if Hashtbl.mem rules (y,x) then 1
  else 0




(** main AND input reading *)
let () =
  (* Read rules *)
  let i_c = open_in input_file in
  let rules = Hashtbl.create 1200 in (* input has ~1200 rules *)
  let rec rules_loop () =
    match input_line i_c with
    | "" -> () (* empty lines ends the rules *)
    | x_y -> Scanf.sscanf x_y "%d|%d" (fun x y -> Hashtbl.add rules (x,y) true); 
             rules_loop ()
  in 
  rules_loop ();
  
  (* Read each update and computes its middle page *)
  let part1 = ref 0 in
  let part2 = ref 0 in
  let rec update_loop () =
    try
      let update = input_line i_c 
                   |> String.split_on_char ',' 
                   |> List.map int_of_string
                   |> Array.of_list            in
      if is_well_ordered rules update then
        part1 := !part1 + update.(Array.length update /2)
      else begin
        Array.fast_sort (cmp_page rules) update;
        part2 := !part2 + update.(Array.length update /2)
      end;
      update_loop ()
    with 
      End_of_file -> ()
  in
  update_loop ();
  close_in i_c;
  Printf.printf "part1 : %d\npart2 : %d\n" !part1 !part2
