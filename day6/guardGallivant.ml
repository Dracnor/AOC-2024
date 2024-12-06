(* Day 6 : Guard Gallivant *)

(** input_file name is read from argv *)
let input_file = 
  if Array.length Sys.argv < 2 then 
    failwith ("Usage : " ^ Sys.argv.(0) ^ " input_file")
  else
    Sys.argv.(1)



(* So, I *could* be smart and not store the whole map,
   juste the obstacles. Same, I *could* be smart and
   not store the whole path, just the segments.
   But being smart is tiresome *)

(** Reads the input as a char matrix
  * The input must be square *)
let read_input () =
  let i_c = open_in input_file in
  let fst_line = input_line i_c in
  let len = String.length fst_line in
  (** Creates the line i of the matrix *)
  let init_line i =
    let str_line = if i = 0 then fst_line else input_line i_c in
    Array.init len (fun j -> str_line.[j])
  in
  let matrix = Array.init len init_line in
  close_in i_c;
  matrix


(** Finds the coordinates of the '^' *)
let find_start lab_map =
  let exception Found of int*int in
  try
    for i = 0 to Array.length lab_map -1 do
      for j = 0 to Array.length lab_map.(i) -1 do
        if lab_map.(i).(j) = '^' then raise (Found (i,j))
      done
    done;
    raise Not_found
  with Found (i,j) -> (i,j)


(* The 4 directions, and the right turn *)
let down  = ( 1,  0)
let up    = (-1,  0)
let right = ( 0,  1)
let left  = ( 0, -1)
let turn dir =
  if      dir = up    then right
  else if dir = right then down
  else if dir = down  then left
  else (* dir = left *)    up


(** An exception for signaling Cycle in part 2 *)
exception Cycle



(* Part 1 *)

(* while in bounds, step.
   Return the pos->dir hashtbl (is usefull for part2).
   The number of distinct pos can be deduced from it. *)

(** Returns the hashtbl of all the position->dir the guard takes.
    Raises Cycle if it cycle ;) *)
let get_path lab_map =

  let history = Hashtbl.create 1000 in  
  (** Adds (i,j) -> dir into history.
      Raises Cycle if this association is already there. *)
  let update_history i j dir =
    if List.mem dir (Hashtbl.find_all history (i,j)) then raise Cycle;
    Hashtbl.add history (i,j) dir
  in
  
  (* while the guard is in the map, compute what it does *)
  let rec walk i j dir =
    let (d_i, d_j) = dir in
    try
      if lab_map.(i + d_i).(j + d_j) = '#' then begin 
        (* Then we turn, but don't advance yet : 
           there might be a '#' in the new direction as well *)
        let new_dir = turn dir in
        update_history i j new_dir;
        walk i j (turn dir)
      end
      else begin
        (* then we safely advance *)
        let new_i = i + d_i in
        let new_j = j + d_j in
        update_history new_i new_j dir;
        walk new_i new_j dir
      end
    with
      (* raised when lab_map.(i + d_i).(j + d_j) doesn't exists,
         which is when the guard goes out of bounds =) *)
      Invalid_argument _ -> history
  in  
   
  let start_i, start_j = find_start lab_map in
  walk start_i start_j up


(* Issue : Hashtbl.length returns the number of (i,j)-> dir
    in the table. That's not what I want, hence this function.
    NB : map is 130*130, so pos*dir is at most 4*130*130.
    So sorting it isn't to costly *)
    
(** Returns the list of distincts keys in tbl *)
let distinct_keys tbl =
  Hashtbl.to_seq_keys tbl
  |> List.of_seq
  |> List.sort_uniq Stdlib.compare




(* Part 2 *)

(* For every position of part 1, try to put an obstacle there
   and see if it makes a loop. *)

(** Does part2 as explained aboved.
  * part1_pos is the list of *distinct* positions in part1 *)
let part2 lab_map part1_pos =
  let count = ref 0 in
  let test_obstacle (x,y) =
    if lab_map.(x).(y) <> '^' then begin
      lab_map.(x).(y) <- '#'; (* put an obstacle *)
      (try ignore @@ get_path lab_map with Cycle -> incr count);
      lab_map.(x).(y) <- '.'  (* undo the put an obstacle *)
    end
  in
  List.iter test_obstacle part1_pos;
  !count
  
  


(** main *)
let () =
  let lab_map = read_input () in
  let part1_pos = distinct_keys (get_path lab_map) in 
  let answer_part1 = List.length part1_pos in
  let answer_part2 = part2 lab_map part1_pos in
  Printf.printf "%d\n%d\n" (answer_part1) (answer_part2)

