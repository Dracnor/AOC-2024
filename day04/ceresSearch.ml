(* Day4 : Ceres Search *)

(* new : I get the input file from argv *)
let input_file = 
  if Array.length Sys.argv < 2 then 
    failwith ("Usage : " ^ Sys.argv.(0) ^ " input_file")
  else
    Sys.argv.(1)

(** Reads the input as a char matrix.
  * The input must be square *)
let read_input () =
  let input = open_in input_file in
  let fst_line = input_line input in
  let len = String.length fst_line in
  
  (** Creates the line i of the matrix *)
  let init_line i =
    let str_line = if i = 0 then fst_line else input_line input in
    Array.init len (fun j -> str_line.[j])
  in
  let matrix = Array.init len init_line in
  close_in input;
  matrix
  
  

(* Part 1 *)

(** Counts how many times is (i,j) the start of a valid XMAS *)
let count_xmas_here matrix i j =
  if matrix.(i).(j) <> 'X' then 0 else
  
  (** Tests if there is a (x)mas from (i,j)
    * in direction given by vector (d_i, d_j).
    * matrix.(i).(j) is assumed to be 'x' *)
  let xmas_direction d_i d_j =
    try
    (* I could do that in a cleaner way with for_all, 
       but 3 conditions isn't a lot to type *)
         matrix.(i+   d_i).(j+   d_j) = 'M'
      && matrix.(i+ 2*d_i).(j+ 2*d_j) = 'A'
      && matrix.(i+ 3*d_i).(j+ 3*d_j) = 'S'
    with
        Invalid_argument _ -> false
  in
  (* Same, I'm too lazy to be smart *)
  let count_here = ref 0 in
  for d_i = -1 to 1 do
    for d_j = -1 to 1 do
      if xmas_direction d_i d_j then incr count_here
    done
  done;
  !count_here


(** Computes part1 answer *)
let part1 matrix =
  let len = Array.length matrix in
  let count = ref 0 in
  for i = 0 to len -1 do
    for j = 0 to len-1 do
      count := !count + count_xmas_here matrix i j
    done
  done;
  !count



(* part 2 *)
(* basically the same *)

(** Cherks if (i,j) the top_right of a valid X-MAS *)
let cross_mas_here matrix i j =
  
  (** Tests if there is a mas from (x,y)
    * in direction given by vector (d_x, d_y). *)
  let mas_direction x y d_x d_y =
    try  matrix.(x       ).(y       ) = 'M'
      && matrix.(x+ 1*d_x).(y+ 1*d_y) = 'A'
      && matrix.(x+ 2*d_x).(y+ 2*d_y) = 'S'
    with
        Invalid_argument _ -> false
  in
  (* four possibles X-MAS :
     M S   M M   S S   S M
      A     A     A     A
     M S   S S   M M   S M 
     
     I'll test them in that order *)
  (    mas_direction  i     j      1    1
    && mas_direction (i+2)  j     (-1)  1 )
  || 
  (    mas_direction  i     j      1    1
    && mas_direction  i    (j+2)   1   (-1) )
  ||
  (    mas_direction (i+2)  j    (-1)   1
    && mas_direction (i+2) (j+2) (-1)  (-1) )
  ||
  (    mas_direction  i    (j+2)  1   (-1)
    && mas_direction (i+2) (j+2) (-1) (-1) )


(** Computes part2 answer *)
let part2 matrix =
  let len = Array.length matrix in
  let count = ref 0 in
  for i = 0 to len -1 do
    for j = 0 to len-1 do
      if cross_mas_here matrix i j then incr count
    done
  done;
  !count

  
  
  
  
(** main *)
let () =
  let matrix = read_input () in
  Printf.printf "%d\n%d\n" (part1 matrix) (part2 matrix)
