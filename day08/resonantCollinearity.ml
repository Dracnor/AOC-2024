(* Day 8 : resonant Collinearity *)

(* Rmk : I don't like my code for today;
	 it's uselessly complicated *)


(** input_file name is read from argv *)
let input_file = 
  if Array.length Sys.argv < 2 then 
    failwith ("Usage : " ^ Sys.argv.(0) ^ " input_file")
  else
    Sys.argv.(1)


(** Returns the input as a frequency -> coordinates array.
  * Also returns the dimension of the input (assumed square) *)
let read_input () =
  let i_c = open_in input_file in
  
  let antennas = Array.make 128 [] in
  (** Adds the (i,j) antenna for frequency c *)
  let add_antenna i j c =
    if c <> '.' then
      antennas.(int_of_char c) <- (i,j) :: antennas.(int_of_char c)
  in
  
  (** Reads line number i then the next ones *)
  let rec loop i =
    try
      String.iteri (add_antenna i) (input_line i_c);
      loop (i+1)
    with 
      End_of_file -> close_in i_c
   in 
   
   (* Treat first line differently to get square dimension *)
   let fst_line = input_line i_c in
   let dim = String.length fst_line in
   String.iteri (add_antenna 0) fst_line;
   (* Then loop for the other lines *)
   loop 1;
   
   dim, antennas




(* Part 1 *)

(** Checks that (x,y) is within the borders *)
let inmap dim (x,y) = 
     0 <= x && x < dim (* x is within bound *)
  && 0 <= y && y < dim (* y is within bound *)


(* Seems like the expected distance is the euclidian one.
   But instead of working on distance, I'll work on vectors :
   the vector from an antinode to one antenna is the double of 
   the vector from that antenna to the other antinode *)

(** Returns the list of the inmap antinodes associated to (i,j) and (k,l);
  * using part1 ruleset. *)
let part1_antinodes dim (i,j) (k,l) =
  (* each solution is the only solution of one of the two eq below :
     E0:    (x,y) - (i,j) = 2 ((x,y) - (k,l))    -> point closer to (k,l)
     E1:    (x,y) - (k,l) = 2 ((x,y) - (i,j))    -> point closer to (i,j)
     
     These are the solutions :
     E0 : (x,y) = (2k-i , 2l-j)
     E1 : (x,y) = (2i-k , 2j-l)
  *)
  if (i,j) = (k,l) then 
    [] 
  else
    [(2*k-i, 2*l-j) ; (2*i-k, 2*j-l)]
    |> List.filter (inmap dim)



(** I'll compute the list of all inmap antinodes, then sort_uniq it *)
let number_of_antinodes get_antinodes dim antennas =

  (** Adds the antinodes associated to any ((i,j) and (k,l)) from coordinates
    * to anti_lst *)
  let add_antinodes anti_lst coordinates =
  
    (** Same with fixed (i,j) and (k,l) *)
    let add_antinodes_ij_kl ij anti_lst kl =
      (get_antinodes dim ij kl) @ anti_lst
    in
    (** Same with fixed (i,j) and any (k,l) from coordinates *)
    let add_antinodes_ij anti_lst ij =
      List.fold_left (add_antinodes_ij_kl ij) anti_lst coordinates
    in
    (* for each ij in coordinates :
        for each kl in coordinates : 
          add antinodes *)
    List.fold_left add_antinodes_ij anti_lst coordinates
  in
  
  (* Computes the antinodes for every frequency, then sort_uniq it. *)
  Array.fold_left add_antinodes [] antennas 
  |> List.sort_uniq Stdlib.compare
  |> List.length




(* Part 2 *)

(** Returns greatest common divisor of a and b *)
let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)


(** Returns the list of the inmap antinodes associated to (i,j) and (k,l);
  * using part1 ruleset. *)
let part2_antinodes dim (i,j) (k,l) =
  (* To compute antinodes associated to (i,j) and (k,l) :
     - compute (a,b) = (i,j) - (k,l)
     - simplify it by gcd, name (dx, dy) the result
     - antinodes are all inmap (i,j) + n.(dx,dy) with n>=0 integer
       (n<=0 will be computed in (k,l) and (i,j))
  *)
  if (i,j) = (k,l) then 
    [] 
  else
    let (a, b) = (i-k , j-l) in
    let (dx, dy) = (a / (gcd a b) , b / (gcd a b)) in
    (** Loop to compute all (i,j) + n.(dx,dy) with sign n = dir *)
    let rec loop dir (x,y) =
      if inmap dim (x,y) then 
        (x,y) :: ( loop dir (x + dir*dx, y + dir*dy) )
      else
        []
    in
    (loop 1 (i,j)) @ (loop (-1) (i,j))
    |> List.filter (inmap dim)




(** main *)
let () =
  let dim, antennas = read_input () in
  let answer_part1 = number_of_antinodes part1_antinodes dim antennas in
  let answer_part2 = number_of_antinodes part2_antinodes dim antennas in
  Printf.printf "%d\n%d\n" (answer_part1) (answer_part2)
