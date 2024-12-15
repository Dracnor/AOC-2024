(* Day 12 : Garden Groups *)

(** input_file name is read from argv *)
let input_file = 
  if Array.length Sys.argv < 2 then 
    failwith ("Usage : " ^ Sys.argv.(0) ^ " input_file")
  else
    Sys.argv.(1)


(** Reads the input as a char matrix *)
let read_input () =
  let i_c = open_in input_file in
  let first_line = input_line i_c in
  let len = String.length first_line in
  let init_line i =
    let str_line = if i = 0 then first_line else input_line i_c in
    Array.init len (String.get str_line)
  in
  let matrix = Array.init len init_line in
  close_in i_c;
  matrix


(* I'll represent a region using the Set of its coordinates *)
module Coo = struct
  type t = int*int
  let compare (x0,y0) (x1,y1) = 
    match Int.compare x0 x1 with 
    | 0 -> Int.compare y0 y1 
    | r -> r
end
(** Sets of int*int *)
module CooSet = Set.Make(Coo)



(* Part 1 *)

(* I'll first partition the coordinates into the list of regions 
   (which are CooSets). To identify one region, I use a dfs.

   Then work on each region to compute its price. 
*)

(** Checks if garden.(i).(j) = plant *)
let has_plant garden plant (i,j) =
  try garden.(i).(j) = plant with Invalid_argument _ -> false


(** Extends set so that it holds the region of (i,j).
  * if (i,j) has laready been seen by the algorithm, doesn't change set.
  * Marks (i,j) as seen in seen.
 *)
let rec identify garden seen set (i,j) =
  if seen.(i).(j) then set 
  else begin 
    seen.(i).(j) <- true;
    let set_with_ij = CooSet.add (i,j) set in
    [ (i-1, j); (i+1,j); (i, j-1); (i,j+1) ]
    |> List.filter (has_plant garden garden.(i).(j))
    |> List.fold_left (identify garden seen) set_with_ij
  end


(** Compute the list of regions *)
let list_of_regions garden =
  let len = Array.length garden in
  let seen = Array.make_matrix len len false in
  let regions = ref [] in
  for i = 0 to len-1 do
    for j = 0 to len-1 do
      if not seen.(i).(j) then
        regions := (identify garden seen CooSet.empty (i,j)) :: !regions
    done
  done;
  !regions


(* To compute the perimeter, count for each (i,j) of the set the number
   of adjacent cases (even not existing ones) that are not in the region *)

(** Computes the price of a region according to price1 ruleset.
  * Adds it to total. *)
let price_part1 total set =
  let area = CooSet.cardinal set in
    
  (** Adds to perim the number of edges around (i,j) *)
  let compute_perimeter (i,j) perim =
    let nb_adja_in_set =  [ (i-1, j); (i+1,j); (i,j-1); (i,j+1) ]
                          |> List.filter (fun (k,l) -> CooSet.mem (k,l) set)
                          |> List.length in
    perim + (4 - nb_adja_in_set)          
  in
  let perimeter = CooSet.fold compute_perimeter set 0 in
  total + area * perimeter



(* Part 2 *)

(* To count the number of sides, count the corners (same number) *)

(** Counts the number of corners around (i,j),
  * and adds it to total *)
let nb_corner_around garden (i,j) total =
  
  (** Checks if a coordinate has the same plant as (i,j) *) 
  let check = has_plant garden garden.(i).(j) 
  in
  (* I'll call a the variable "top left has same plant as (i,j)" and so on :
            a   b   c
            d (i,j) e
            f   g   h      *)
  let a = check (i-1, j-1) in
  let b = check (i-1, j)   in
  let c = check (i-1, j+1) in
  let d = check (i,   j-1) in
  let e = check (i,   j+1) in
  let f = check (i+1, j-1) in
  let g = check (i+1, j)   in
  let h = check (i+1, j+1) in 
  
  (* if P =garden.(i).(j) and X anything that's not P, a corner looks like :
     X   P           _   X   
     p (i,j)   or    X  (i,j)      or  one of the three rotations of it.
     
     I'll each time enumerate the adjacent in direct order.
  *)
  [ b && not a && d ; not b && not d;  (* top left corner *)   
    d && not f && g ; not d && not g;  (* bottom left corner *)
    g && not h && e ; not g && not e;  (* bottom right corner *)
    e && not c && b ; not e && not b ] (* top right corner *)
  |> List.map Bool.to_int
  |> List.fold_left (+) total


(** Computes the price of a region according to price2 ruleset.
  * Adds it to total. *)
let price_part2 garden total set =
  let area = CooSet.cardinal set in
  let nb_sides = CooSet.fold (nb_corner_around garden) set 0 in
  total + area * nb_sides
  
  


(** main *)
let () =
  let garden = read_input () in
  let regions = list_of_regions garden in
  let answer1 = List.fold_left price_part1 0 regions in
  let answer2 = List.fold_left (price_part2 garden) 0 regions in
  Printf.printf "%d\n%d\n" answer1 answer2
