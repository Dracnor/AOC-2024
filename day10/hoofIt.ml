(* Day 10 : hoofIt *)

(** input_file name is read from argv *)
let input_file =
  if Array.length Sys.argv < 2 then 
    failwith ("Usage : " ^ Sys.argv.(0) ^ " input_file")
  else
    Sys.argv.(1)


(** Returns the input as an int matrix *)
let read_input () =
  let i_c = open_in input_file in
  let first_line = input_line i_c in
  let len = String.length first_line in
  let init_line i =
    let str_line = if i = 0 then first_line else input_line i_c in
    Array.init len (fun j-> int_of_char str_line.[j] - int_of_char '0')
  in
  let matrix = Array.init len init_line in
  close_in i_c;
  matrix
  


(* Part 1 *)

(* WTF, part1 is harder than part2. *) 

(* I'll compute for each (i,j) the list of the coordinates
   of 9s that can be reach through a trail starting there *)


(** Merge two sorted list removing duplicates *)
let [@tail_mod_cons] rec merge l0 l1 =
  match l0,l1 with
  | [], lst | lst, [] -> lst
  | h0::t0, h1::t1 when h0 < h1 -> h0 :: (merge t0 l1)
  | h0::t0, h1::t1 when h0 > h1 -> h1 :: (merge l0 t1)
  | h0::t0, h1::t1 when h0 = h1 -> h0 :: (merge t0 t1)
  | _ -> assert false (* the order should be total *)


let part1 topo =
  let len = Array.length topo in
  
  (** Checks if the (next_, next_j) coordinates has height curr_height+1 *)
  let is_incr_height curr_height (next_i, next_j) =
    try topo.(next_i).(next_j) = curr_height + 1 
    with Invalid_argument _ -> false
  in
  let reachable = Array.make_matrix len len None in
  let rec compute_reachable (i,j) =
    match reachable.(i).(j) with
    | Some lst -> lst
    | None -> 
        let lst = if topo.(i).(j) = 9 then [(i,j)] else begin
          [(i-1,j); (i+1,j); (i,j-1); (i,j+1)]
          |> List.filter (is_incr_height topo.(i).(j))
          |> List.map compute_reachable (* lists are small so low mem cost *)
          |> List.fold_left merge [] end
        in
        reachable.(i).(j) <- Some lst;
        lst
  in
  let answer = ref 0 in
  for i = 0 to len -1 do
    for j = 0 to len -1 do
      let lst = compute_reachable (i,j) in
      if topo.(i).(j) = 0 then answer := !answer + List.length lst
    done
  done;
  !answer


(* Part 2 *)

(* Same as before but we don't memorise the list of reachable 9s,
   just the number of reachable 9s (counting duplicates since we can
   reach one by different paths) 
   
   I mean, it's litteraly the previous one but easier.
   I also could have changed the previous one and replaced merge by @ ,
   but I wanted to be sure that part2 was indeed easier.
   
   I'm frustrtated because part2 is easily writable in C, while part1 is
   harder to write to due the lists :'(
   *)

let part2 topo =
  let len = Array.length topo in
  
  (** Checks if the (next_, next_j) coordinates has height curr_height+1 *)
  let is_incr_height curr_height (next_i, next_j) =
    try topo.(next_i).(next_j) = curr_height + 1 
    with Invalid_argument _ -> false
  in
  let reachable = Array.make_matrix len len None in
  let rec compute_reachable (i,j) =
    match reachable.(i).(j) with
    | Some n -> n
    | None -> 
        let n = if topo.(i).(j) = 9 then 1 else begin
          [(i-1,j); (i+1,j); (i,j-1); (i,j+1)]
          |> List.filter (is_incr_height topo.(i).(j))
          |> List.map compute_reachable (* lists are small so low mem cost *)
          |> List.fold_left (+) 0 end
        in
        reachable.(i).(j) <- Some n;
        n
  in
  let answer = ref 0 in
  for i = 0 to len -1 do
    for j = 0 to len -1 do
      let n = compute_reachable (i,j) in
      if topo.(i).(j) = 0 then answer := !answer + n
    done
  done;
  !answer
  
  


(** main *)

let _ =
  let input = read_input () in
  Printf.printf "%d\n%d\n" (part1 input) (part2 input)


