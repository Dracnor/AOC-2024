(* Day X : title *)

(** input_file name is read from argv *)
let input_file = 
  if Array.length Sys.argv < 2 then 
    failwith ("Usage : " ^ Sys.argv.(0) ^ " input_file")
  else
    Sys.argv.(1)

let read_input () =
  failwith "TODO"
  


(* Part 1 *)

(* Explain here if necessary *)

let part1 input =
  -666



(* Part 2 *)

(* Explain (very smartly) here if necessary *)

let part2 input =
  -666
  
  


(** main *)
let () =
  let input = read_input () in
  Printf.printf "%d\n%d\n" (part1 input) (part2 input)


