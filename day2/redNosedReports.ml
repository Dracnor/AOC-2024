(* Day2 : red -nosed reports *)

let input_file = "input.in"

(* NB : the reports are at most of length 8.
   So I don't really care about complexity. *)


(** Converts the string of a line into the associated int list *)
let report_of_line str =
  String.split_on_char ' ' str
  |> List.map int_of_string
  

(** Checks if a report (int list) is safe, meaning
  * that it is sorted and adjacent value differs by at least 1
  * and at most 3. *)
let is_safe report =

  (** Recursvily goes through a list and ensure it is safe
    * according to the order cmp *)
  let rec loop cmp = function
    | h0 :: h1 :: q -> cmp h0 h1 
                       && 1 <= abs (h0-h1) && abs (h0-h1) <= 3 (* technically the 1<= is useless if cmp is a strict comparison *)
                       && loop cmp (h1::q)
    | [ _ ]         -> true
    | []            -> failwith "is_safe : unreachable case"
  in
  match report with
  | h0::h1::_ -> if h0 < h1 then loop (<) report else loop (>) report
  | _ -> failwith "is-safe : report of len 0/1"


(* Part2 : as I said, I don't care about complexity : I'll n² *)

(** Remove the ith element of a list. Not tl-rec. *)
let rec remove_ith i = function
    | h :: t -> if i=0 then t else h :: (remove_ith (i-1) t)
    | [] -> []


(** Checks if a report (int list) is safe up to one tolerance 
  * meaning part2 *)
let is_tolerable report =

  (** Same as before *)
  let rec loop cmp = function
    | h0 :: h1 :: q -> cmp h0 h1 
                       && 1 <= abs (h0-h1) && abs (h0-h1) <= 3
                       && loop cmp (h1::q)
    | [ _ ]         -> true
    | []            -> failwith "is-tolerable : unreachable case"
  in
  let test lst =
    match lst with
    | h0::h1::_ -> if h0 < h1 then loop (<) lst else loop (>) lst
    | _ -> failwith "is_tolerable : report of len 0 or 1"
  in
  let len = List.length report in
  Seq.init (len+1) (fun n -> remove_ith (len-n) report)
  |> Seq.exists test




(** main *)
let () =
  let input = open_in input_file in 
  let part1 = ref 0 in
  let part2 = ref 0 in
  let rec loop () =
    try
      let report = input_line input |> report_of_line in
      if is_safe report then incr part1;
      if is_tolerable report then incr part2;
      loop ()
    with End_of_file ->
      close_in input
   in
   loop ();
   print_int !part1;
   print_newline ();
   print_int !part2;
   print_newline ()
   
