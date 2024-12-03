(* Day3 : Mull It Over *)

let input_file = "input.in"

(** Returns the input and returns it as a big string *)
let read_input () =
  let input = open_in input_file in
  let rec loop str_list =
    try loop ((input_line input) :: str_list)
    with End_of_file -> List.rev str_list 
  in
  let str_list = loop [] in
  close_in input;
  String.concat "" str_list


(* I'll use regex from the Re module. Compilation line :
      ocamlopt -package re -linkpkg mullItOver.ml
   NB : yes it would be cleaner with dune. I'm lazy.
*)


(** Regex that matches mul([0-9]+,[0-9]+)  *) 
let regex_mul =
  Re.([str "mul("; rep1 digit; str ","; rep1 digit; str ")"])
  |> Re.seq
  |> Re.compile


(** Returns the result of a valid "mul(a,b)" string *)
let multiply mul_string =
  Scanf.sscanf mul_string "mul(%d,%d)" (fun a b -> a*b)
  

(** Does part1. memory is the whole input *)
let part1 memory =
  Re.Seq.matches regex_mul str
  |> Seq.map multiply
  |> Seq.fold_left (+) 0
  
  
let () =
  read_input ()
  |> part1
  |> print_int;
  print_newline ()











(* Older version of multiply. No longuer used. *)

(* Returns the result of a valid "mul(a,b)" string *)
let former_multiply str =
  (* a is str[start_a : index_comma[ *) 
  let start_a = 4 in (* a starts at str[4] *)
  let index_comma = String.index_from str 0 ',' in
  let length_a = index_comma - start_a in
  let a = String.sub str start_a length_a
          |> int_of_string
  in
  (* b is str[start_b : length str -1[ *)
  let start_b = index_comma +1 in
  let length_b = (String.length str -1) - start_b in
  let b = String.sub str start_b length_b 
          |> int_of_string
  in
  a*b

