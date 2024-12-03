(* Day3 : Mull It Over *)

(* I'll use regex from the Re module. Compilation line :
      ocamlopt -package re -linkpkg mullItOver.ml
   NB : yes it would be cleaner with dune. I'm lazy.
*)

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


(* part 1 *)

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
  Re.Seq.matches regex_mul memory
  |> Seq.map multiply
  |> Seq.fold_left (+) 0


(* part 2 *)

(* I'll add two new regex, use the union of them, and memorise
   the enabled/disabled state as I go through matches.
   I would have loved to do it as a big regexp, but Re doesn't
   allow the complementary of a regexp :'( *)


(** Regex that matches a mul, a "do()" or a "don't()" *)
let regex_part2 =
  let open Re in
  [ seq [str "mul("; rep1 digit; str ","; rep1 digit; str ")"];
    str "do()";
    str "don't()"
  ]
  |> alt
  |> compile


(** Does part2. memory is the whole input *)
let part2 memory =

  (** Updates sum (sum of previous mul) and enabled 
    * according to a regex_part2 matching.
    * Will be folded. *)
  let update (sum, enabled) = function
    | "do()" -> (sum, true)
    | "don't()" -> (sum, false)
    | mul_string when enabled -> (sum + multiply mul_string, enabled)
    | _ -> (sum, enabled) (* disabled mul_string *)
  in
  Re.Seq.matches regex_part2 memory
  |> Seq.fold_left update (0, true)
  |> fst

  
(** main *)
let () =
  let memory = read_input () in
  Printf.printf "%d\n%d\n" (part1 memory) (part2 memory)
