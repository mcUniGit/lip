
(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)
             
let toklist_of_string s = 
  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [] in

  let tok_of_char c = match c with
      'A' -> A
    |'B' -> B
    |'=' -> X  
    |_ -> failwith "Wrong input" in

  List.map tok_of_char (explode s)  

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)

let valid l =
    
  let rec check x b l = match l with
      [] -> true
    |A::l -> not(x || b) && check x b l 
    |X::l -> (not b) && check true b l 
    |B::l -> check x true l in check false false l 

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)
let win l = 
  let winner = List.fold_left( fun acc t -> if t = A then acc - 1 else if t = B then acc + 1 else acc ) 0 l in

  if winner < 0 then A else if winner = 0 then X else B

(* val string_of_winner : token -> string *)
let string_of_winner w = match w with
    X -> "tie"
  |A -> "A"
  |B -> "B";;

