(* 

HOMEWORK 4

Due: Wed Mar 2, 2022 (23h59)

Name: 

Email:

Remarks, if any:

*)


(*
 *
 * Please fill in this file with your solutions and submit it
 *
 * The functions below are stubs that you should replace with your
 * own implementation.
 *
 * PLEASE DO NOT CHANGE THE TYPES IN THE STUBS BELOW.
 * Doing so risks making it impossible for me to test your code.
 *
 * Always make sure you can #use this file in a FRESH OCaml shell
 * before submitting it. It has to load without any errors.
 *
 *)



(*
 * Type for deterministic Turing machines
 *
 * States are integers
 *)

type tm = {
    states : int list;
    input_alphabet : char list;
    tape_alphabet : char list;
    delta : (int * char) -> (int * char * int);   (* -1 = Left, 1 = Right *)
    start : int;
    accept : int;
    reject : int
  }

type config = {
    state : int;
    tape: char list;
    position: int
  }


(* 
 * Some sample deterministic Turing machines
 *
 * asbs is the regular language {a^m b^n | m,n >= 0}
 * anbn is the non-regular language {a^n b^n | n >= 0}
 * anbncn is the non-regular language {a^n b^n c^n | n >= 0}
 *
 *)

let asbs =
  let d inp = (match inp with
               | (1, 'a') -> (1, 'a', 1)
               | (1, 'b') -> (10, 'b', 1)
               | (1, '_') -> (777, '_', 1)
               | (10, 'b') -> (10, 'b', 1)
               | (10, '_') -> (777, '_', 1)
               | (777, 'a') -> (777, 'a', 1)
               | (777, 'b') -> (777, 'b', 1)
               | (777, '_') -> (777, '_', 1)
               | (_,c) -> (666,c,1))
  in { states = [1; 10; 777; 666];
       input_alphabet = ['a';'b'];
       tape_alphabet = ['a';'b';'_'];
       start = 1;
       accept = 777;
       reject = 666;
       delta = d }

let anbn =
  let d inp = (match inp with
               | (1, 'a') -> (2, 'X', 1)
               | (1, '_') -> (777, '_', 1)
               | (2, 'a') -> (2, 'a', 1)
               | (2, 'Y') -> (2, 'Y', 1)
               | (2, 'b') -> (4, 'Y', -1)
               | (4, 'Y') -> (4, 'Y', -1)
               | (4, 'a') -> (7, 'a', -1)
               | (4, 'X') -> (6, 'X', 1)
               | (6, 'Y') -> (6, 'Y', 1)
               | (6, '_') -> (777, '_', 1)
               | (7, 'a') -> (7, 'a', -1)
               | (7, 'X') -> (1, 'X', 1)
               | (_, c) -> (666, c, 1))
  in { states = [1; 2; 4; 6; 7; 777; 666];
       input_alphabet = ['a';'b'];
       tape_alphabet = ['a';'b';'X';'Y';'_'];
       start = 1;
       accept = 777;
       reject = 666;
       delta = d }
   

let anbncn =
  let d inp = (match inp with
               | (1, 'a') -> (2, 'X', 1)
               | (1, '_') -> (777, '_', 1)
               | (2, 'a') -> (2, 'a', 1)
               | (2, 'Y') -> (2, 'Y', 1)
               | (2, 'b') -> (3, 'Y', 1)
               | (3, 'b') -> (3, 'b', 1)
               | (3, 'Z') -> (3, 'Z', 1)
               | (3, 'c') -> (4, 'Z', -1)
               | (4, 'Z') -> (4, 'Z', -1)
               | (4, 'Y') -> (5, 'Y', -1)
               | (4, 'b') -> (7, 'b', -1)
               | (5, 'Y') -> (5, 'Y', -1)
               | (5, 'X') -> (6, 'X', 1)
               | (6, 'Y') -> (6, 'Y', 1)
               | (6, 'Z') -> (6, 'Z', 1)
               | (6, '_') -> (777, '_', 1)
               | (7, 'b') -> (7, 'b', -1)
               | (7, 'Y') -> (7, 'Y', -1)
               | (7, 'a') -> (8, 'a', -1)
               | (8, 'a') -> (8, 'a', -1)
               | (8, 'X') -> (1, 'X', 1)
               | (_, c) -> (666, c, 1))
  in { states = [1; 2; 3; 4; 5; 6; 7; 8; 666; 777];
       input_alphabet = ['a';'b';'c'];
       tape_alphabet = ['a';'b';'c';'X';'Y';'Z';'_'];
       start = 1;
       accept = 777;
       reject = 666;
       delta = d }


(*
 * Helper functions
 *
 *   explode : string -> char list
 *      return the list of characters making up a string
 *
 *   printConfig: tm -> config -> 'a -> 'a
 *      print a configuration (including newline) to standard output
 *      and return a value
 * 
 *   validateStates : tm -> bool
 *      check that all the states reachable from the start state
 *      appear in the set of states of the Turing machine
 *
 *)

let explode (str: string): char list = 
  let rec acc (index, result) = 
    if (index < 0) then
      result
    else
      acc (index - 1, (String.get str index) :: result)
  in
  acc (String.length (str) - 1, [])

let printConfig (m: tm) (c: config): unit =
  let maxStateLength a r = max (String.length (string_of_int a)) r in
  let mw = List.fold_right maxStateLength m.states 0 in
  let padding = max 0 (c.position + 1 - List.length c.tape) in
  let rec mkBlank k =
    match k with
    | 0 -> []
    | _ -> '_' :: (mkBlank(k - 1)) in
  let tape' : char list = c.tape @ (mkBlank padding) in
  let str_state = string_of_int c.state in
  let _ = print_string (String.sub (str_state ^ (String.make mw ' ')) 0 mw) in
  let _ = print_string "  "  in
  let _ = List.iteri (fun i sym -> 
              if (i = c.position) then
                Printf.printf "[%c]" sym
	      else
                Printf.printf " %c " sym) tape'  in
  print_newline ()


let validateStates (m: tm):bool =
  let rec addTransitions q syms queue =
    match syms with
    | [] -> queue
    | sym::syms' -> let (p, _, _) = m.delta (q, sym) in
                    addTransitions q syms' (p :: queue) in
  let rec loop seen queue =
    match queue with

    | [] -> true
    | q::qs -> if not (List.mem q m.states) then
                 let str_q = string_of_int q in
                 failwith ("Reachable state " ^ str_q ^ " not defined in states list")
               else if not (List.mem q seen) then
                 let str_q = string_of_int q in
                 let _ = print_string ("Following state " ^ str_q ^ "\n") in
                 loop (q :: seen) (addTransitions q m.tape_alphabet qs)
               else
                 loop seen qs  in
  loop [] [m.start]
  

  
(* QUESTION 1 *)


let startConfig (m:tm) (w:string): config = 
  failwith "Not implemented"


let acceptConfig (m: tm) (c: config): bool = 
  failwith "Not implemented"


let rejectConfig (m: tm) (c: config): bool = 
  failwith "Not implemented"


let replace_nth (lst: 'a list) (n: int) (s: 'a): 'a list = 
  failwith "Not implemented"


let step (m: tm) (c: config): config = 
  failwith "Not implemented"


let run (m: tm) (w: string): bool = 
  failwith "Not implemented"

  


(* QUESTION 2 *)


let dummyTM = { states = [0; 1];
		input_alphabet = ['x'];
		tape_alphabet = ['x'; '_'];
		start = 0;
		accept = 0;
		reject = 1;
		delta = (fun (x, y) -> (x, y, 0))}
            

let tm_ab3 : tm = dummyTM

                     
let tm_pal : tm = dummyTM

                      
let tm_not : tm = dummyTM


let tm_and : tm = dummyTM


let tm_plus1 : tm = dummyTM

