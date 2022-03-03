(* 

HOMEWORK 4

Due: Wed Mar 2, 2022 (23h59)

Name: Melody Chiu

Email: cchiu@olin.edu

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
  {
    state = 1;
    tape = explode w;
    position = 0;
  }

let acceptConfig (m: tm) (c: config): bool = 
  c.state = m.accept

let rejectConfig (m: tm) (c: config): bool = 
  c.state = m.reject

let rec replace_nth (lst: 'a list) (n: int) (s: 'a): 'a list = 
  match lst with
  | [] -> []
  | hd::tl -> if n = 0 then s::tl
              else hd::(replace_nth tl (n-1) s)

let rec step (m: tm) (c: config): config = 
  let tape_length = List.length c.tape in
  let tape = if c.position < tape_length then c.tape
                                else c.tape@['_'] in
  match m.delta(c.state, (List.nth tape c.position)) with
  | (x,y,z) -> { state = x;
                 tape = replace_nth c.tape c.position y;
                 position = max (c.position + z) 0}

let run (m: tm) (w: string): bool = 
  let config = startConfig m w in
    let rec loop m c =
      let _ = printConfig m (step m c) in
        if acceptConfig m (step m c) then true
        else if rejectConfig m (step m c) then false
        else loop m (step m c) in
  loop m config
  


(* QUESTION 2 *)


let dummyTM = { states = [0; 1];
		input_alphabet = ['x'];
		tape_alphabet = ['x'; '_'];
		start = 0;
		accept = 0;
		reject = 1;
		delta = (fun (x, y) -> (x, y, 0))}
            

let tm_ab3 : tm =
  let d inp = (match inp with
               | (1, 'a') -> (2, 'X', 1)
               | (1, '_') -> (777, '_', 1)
               | (2, 'a') -> (2, 'a', 1)
               | (2, 'Y') -> (2, 'Y', 1)
               | (2, 'b') -> (10, 'Y', 1)
               | (10, 'b') -> (11, 'Y', 1)
               | (11, 'b') -> (4, 'Y', 0)
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

let tm_pal : tm =
  let d inp = (match inp with
               | (1, 'a') -> (2, '_', 1)
               | (1, 'b') -> (3, '_', 1)
               | (1, '_') -> (777, '_', 1)
               | (2, 'a') -> (2, 'a', 1)
               | (2, 'b') -> (2, 'b', 1)
               | (2, '_') -> (4, '_', -1)
               | (3, 'a') -> (3, 'a', 1)
               | (3, 'b') -> (3, 'b', 1)
               | (3, '_') -> (5, '_', -1)
               | (4, 'a') -> (6, '_', -1)
               | (4, 'b') -> (666, 'b', 1)
               | (4, '_') -> (777, '_', 0)
               | (5, 'a') -> (666, 'a', 1)
               | (5, 'b') -> (7, '_', -1)
               | (5, '_') -> (777, '_', 0)
               | (6, 'a') -> (6, 'a', -1)
               | (6, 'b') -> (6, 'b', -1)
               | (6, '_') -> (1, '_', 1)
               | (7, 'a') -> (7, 'a', -1)
               | (7, 'b') -> (7, 'b', -1)
               | (7, '_') -> (1, '_', 1)
               | (_, c) -> (666, c, 1))
  in { states = [1; 2; 4; 6; 7; 777; 666];
       input_alphabet = ['a';'b'];
       tape_alphabet = ['a';'b';'_'];
       start = 1;
       accept = 777;
       reject = 666;
       delta = d }

                      
let tm_not : tm =
  let d inp = (match inp with
               | (1, '#') -> (2, '_', 1)
               | (2, '0') -> (3, '#', 1)
               | (2, '1') -> (5, '#', 1)
               | (2, '#') -> (777, '#', 0)
               | (3, '0') -> (3, '0', 1)
               | (3, '1') -> (3, '1', 1)
               | (3, '#') -> (4, '#', 1)
               | (4, '0') -> (666, '0', 0)
               | (4, '1') -> (7, '#', -1)
               | (4, '#') -> (4, '#', 1)
               | (4, '_') -> (666, '_', 1)
               | (5, '0') -> (5, '0', 1)
               | (5, '1') -> (5, '1', 1)
               | (5, '#') -> (6, '#', 1)
               | (6, '0') -> (7, '#', -1)
               | (6, '1') -> (666, '1', 0)
               | (6, '#') -> (6, '#', 1)
               | (6, '_') -> (666, '_', 1)
               | (7, '0') -> (7, '0', -1)
               | (7, '1') -> (7, '1', -1)
               | (7, '#') -> (8, '#', 0)
               | (8, '0') -> (9, '0', -1)
               | (8, '1') -> (9, '1', -1)
               | (8, '#') -> (8, '#', -1)
               | (8, '_') -> (10, '_', 1)
               | (9, '0') -> (9, '0', -1)
               | (9, '1') -> (9, '1', -1)
               | (9, '#') -> (2, '#', 1)
               | (10, '0') -> (666, '0', 1)
               | (10, '1') -> (666, '1', 1)
               | (10, '#') -> (10, '#', 1)
               | (_, c) -> (777, c, 1))
  in { states = [1; 2; 4; 5; 6; 7; 8; 9; 10; 777; 666];
       input_alphabet = ['0';'1'];
       tape_alphabet = ['0';'1';'#';'_'];
       start = 1;
       accept = 777;
       reject = 666;
       delta = d }

let tm_and : tm =
  let d inp = (match inp with
               | (1, '#') -> (2, '_', 1)
               | (2, '0') -> (3, '#', 1)
               | (2, '1') -> (4, '#', 1)
               | (2, '#') -> (2, '#', 1)
               | (2, '_') -> (666, '_', 0)
               | (3, '0') -> (3, '0', 1)
               | (3, '1') -> (3, '1', 1)
               | (3, '#') -> (20, '#', 1)
               | (4, '0') -> (4, '0', 1)
               | (4, '1') -> (4, '1', 1)
               | (4, '#') -> (30, '#', 1)
               | (5, '0') -> (5, '0', -1)
               | (5, '1') -> (5, '1', -1)
               | (5, '#') -> (5, '#', -1)
               | (5, '_') -> (10, '_', 1)
               | (10, '0') -> (2, '0', 0)
               | (10, '1') -> (2, '1', 0)
               | (10, '#') -> (10, '#', 1)
               | (10, '_') -> (777, '_', 0)
               | (30, '0') -> (31, '#', 1)
               | (30, '1') -> (33, '#', 1)
               | (30, '#') -> (30, '#', 1)
               | (31, '0') -> (31, '0', 1)
               | (31, '1') -> (31, '1', 1)
               | (31, '#') -> (32, '#', 1)
               | (32, '0') -> (5, '#', -1)
               | (32, '1') -> (666, '#', 0)
               | (32, '#') -> (32, '#', 1)
               | (33, '0') -> (33, '0', 1)
               | (33, '1') -> (33, '1', 1)
               | (33, '#') -> (34, '#', 1)
               | (34, '0') -> (666, '#', 0)
               | (34, '1') -> (5, '#', -1)
               | (34, '#') -> (34, '#', 1)
               | (20, '0') -> (21, '#', 1)
               | (20, '1') -> (21, '#', 1)
               | (20, '#') -> (20, '#', 1)
               | (20, '_') -> (666, '#', 0)
               | (21, '0') -> (21, '0', 1)
               | (21, '1') -> (21, '1', 1)
               | (21, '#') -> (22, '#', 1)
               | (21, '_') -> (666, '#', 0)
               | (22, '0') -> (5, '#', -1)
               | (22, '1') -> (666, '_', 1)
               | (22, '#') -> (22, '#', 1)
               | (_, c) -> (777, c, 1))
  in { states = [1; 2; 4; 5; 10; 20; 21; 22 30; 31; 32; 33; 34; 777; 666];
       input_alphabet = ['a';'b'; '#];
       tape_alphabet = ['a';'b';'#';'_'];
       start = 1;
       accept = 777;
       reject = 666;
       delta = d }

let tm_plus1 : tm =
  let d inp = (match inp with
               | (1, '#') -> (2, '_', 1)
               | (2, '0') -> (2, '0', 1)
               | (2, '1') -> (2, '1', 1)
               | (2, '#') -> (3, '#', 1)
               | (2, '_') -> (666, '_', 0)
               | (3, '0') -> (3, '0', 1)
               | (3, '1') -> (3, '1', 1)
               | (3, '#') -> (4, '#', 1)
               | (3, '_') -> (5, '_', -1)
               | (4, '0') -> (4, '0', 1)
               | (4, '1') -> (4, '1', 1)
               | (4, '#') -> (666, '#', 0)
               | (4, '_') -> (666, '_', 0)
               | (5, '0') -> (30, '_', -1)
               | (5, '1') -> (20, '_', -1)
               | (8, '0') -> (8, '0', 1)
               | (8, '1') -> (8, '1', 1)
               | (8, '#') -> (8, '#', 1)
               | (8, '_') -> (40, '_', -1)
               | (9, '0') -> (9, '0', 1)
               | (9, '1') -> (9, '1', 1)
               | (9, '#') -> (9, '#', 1)
               | (9, '_') -> (50, '_', -1)
               | (20, '0') -> (20, '0', -1)
               | (20, '1') -> (20, '1', -1)
               | (20, '#') -> (21, '#', -1)
               | (21, '0') -> (8, '#', 1)
               | (21, '1') -> (666, '_', 0)
               | (21, '#') -> (21, '#', -1)
               | (30, '0') -> (30, '0', -1)
               | (30, '1') -> (30, '1', -1)
               | (30, '#') -> (31, '#', -1)
               | (31, '0') -> (666, '_', 0)
               | (31, '1') -> (9, '#', 1)
               | (31, '#') -> (31, '#', -1)
               | (50, '0') -> (51, '_', -1)
               | (50, '1') -> (20, '_', -1)
               | (50, '#') -> (666, '_', 0)
               | (51, '0') -> (51, '0', -1)
               | (51, '1') -> (51, '1', -1)
               | (51, '#') -> (52, '#', -1)
               | (52, '0') -> (666, '_', 0)
               | (52, '1') -> (9, '#', 1)
               | (52, '#') -> (52, '#', -1)
               | (40, '0') -> (41, '_', -1)
               | (40, '1') -> (42, '_', -1)
               | (42, '0') -> (42, '0', -1)
               | (42, '1') -> (42, '1', -1)
               | (42, '#') -> (44, '#', -1)
               | (41, '0') -> (41, '0', -1)
               | (41, '1') -> (41, '1', -1)
               | (41, '#') -> (43, '#', -1)
               | (43, '0') -> (8, '#', 1)
               | (43, '1') -> (666, '_', 0)
               | (43, '#') -> (43, '#', -1)
               | (44, '1') -> (8, '#', 1)
               | (44, '0') -> (666, '_', 0)
               | (44, '#') -> (44, '#', -1)
               | (44, '_') -> (666, '_', 0)
               | (_, c) -> (777, c, 1))
  in { states = [1; 2; 4; 5; 8; 9; 20; 21; 30; 31; 41; 42; 43; 44; 50; 51; 52; 777; 666];
       input_alphabet = ['a';'b'; '#];
       tape_alphabet = ['a';'b';'#';'_'];
       start = 1;
       accept = 777;
       reject = 666;
       delta = d }
