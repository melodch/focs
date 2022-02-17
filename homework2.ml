(* 

HOMEWORK 2

Due: Wed Feb 16, 2022 (23h59)

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
 * String <-> characters utility functions:
 *
 *   explode : string -> char list
 *      returns the list of characters making up a string
 *
 *   implode : char list -> string
 *      concatenates the list of characters into a string
 *
 *)

let explode (str) = 
  let rec acc (index,result) = 
    if (index<0) then
      result
    else
      acc(index-1, (String.get str index)::result)
  in
    acc(String.length(str)-1, [])

let implode cs = 
  List.fold_right (fun a r -> (String.make 1 a)^r) cs ""



(*
 *  The type of a finite automaton
 * 
 *)

type fa = { states: int list;
     	    alphabet: char list;
            delta: (int * char * int) list;
            start : int;
            final : int list }


(* 
 * Sample Deterministic Finite Automata
 *
 * The first accepts the language of all strings over {x,y} 
 * with a multiple-of-3 number of x's.
 *
 * The second accepts the language of all strings over {x,y,z}
 * that start with x and end with z.
 *
 *)

let faMod3X = { 
    states = [1; 2; 3];
    alphabet = ['x'; 'y'];
    delta = [ (1,'x',2);
	      (2,'x',3);
	      (3,'x',1);
	      (1,'y',1);
	      (2,'y',2);
	      (3,'y',3); ];
    start = 1;
    final = [1]
  } 

let faStartXEndZ = {
    states = [0; 1; 2; 3];
    alphabet = ['x'; 'y'; 'z'];
    delta = [ (0, 'x', 1);
              (0, 'y', 3);
              (0, 'z', 3);
              (1, 'x', 1);
              (1, 'y', 1);
              (1, 'z', 2);
              (2, 'x', 1);
              (2, 'y', 1);
              (2, 'z', 2);
              (3, 'x', 3);
              (3, 'y', 3);
              (3, 'z', 3);];
    start = 0;
    final = [2]
  } 



(* Helper function to count how many transitions from a
   given state there are for a given symbol
 *)

let countTransitions (m:fa) (q:int) (a:char):int =
  let rec loop trs =
    match trs with
    | [] -> 0
    | t :: trs' -> (match t with
                      (q', a', p') -> if q = q' && a = a'
                                      then 1 + loop trs'
                                      else loop trs') in
  loop m.delta




  
(* QUESTION 1 *)


let rec isFinal (m:fa) (q:int):bool = 
  if m.final = [q] then true
  else false

let rec makeTransition trs q a =
  match trs with
  | [] -> 0
  | t :: trs' -> (match t with
                    (q', a', p') -> if q = q' && a = a'
                                    then p'
                                    else makeTransition trs' q a)

let followSymbol (m:fa) (q:int) (a:char):int =
  if (countTransitions m q a) = 0 || (countTransitions m q a) > 1 then failwith "No/more than one transition(s) possible"
  else makeTransition m.delta q a

let followString (m:fa) (q:int) (syms: char list):int =
  let rec loop symbols start =
    match symbols with
    | [] -> start
    | x :: xs' -> loop xs' (makeTransition m.delta start x) in
  loop syms q

let followString (m:fa) (q:int) (syms: char list):int =
  let rec loop symbols start =
    match symbols with
    | [] -> start
    | x :: xs' -> if xs' = [] then makeTransition m.delta start x
                  else loop xs' (makeTransition m.delta start x) in
  loop syms q
  
let rec accept (m:fa) (input:string):bool = 
  let symbols = explode input in
  isFinal m (followString m m.start symbols)


(* QUESTION 2 *)

(* Right now, these are dummy finite automata that always reject. 
   Replace by your own *)

let dummy : fa = { states = [0];
                   alphabet = [];
                   delta = [];
                   start = 0;
                   final = []}

let fa_a : fa = { states = [1; 2; 3];
                  alphabet = ['a';'b';'c'];
                  delta = [(1,'a',3);
                           (1,'b',3);
                           (1,'c',3);
                           (2,'a',1);
                           (2,'b',1);
                           (2,'c',1);
                           (3,'a',2);
                           (3,'b',2);
                           (3,'c',2);];
                  start = 1;
                  final = [2;3]}


let fa_b : fa = { states = [1; 2; 3];
                  alphabet = ['a';'b';'c'];
                  delta = [(1,'a',2);
                           (1,'c',1);
                           (2,'a',3);
                           (2,'c',2);
                           (3,'c',3);];
                  start = 1;
                  final = [3]}
                        

let fa_c : fa = { states = [1; 2; 3; 4; 5; 6];
                  alphabet = ['a';'b';'c'];
                  delta = [(1,'a',2);
                           (1,'b',3);
                           (1,'c',1);
                           (2,'a',5);
                           (2,'b',4);
                           (2,'c',2);
                           (3,'a',6);
                           (3,'c',3);
                           (4,'a',7);
                           (4,'c',4);
                           (5,'b',7);
                           (5,'c',5);
                           (6,'a',7);
                           (6,'c',6);
                           (7,'c',7);];
                  start = 1;
                  final = [7]}
                    

let fa_d : fa = { states = [1; 2; 3; 4; 5; 6];
                  alphabet = ['a';'b';'c'];
                  delta = [(1,'a',5);
                           (1,'b',2);
                           (1,'c',1);
                           (2,'a',3);
                           (2,'b',4);
                           (2,'c',2);
                           (3,'a',2);
                           (3,'b',5);
                           (3,'c',3);
                           (4,'a',5);
                           (4,'b',2);
                           (4,'c',4);
                           (5,'a',4);
                           (5,'b',6);
                           (5,'c',5);
                           (6,'a',2);
                           (6,'b',5);
                           (6,'c',6);];
                  start = 1;
                  final = [5]}


let fa_e : fa = { states = [1; 2; 3; 4; 5; 6; 7; 8; 9];
                  alphabet = ['a';'b';'c'];
                  delta = [(1,'a',2);
                           (1,'b',4);
                           (1,'c',7);
                           (2,'a',9);
                           (2,'b',3);
                           (2,'c',8);
                           (3,'a',4);
                           (3,'b',2);
                           (3,'c',5);
                           (4,'a',3);
                           (4,'b',9);
                           (4,'c',6);
                           (5,'a',6);
                           (5,'b',8);
                           (6,'a',5);
                           (6,'b',7);
                           (7,'a',8);
                           (7,'b',6);
                           (8,'a',7);
                           (8,'b',5);
                           (9,'a',2);
                           (9,'b',4);
                           (9,'c',7);];
                  start = 1;
                  final = [8]}

              

  
(* This function is the base function that basically loops through all
 * strings  of length up to n, and prints those that are accepted by the
 * finite automaton.
 *
 * This is being way too clever to try to not blow the stack 
 * while enumerating all strings up to a given length. Basically.
 * we enumerate all integer, convert them to base K (where K is the
 * size of the alphabet) and then replace every digit base K by the
 * letter of the alphabet at the corresponding index in the alphabet. 
 *
 * The key is that we can enumerate integers super easily
 *
 *)

let lang (m:fa) (n:int) = 

  let rec expt a n = if n <= 0 then 1 else a*(expt a (n-1)) in
  
  let rec take n default l = 
    if n <= 0 then []
    else (match l with
          | [] -> default::(take (n-1) default l)
          | x::xs -> x::(take (n-1) default xs)) in
  
  let to_base_n base size n = 
    let rec loop n = 
      if n <= 0 then []
      else if n mod base = 0 then 0::(loop (n / base))
      else (n mod base)::(loop ((n - n mod base) / base))  in
    take size 0 (loop n)  in
  
  let to_string alphabet size n = 
    let base = List.length alphabet in
    let num_base = to_base_n base size n in
    implode (List.map (fun i -> List.nth alphabet i) num_base) in
  
    if n < 0 then ()
    else
      let print_str s = if s = "" then print_string "  <epsilon>\n"
  	              else print_string ("  "^s^"\n")  in
      let rec loop i = 
        if i <= n then 
  	  let ts = to_string m.alphabet i  in
  	  let bound = expt (List.length m.alphabet) i in
  	  let rec loop2 j = 
  	    if j < bound then (if accept m (ts j) 
                                 then print_str (ts j)
                               else ();
  			       loop2 (j+1))
  	    else ()  in
  	  (loop2 0; loop (i+1))
        else ()  in
      loop 0

