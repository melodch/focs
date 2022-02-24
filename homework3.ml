(* 

HOMEWORK 3

Due: Wed Feb 23, 2022 (23h59)

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


(********************* QUESTION 1 *********************)

let absNonZero (xs: int list): int list =
  List.map (fun x -> abs x) (List.filter (fun x -> x <> 0) xs)

let map_functions (fs: ('a -> 'b) list) (x: 'a) : 'b list =
  List.map (fun f -> f x) fs

let compose_all (fs:('a -> 'a) list) (x: 'a) : 'a =
  List.fold_right (fun x r -> x r) fs (fun x -> x)

let pairs1 (x: 'a) (ys: 'b list) : ('a * 'b) list =
  List.map (fun b -> (x, b)) ys

let pairs (xs:'a list) (ys:'b list) : ('a * 'b) list =
  let map_b x = List.map (fun b -> (x, b)) ys in
  List.flatten (List.map (map_b) xs)


(********************* QUESTION 2 *********************)

  
let cons_all (x:'a) (xss:'a list list) : 'a list list = 
  List.map (fun xs -> match xs with [] -> [x] | xs -> x::xs) xss

let prefixes (xs:'a list) : 'a list list = 
  List.fold_right (fun x r -> []::cons_all x r) xs [[]]

let cons_all_opp (x:'a) (xss:'a list list) : 'a list list = 
  List.map (fun xs -> match xs with [] -> [x] | xs -> xs@[x]) xss

let suffixes (xs:'a list) : 'a list list =
  List.fold_right (fun x r -> (cons_all_opp x r)@[[]]) (List.rev xs) [[]]

let splits (xs:'a list) : ('a list * 'a list) list =
  List.map2 (fun x y -> (x, y)) (prefixes xs) (suffixes xs)
  
let inject (x:'a) (xs:'a list) : 'a list list =
  List.map (fun (a,b) ->  a@[x]@b) (splits xs)

  
(* BONUS *)

let permutations (xs:'a list):'a list list = 
  failwith "Not implemented"



(********************* QUESTION 3 *********************)


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
 * Sample Finite Automata
 *
 * The first accepts the language of all strings over {x,y,z} 
 * starting and ending with an x.
 *
 * The second accepts the language of all strings over {x,y,z}
 * ending with three y's.
 *
 *)

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
         
let faLastThreeY = {
    states = [0; 1; 2; 3];
    alphabet = ['x'; 'y'; 'z'];
    delta = [ (0,'x',0);
	      (0,'y',0);
	      (0,'z',0);
	      (0,'y',1);
	      (1,'y',2);
	      (2,'y',3); ];
    start = 0;
    final = [3]
  }

                 
let has_final (m:fa) (qs:int list):bool =
  List.for_all (fun x -> List.mem x qs) m.final

let follow_symbol (m:fa) (q:int) (a:char):int list =
  List.map (fun (x, y, z) -> z) (List.filter (fun (x, y, z) -> x = q && y = a) m.delta)
  
let follow_symbol_from_states (m:fa) (qs:int list) (a:char):int list =
  List.fold_right (fun x r -> (follow_symbol m x a) @ r) qs []

let rec follow_sequence_from_states (m:fa) (qs:int list) (syms:char list):int list =
  match syms with
  | [] -> qs
  | x::xs' -> follow_sequence_from_states m (follow_symbol_from_states m qs x) xs'

let accept (m:fa) (input:string):bool = 
  let symbols = explode input in
  has_final m (follow_sequence_from_states m [m.start] symbols)



  
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
