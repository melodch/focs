(* 

HOMEWORK 5

Due: Wed Mar 23, 2022 (23h59)

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




(* QUESTION 1 *)


(* Type for 2-tapes deterministic Turing machines with integer states *)

type tm2 = {
    states : int list;
    input_alphabet : char list;
    tape_alphabet : char list;
    delta : (int * char * char) -> (int * char * int * char * int);   (* -1 = Left, 0 = Stay, 1 = Right *)
    start : int;
    accept : int;
    reject : int
  }

type config2 = {
    state : int;
    tape1: char list;
    tape2: char list;
    position1: int;
    position2: int
  }


(* A sample 2-tapes TM that accepts {a^nb^n | n >= 0} *)
             
let anbn =
  let d inp = (match inp with
               | (1, '_', '_') -> (777, '_', 0, '_', 0)
               | (1, 'a', '_') -> (2, 'a', 0, 'X', 1)
               | (2, 'a', '_') -> (2, 'a', 1, 'a', 1)
               | (2, 'b', '_') -> (3, 'b', 0, '_', -1)
               | (3, 'b', 'a') -> (3, 'b', 1, 'a', -1)
               | (3, '_', 'X') -> (777, '_', 0, 'X', 0)
               | (_, c1, c2) -> (666, c1, 0, c2, 0))
  in {
      states = [1; 2; 3; 3; 777; 666];
      input_alphabet = ['a';'b'];
      tape_alphabet = ['a';'b';'X';'_'];
      start = 1;
      accept = 777;
      reject = 666;
      delta = d
    }
   

(* A dummy TM used as a placeholder *)
   
let dummy_tm2 = {
    states = [0; 1];
    input_alphabet = ['x'];
    tape_alphabet = ['x'; '_'];
    start = 0;
    accept = 0;
    reject = 1;
    delta = (fun _ -> (1, '_', 0, '_', 0))
  }
             
             
(* Helper functions
 *
 *   explode : string -> char list
 *      return the list of characters making up a string
 *
 *   print_config: tm2 -> config2 -> 'a -> 'a
 *      print a configuration (including newline) to standard output
 *      and return a value
 * 
 *   validate_states : tm2 -> bool
 *      check that all the states reachable from the start state
 *      appear in the set of states of the Turing machine
 *
 *   replace_nth : 'a list -> int -> 'a -> 'a list
 *      replace the nth element of a list with a given value,
*       returning the new list
 *)

let explode (str: string): char list = 
  let rec acc (index, result) = 
    if (index < 0) then
      result
    else
      acc (index - 1, (String.get str index) :: result)
  in
  acc (String.length (str) - 1, [])

let print_config (m: tm2) (c: config2): unit =
  let max_state_length a r = max (String.length (string_of_int a)) r in
  let mw = List.fold_right max_state_length m.states 0 in
  let padding1 = max 0 (c.position1 + 1 - List.length c.tape1) in
  let padding2 = max 0 (c.position2 + 1 - List.length c.tape2) in
  let rec mk_blank k =
    match k with
    | 0 -> []
    | _ -> '_' :: (mk_blank(k - 1)) in
  let tape1' : char list = c.tape1 @ (mk_blank padding1) in
  let tape2' : char list = c.tape2 @ (mk_blank padding2) in
  let str_state = string_of_int c.state in
  let _ = print_string (String.sub (str_state ^ (String.make mw ' ')) 0 mw) in
  let _ = print_string "  "  in
  let _ = List.iteri (fun i sym -> 
              if (i = c.position1) then
                Printf.printf "[%c]" sym
	      else
                Printf.printf " %c " sym) tape1'  in
  let _ = print_newline () in
  let _ = print_string (String.make mw ' ') in
  let _ = print_string "  "  in
  let _ = List.iteri (fun i sym -> 
              if (i = c.position2) then
                Printf.printf "[%c]" sym
	      else
                Printf.printf " %c " sym) tape2'  in
  let _ = print_newline ()  in
  print_newline ()


let validate_states (m: tm2):bool =
  let add_transitions q syms1 syms2 queue =
    let loop queue sym1 =
      let add_to_queue queue sym2 = let (p, _, _, _, _) = m.delta (q, sym1, sym2) in p :: queue in
      List.fold_left add_to_queue queue syms2 in
    List.fold_left (fun queue sym1 -> loop queue sym1) queue syms1  in
  let rec loop seen queue =
    match queue with
    | [] -> true
    | q::qs -> if not (List.mem q m.states) then
                 let str_q = string_of_int q in
                 failwith ("Reachable state " ^ str_q ^ " not defined in states list")
               else if not (List.mem q seen) then
                 let str_q = string_of_int q in
                 let _ = print_string ("Following state " ^ str_q ^ "\n") in
                 loop (q :: seen) (add_transitions q m.tape_alphabet m.tape_alphabet qs)
               else
                 loop seen qs  in
  loop [] [m.start]
  

let rec replace_nth (lst: 'a list) (n: int) (s: 'a): 'a list = 
  match lst with
  | [] -> []
  | x::xs when n = 0 -> s::xs
  | x::xs -> x::(replace_nth xs (n - 1) s)


           

let start_config (m:tm2) (w:string): config2 =
  failwith "not implemented"

  
let accept_config (m: tm2) (c: config2): bool = 
  failwith "not implemented"

  
let reject_config (m: tm2) (c: config2): bool = 
  failwith "not implemented"


let step (m: tm2) (c: config2): config2 = 
  failwith "not implemented"

  
let run (m: tm2) (w: string): bool = 
  failwith "not implemented"

  
let tm_anbmcndm : tm2 =
  (* replace by your actual implementation of the Turing machine *)
  dummy_tm2


  
(* QUESTION 2 *)

(* A type for partial functions that return a value for some inputs and None for others *)
  
type ('a, 'b) pfn = 'a -> 'b option

                  
let null_pfn (): ('a, 'b) pfn = 
  failwith "not implemented"

  
let extend_pfn (arg: 'a) (value: 'b) (pfn: ('a, 'b) pfn): ('a, 'b) pfn = 
  failwith "not implemented"


let join_pfn (pfn1: ('a, 'b) pfn) (pfn2: ('a, 'b) pfn): ('a, 'b) pfn  =
  failwith "not implemented"

                                                                                 
let default_pfn (b: 'b) (pfn: ('a, 'b) pfn): ('a -> 'b) =
  failwith "not implemented"


let fail_pfn (pfn: ('a, 'b) pfn): ('a -> 'b) =
  failwith "not implemented"

                                                                        
let mk_dict (pairs: ('a * 'b) list): ('a -> 'b) =
  failwith "not implemented"
    

  
(* QUESTIONS 3-4 *)

(*
 *  The type for instructions - each row is a constructor representing an instruction
 *
 *) 

type instruction =
  | INC of string
  | DEC of string * string
  | JUMP of string
  | TRUE
  | FALSE
  | PRINT of string
  | LABEL of string
  | REGISTER of string * int
  | EQUAL of string * string

let pp instructions =
  let instruction_to_string i instr =
    match instr with
    | INC n -> Printf.sprintf "%04d INC %s\n" i n
    | DEC (n, addr) -> Printf.sprintf "%04d DEC (%s, %s)\n" i n addr
    | JUMP addr -> Printf.sprintf "%04d JUMP %s\n" i addr
    | TRUE -> Printf.sprintf "%04d TRUE\n" i
    | FALSE -> Printf.sprintf "%04d FALSE\n" i
    | PRINT n -> Printf.sprintf "%04d PRINT %s\n" i n
    | LABEL addr -> Printf.sprintf "%04d LABEL %s\n" i addr
    | REGISTER (r, n) -> Printf.sprintf "%04d REGISTER (%s, %d)\n" i r n
    | EQUAL (r1, r2) -> Printf.sprintf "%04d EQUAL (%s, %s)\n" i r1 r2  in
  let program_to_string instructions =
    String.concat "" (List.mapi instruction_to_string instructions)  in
  print_string (program_to_string instructions)


(* 
 * Helper functions
 *
 * Create a map from registers name to positions
 * Create a map from label names to instruction indices
 * Lookup a name in a map
 * Get the largest register used in a program
 *
 *)
  
module SMap = Map.Make(String)
    
let register_map (instrs: instruction list): int SMap.t =
  let rec loop instrs map =
    match instrs with
    | [] -> map
    | (REGISTER (name, r))::instrs' -> loop instrs' (SMap.add name r map)
    | _::instrs' -> loop instrs' map  in
  loop instrs SMap.empty
    
let label_map (instrs: instruction list): int SMap.t = 
  let rec loop instrs addr map =
    match instrs with
    | [] -> map
    | (LABEL n)::instrs' -> loop instrs' (addr + 1) (SMap.add n addr map)
    | _::instrs' -> loop instrs' (addr + 1) map  in
  loop instrs 0 SMap.empty

let lookup sym map =
  try SMap.find sym map with
  | Not_found -> let _ = Printf.printf "Cannot find symbol %s\n" sym in
                 raise Not_found

let max_register (p : instruction list): int =
  let rec loop instrs curr_max =
    match instrs with
    | [] -> curr_max
    | (REGISTER (_, cntr))::instrs' -> loop instrs' (max curr_max cntr)
    | _::instrs' -> loop instrs' curr_max in
  loop p 0

  
(*
 * Run a CPU program directly
 *
 * Inputs are: program and list of initial values for the first few registers
 *
 *)
  
let run_cpu (p: instruction list) (nums: int list): bool =
  let _ = print_string "----------------------------------------------------------------------\n" in
  let _ = pp p in
  let _ = print_string "----------------------------------------------------------------------\n" in
  let rmap = register_map p in
  let lmap = label_map p in
  let p_array = Array.of_list p in
  let registers =  Array.make (max_register p + 1) 0 in
  let _ = List.iteri (fun i v -> Array.set registers i v) nums in
  let rec loop addr =
    let regs = String.concat " " (List.map string_of_int (Array.to_list registers)) in
    let _ = Printf.printf "%04d: %s\n" addr regs in
    match (Array.get p_array addr) with
    | INC (r) -> let r = lookup r rmap in
                 let _ = Array.set registers r ((Array.get registers r) + 1) in
                 loop (addr + 1) 
    | DEC (r, addr') -> let r = lookup r rmap in
                        let addr' = lookup addr' lmap in
                        let v = Array.get registers r in
                        if (v > 0) then
                          let _ = Array.set registers r (v - 1) in
                          loop (addr + 1)
                        else
                          loop addr'
    | JUMP addr' -> let addr' = lookup addr' lmap in
                    loop addr'
    | TRUE -> true
    | FALSE -> false
    | PRINT r -> let r' = lookup r rmap in
                 let _ = Printf.printf "      Register %s = %d\n" r (Array.get registers r') in
                 loop (addr + 1)
    | EQUAL (r1, r2) -> let r1 = lookup r1 rmap in
                        let r2 = lookup r2 rmap in
                        Array.get registers r1 = Array.get registers r2
    | _ -> loop (addr + 1)  in
  loop 0


let p_reset: instruction list = [
    REGISTER ("X", 0);
    LABEL "loop";
    DEC ("X", "done");
    JUMP "loop";
    LABEL "done";
    TRUE;
  ]

let p_transfer: instruction list = [
    REGISTER ("X", 0);
    REGISTER ("Y", 1);
    LABEL "loop";
    DEC ("X", "done");
    INC "Y";
    JUMP "loop";
    LABEL "done";
    TRUE;
  ]

let p_succ: instruction list = [
    REGISTER ("X", 0);
    REGISTER ("Z", 1);
    INC "X";
    EQUAL ("X", "Z");
  ]



             
let p_copy: instruction list = [
    (* Replace by your own program *)
    TRUE;
  ]

                             
let p_swap: instruction list = [
    (* Replace by your own program *)
    TRUE;
  ]    

                             
let p_plus: instruction list = [
    (* Replace by your own program *)
    TRUE;
  ]        

                             
let p_sub: instruction list = [
    (* Replace by your own program *)
    TRUE;
  ]

                            
let p_max: instruction list = [
    (* Replace by your own program *)
    TRUE;
  ]
            
           
let p_diff: instruction list = [
    (* Replace by your own program *)
    TRUE;
  ]
               

               
  
let p_times: instruction list = [
    (* Replace by your own program *)
    TRUE;
  ]

                              
let p_square: instruction list = [
    (* Replace by your own program *)
    TRUE;
  ]


let p_square_x_2: instruction list = [
    (* Replace by your own program *)
    TRUE;
  ]
            
