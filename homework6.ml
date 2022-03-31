(* 

HOMEWORK 6

Due: Thu Mar 31, 2022 (23h59)

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
 * Type for grammars
 *
 *)

type symbol = string

type grammar = {
  nonterms: symbol list;
  terms: symbol list;
  rules: (string * string) list;
  start : symbol
}


(* 
 * Some sample context-free grammars 
 *
 *)

let anbn = {
  nonterms = ["S"];
  terms = ["a"; "b"];
  rules = [("S", "");
           ("S", "aSb")];
  start = "S"
}

let anbm = {
  nonterms = ["S"; "T"; "U"];
  terms = ["a"; "b"];
  rules = [ ("S", "TU");
            ("T", "");
            ("T", "aTb");
            ("U", "");
            ("U", "Ub")];
  start = "S"
}


(*
 * Here's a grammar that is _not_ context-free
 *
 * It's also harder to generate its strings
 *
 *)

let anbncn = {
  nonterms = ["S"; "A"; "B"; "C"; "X"];
  terms = ["a"; "b"; "c"];
  rules = [ ("S", "");
            ("S", "AB");
            ("B", "XbBc");
            ("B", "");
            ("A", "AA");
            ("AX", "a");
            ("aX", "Xa");
            ("bX", "Xb") ];
  start = "S"
}

(*
Sample derivation: 
S
AB
AAB
AAAB
AAAXbBc
AAAXbXbBcc
AAAXbXbXbBccc
AAAXbXbXbccc
AAAXbXXbbccc
AAAXXbXbbccc
AAAXXXbbbccc
AAaXXbbbccc
AAXaXbbbccc
AaaXbbbccc
AaXabbbccc
AXaabbbccc
aaabbbccc
*)



(* abbreviations *)

let map = List.map
let len = String.length
let sub = String.sub


(*
 * Utility functions 
 * 
 *)


(* check if lhs is a prefix of str *)

let prefix lhs str =
  lhs = (sub str 0 (len lhs))


(* replace prefix lhs of str with rhs *)

let replace lhs str rhs =
  let l = len lhs in
  rhs ^ (sub str l (len str - l))


(* try to apply rule (lhs,rhs) to str (assuming prefix prf) *)

let apply_rule prf (lhs,rhs) str =
  if len str < len lhs 
    then []
  else if prefix lhs str
    then [prf^(replace lhs str rhs)]
  else []


(* try to apply every rule in rs to str *)

let rec apply_rules rs str =
  let rec loop prefix str = 
    if str = "" then []
    else let rest = loop (prefix^(sub str 0 1)) (sub str 1 (len str -1))  in
       (List.fold_left (fun res r -> res@(apply_rule prefix r str)) [] rs)@rest  in
  loop "" str


(* check if a string can be obtained from another string by applying a rule in rs *)

let obtainable rs start str1 str2 =
  if str1 = ""
    then str2 = start
  else
    let strs = apply_rules rs str1 in
    List.mem str2 strs

(*
 * Perform an iteratively deepening depth-first search of the rewrite 
 * tree
 *
 *)

module StringSet = Set.Make(String)

let dfs_path init maxdepth maxwidth grammar target =
  let lt = len target  in
  let rec initPart init previous =
    match init with
    | [] ->  previous
    | x::xs -> if obtainable grammar.rules grammar.start previous x
                 then initPart xs x
	       else failwith ("Unacceptable string '"^ x ^"' in init derivation")  in
  let actualStart = initPart (match init with [] -> [grammar.start] | _ -> init) "" in
  let widthAdjustment = len actualStart in 
  let rec loop q seen =
    match q with
    | [] -> []
    | ((path,d)::q) -> 
           (match path with
            | str::_ -> if len str > maxwidth + widthAdjustment
                          then loop q seen 
                        else if len str = lt && str = target
                          then path
                        else if StringSet.mem str seen
                          then loop q seen
                        else if d > maxdepth
                          then loop q (StringSet.add str seen)
                        else (* let _ = (print_string str; print_newline()) in *)
                          let new_strs = apply_rules grammar.rules str in
                          let new_strs_d = map (fun x -> (x::path,d+1)) new_strs in
                          let q = (new_strs_d)@q in
                          loop q (StringSet.add str seen)
            | _ -> failwith "problem - empty path in dfs_path") in
  loop [([actualStart],0)] StringSet.empty

let idfs_path init maxdepth grammar target =
  let rec loop n =
    let _ = Printf.printf "Searching (depth %02d, max width %02d)" n n in
    let _ = print_newline ()  in
    if n > maxdepth
      then []
    else match dfs_path init n n grammar target with
         | [] -> loop (n+1)
         | path -> path  in
  loop 1


(* 
 * Check if a grammar is well-formed 
 *
 *)

let checkGrammar grammar = 
  let _ = List.iter (fun x -> if String.length x != 1
                                then failwith ("symbol "^x^" not a single character")
                              else ()) grammar.nonterms  in
  let _ = List.iter (fun x -> if String.length x != 1
                                then failwith ("symbol "^x^" not a single character")
                              else ()) grammar.terms  in
  let _ = List.iter (fun (p,q) -> if String.length p < 1
                                    then failwith "rule with empty left-hand side"
                                  else ()) grammar.rules  in
  let _ = if List.mem grammar.start grammar.nonterms
            then ()
          else failwith "start symbol not a nonterminal"  in
  ()


(*
 * Try to generate a string for a given grammar 
 * 
 *)

let generate (grammar:grammar) (str:string) (md:int) (init:symbol list) =
  let _ = checkGrammar grammar in
  let print pre str = (print_string pre;
                       print_string str;
                       print_newline ())  in
  let rec rev_print path =
    match path with
    | [] -> ()
    | [s] -> print "   " s
    | s::ss -> (rev_print ss; print "-> " s)  in
  let path = idfs_path init md grammar str  in
  let _ = rev_print path  in
  path != []
  


(* 
 * QUESTION 1
 *
 *)


let dummy_grammar = {
  nonterms = [];
  terms = [];
  rules = [];
  start = ""
} 


let q1_amnbmcn = {
  nonterms = ["S"; "T"; "U"];
  terms = ["a"; "b"];
  rules = [ ("S", "TU");
            ("TU", "aTUc");
            ("TU", "aTUb");
            ("TU", "")];
  start = "S"
}

               
let q1_ambmncn = {
  nonterms = ["S"; "T"; "U"];
  terms = ["a"; "b"];
  rules = [ ("S", "TU");
            ("T", "aTb");
            ("T", "");
            ("U", "bUc");
            ("U", "")];
  start = "S"
}

               
let q1_a2mbncmn = {
  nonterms = ["S"; "T"; "U"];
  terms = ["a"; "b"];
  rules = [ ("S", "TU");
            ("TU", "aaTUc");
            ("TU", "bTUc");
            ("TU", "")];
  start = "S"
}

                
let q1_equal = {
  nonterms = ["S"; "T"; "U"];
  terms = ["a"; "b"];
  rules = [ ("S", "TU");
            ("TU", "aTUb");
            ("TU", "bTUa");
            ("TU", "");
            ("ab", "ba");
            ("ba", "ab")];
  start = "S"
}

             
let q1_addition = {
  nonterms = ["S"; "T"; "U"];
  terms = ["a"; "b"];
  rules = [ ("S", "+=");
            ("+=", "1+=1");
            ("=", "1=1")];
  start = "S"
}


(*
 * QUESTION 2
 *
 *)
  

let q2_anbncndn = {
  nonterms = ["S"; "A"; "B"; "C"; "X"];
  terms = ["a"; "b"; "c"; "d"];
  rules = [ ("S", "");
            ("S", "ABd");
            ("AB", "AABd");
            ("B", "XbBc");
            ("B", "");
            ("AX", "a");
            ("aX", "Xa");
            ("bX", "Xb") ];
  start = "S"
}

(* please put your sequence of rewrites for the string aabbccdd here *)

let rewrites_q2_anbncndn_2 = ["S"; "ABd"; "AABdd"; "AAXbBcdd"; "AAXbXbBccdd"; "AAXbXbccdd"]
   
(* please put your sequence of rewrites for the string aaabbbcccddd here *)

let rewrites_q2_anbncndn_3 = ["S"; "ABd"; "AABdd"; "AAABddd"; "AAAXbBcddd"; "AAAXbXbBccddd"; "AAAXbXbXbBcccddd"; "AAAXbXbXbcccddd"]

let rewrites_q2_anbncndn_4 = ["S"; "ABd"; "AABdd"; "AAABddd"; "AAAABdddd"; "AAAAXbBcdddd"; "AAAAXbXbBccdddd"; "AAAAXbXbXbBcccdddd"; "AAAAXbXbXbXbBccccdddd"; "AAAAXbXbXbXbccccdddd"]


let q2_straddle = {
  nonterms = ["S"; "A"; "B"; "X"; "Y"; "T"; "U"];
  terms = ["a"; "b"];
  rules = [ ("S", "TU");
            ("T", "aTA");
            ("T", "bTB");
            ("U", "XUa");
            ("U", "YUb");
            ("T", ">");
            ("U", "<");
            (">BY", "b>");
            (">AX", "a>");
            ("><", "");
            ("AY", "YA");
            ("BY", "YB");
            ("XY", "YX");
            ("BX", "XB");
            ("YX", "XY");
            ("AX", "XA");];
  start = "S"
}
                
  
(* please put your sequence of rewrites for the string abaabaaba here *)

let rewrites_q2_straddle_aba = ["S"; "TU"; "aTAU"; "abTBAU"; "abaTABAU"; "aba>ABAU"; "aba>ABAXUa"; "aba>ABAXYUba"; "aba>ABAXYXUaba"; "aba>ABAXYX<aba"; "aba>ABXAYX<aba"; "aba>AXBAYX<aba"; "abaa>BAYX<aba"; "abaa>BYAX<aba"; "abaab>AX<aba"]
   
(* please put your sequence of rewrites for the string abbbbbbaabbb here *)

let rewrites_q2_straddle_abbb = ["S"; "TU"; "aTAU"; "abTBAU"; "abbTBBAU"; "abbbTBBBAU"; "abbb>BBBAU"; "abbb>BBBAYUb"; "abbb>BBBAYYUbb"; "abbb>BBBAYYYUbbb"; "abbb>BBBAYYYXUabbb"; "abbb>BBBAYYYX<abbb"; "abbb>BBBYAYYX<abbb"; "abbb>BBYBAYYX<abbb"; "abbb>BYBBAYYX<abbb"; "abbbb>BBAYYX<abbb"; "abbbb>BBYAYX<abbb"; "abbbb>BYBAYX<abbb"; "abbbbb>BAYX<abbb"; "abbbbb>BYAX<abbb"; "abbbbbb>AX<abbb"]
