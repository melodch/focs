(* 

HOMEWORK 1

Due: Wed Feb 9, 2022 (23h59)

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



(* Question 1 *)

let linear (a:float) (b:float) (x:float) (y:float) (c:float):float =
  if (c < a || c > b) then
    failwith "value out of range"
  else
    (c -. a) *. (y -. x) /. (b -. a) +. x
;;


let roots (a:float) (b:float) (c:float):(float * float) =
  if b*.b -. 4.*.a*.c < 0. || a = 0.
  then failwith "No real root"
  else ((~-.b +. sqrt(b*.b -. 4.*.a*.c)) /. (2.*.a),(~-.b -. sqrt(b*.b -. 4.*.a*.c)) /. (2.*.a))
;;


let rec selfConcat (s:string) (n:int):string =
  if n = 0 then "" else s^(selfConcat s (n-1))
;;


let rec collatz (a:int):int list=
  if a = 1 then [a]
  else if a mod 2 = 0 then a::collatz (a/2)
  else a::collatz (3*a+1)
;;


let rec range (i:int) (j:int):int list =
  if i = j then []
  else i::range (i+1) j
;;

  
(* Question 2 *)

  
let rec cubes (xs:int list):int list =
  match xs with
  | [] -> []
  | x::xs' -> (x*x*x)::cubes xs'
;;

  
let rec appendString (x:string) (ys:string list):string list = 
  match ys with
  | [] -> []
  | y::ys' -> (y^x)::appendString x ys'
;;


let rec doubleUp (xs:int list):int list =
  match xs with
  | [] -> []
  | x::xs' -> x::(x*2)::doubleUp xs'
;;

  
let rec nonNegative (xs:int list):int list =
  match xs with
  | [] -> []
  | x::xs' -> if x < 0 then nonNegative xs'
              else x::nonNegative xs'
;;

  
let rec classify (xs:int list):int list * int list =
  match xs with
  | [] -> ([],[])
  | x::xs' -> if x < 0 then (match classify xs' with (i,j) -> i), x::(match classify xs' with (i,j) -> j)
              else x::(match classify xs' with (i,j) -> i), (match classify xs' with (i,j) -> j)
;;

  
(* QUESTION 3 *)

  
let rec scale (a:int) (v:int list):int list =
  match v with
  | [] -> []
  | x::xs' -> (a*x)::scale a xs'
;;


let rec add (v:int list) (w:int list):int list =
  match (v,w) with
  | ([],[]) -> []
  | (x::xs',[]) -> x::xs'
  | ([],y::ys') -> y::ys'
  | (x::xs',y::ys') -> (x+y)::(add xs' ys')
;;


let rec length (v:int list): float =
  let rec sum_squares (v:int list): int =
    match v with
    | [] -> 0
    | x::xs' -> x*x + sum_squares (xs') in
  sqrt(float(sum_squares v))
;;


let rec inner (v:int list) (w:int list):int =
  match (v,w) with
  | ([],[]) -> 0
  | (x::xs',[]) -> failwith "invalid input"
  | ([],y::ys') -> failwith "invalid input"
  | (x::xs',y::ys') -> if xs' = [] then x * y
                       else x * y + inner xs' ys'
;;
        

(* QUESTION 4 *)


let rec set (xs: 'a list): 'a list =
  match xs with
  | [] -> []
  | h::t -> if List.mem h t then set t
            else h::set t
;;

            
let rec set_sub (xs:'a list) (ys:'a list): bool = 
  match (xs,ys) with
  | ([],[]) -> true
  | (x::xs',[]) -> false
  | ([],y::ys') -> true
  | (x::xs',y::ys') -> if List.mem x ys' || x = y then set_sub xs' (y::ys')
                       else false
;;

            
let rec set_union (xs: 'a list) (ys: 'a list): 'a list = 
  match (xs,ys) with
  | ([],[]) -> []
  | (x::xs',[]) -> x::xs'
  | ([],y::ys') -> y::ys'
  | (x::xs',y::ys') -> if List.mem x ys' || x = y then set_union xs' (y::ys')
                       else x::set_union xs' (y::ys')
;;


let rec set_inter (xs:'a list) (ys:'a list): 'a list = 
  match (xs,ys) with
  | ([],[]) -> []
  | (x::xs',[]) -> []
  | ([],y::ys') -> []
  | (x::xs',y::ys') -> if List.mem x ys' || x = y then x::(set_inter xs' (y::ys'))
                       else set_inter xs' (y::ys')
;;
