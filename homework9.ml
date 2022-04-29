(* 

HOMEWORK 9

Due: Thu Apr 28, 2022 (23h59)

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
 * The implementation of dataflow networks.
 *
 * I've isolated it away into a module that you don't need to peer into.
 *
 * Please do not modify it -- I will be using my own copy, and your code
 * might break if you change it
 *
 *)

module S : sig

  type 'a network
  type 'a syntax
  type 'a raw_stream
  type 'a stream

  val cst : 'a -> 'a syntax
  val fby : string -> string -> 'a syntax
  val map : ('a -> 'a) -> string -> 'a syntax
  val map2 : ('a -> 'a -> 'a) -> string -> string -> 'a syntax
  val drop : string -> 'a syntax
  val filter : ('a -> 'a -> bool) -> string -> string -> 'a syntax
  val component : 'a network -> string list -> 'a syntax
  val rcomponent : string list -> 'a syntax

  val network : string list -> string -> (string * 'a syntax) list -> 'a network

  val run : 'a network -> 'a stream list -> 'a stream
  val prefix : int -> 'a stream -> 'a list
  val nth : int -> 'a stream -> 'a
            
end = struct

  type 'a syntax = Cst of 'a
                 | Fby of string * string
                 | Map of ('a -> 'a) * string
                 | Map2 of ('a -> 'a -> 'a) * string * string
                 | Drop of string
                 | Filter of ('a -> 'a -> bool) * string * string
                 | Component of 'a network * string list
                 | RecComponent of string list

  and 'a primitive = P_Cst of 'a
                   | P_Fby of 'a comp * 'a comp
                   | P_Map of ('a -> 'a) * 'a comp
                   | P_Map2 of ('a -> 'a -> 'a) * 'a comp * 'a comp
                   | P_Drop of 'a comp
                   | P_Filter of ('a -> 'a -> bool) * 'a comp * 'a comp
                   | P_Wire of string
                   | P_Component of 'a network * 'a comp list
                   | P_RecComponent of 'a network ref * 'a comp list

  and 'a comp = C of ('a raw_stream option ref * 'a primitive * 'a network ref)
              | NC of ('a primitive * 'a network ref)  (* non-cacheable *)

  and 'a network = Comp of ((string * 'a comp) list * string list * string)
  (* network is (pieces, inputs, output) *)

  and 'a raw_stream = Stream of ('a * 'a comp)

  type 'a stream = 'a network
  (* stream is a network with no inputs *)

  let hd (Stream (h,t)) = h
  let tl (Stream (h,t)) = t

  let lookup (Comp (comp,_,_)) n =
    let rec loop comp =
      match comp with
      | [] -> failwith ("cannot find "^n)
      | (n',s)::comp' when n=n' -> s
      | _::comp' -> loop comp'  in
    loop comp

  (* reach into a component and reset every environment to the provided one *)
  let rec smash prim renv =
    match prim with
    | P_Fby (s1,s2) -> P_Fby (smash_comp s1 renv, smash_comp s2 renv)
    | P_Map (f,s1) -> P_Map (f,smash_comp s1 renv)
    | P_Map2 (f,s1,s2) -> P_Map2(f,smash_comp s1 renv, smash_comp s2 renv)
    | P_Drop (s1) -> P_Drop (smash_comp s1 renv)
    | P_Filter (p,s1,s2) -> P_Filter (p,smash_comp s1 renv, smash_comp s2 renv)
    | x -> x

  and smash_comp = function (NC (c,_)) -> (fun renv -> NC (smash c renv,renv)) | (C (r,c,_)) -> (fun renv -> C (r,smash c renv,renv))

  let rec eval comp primitive =
    match primitive with

    | P_Wire n -> eval_comp (lookup comp n)
    | P_Cst k -> Stream(k, C (ref None, P_Cst k, ref comp))
    | P_Fby (a,b) -> let a' = eval_comp a in
                     Stream(hd(a'), b)
    | P_Map (f,a) -> let a' = eval_comp a in
                     Stream(f(hd(a')), C (ref None, P_Map (f,tl(a')),ref comp))
    | P_Map2 (f,a,b) -> let a' = eval_comp a in let b' = eval_comp b in
                                                Stream(f(hd(a')) (hd (b')), C (ref None, P_Map2 (f,tl(a'),tl(b')),ref comp))
    | P_Drop (a) -> let a' = eval_comp a in eval_comp (tl(a'))
    | P_Filter (p,a,b) -> let a' = eval_comp a in let b' = eval_comp b in
                                                  if p (hd a') (hd b') then Stream(hd b', C (ref None, P_Filter(p,tl(a'),tl(b')),ref comp))
                                                  else eval comp (P_Filter(p,tl(a'),tl(b')))
    | P_Component (comp',input_comps) -> let Comp(c,inps,out) = comp' in 
                                         let newW = List.map2 (fun x -> function (NC (y,_)) -> (x,C (ref None, y, ref comp)) | (C _) -> failwith "cannot be cached") inps input_comps in
                                         let me = ref (Comp ([],[],"")) in
                                         let newC = Comp(newW @ (List.map (fun (n,x) -> (n, smash_comp x me)) c), inps, out)  in
                                         let _ = (me := newC) in
                                         eval newC (P_Wire out)
    | P_RecComponent (rcomp',input_comps) -> let Comp(c,inps,out) = !rcomp' in 
                                             let newW = List.map2 (fun x -> function (NC (y,_)) -> (x,C (ref None, y, ref comp)) | (C _) -> failwith "cannot be cached") inps input_comps in
                                             let me = ref (Comp ([],[],"")) in
                                             let newC = Comp(newW @ (List.map (fun (n,x) -> (n, smash_comp x me)) c), inps, out)  in
                                             let _ = (me := newC) in
                                             eval newC (P_Wire out)

  and eval_comp comp = 
    match comp with
    | C (memoref, prim, comp) -> 
       (match !memoref with
        | Some v -> v
        | None -> let v = eval (!comp) prim  in
                  let _ = (memoref := Some v) in
                  v)
    | NC (prim, comp) -> eval (!comp) prim


  let prefix n comp =
    let Comp (_,_,output) = comp in
    let rec loop n c = 
      if n = 0 then []
      else 
        let str = eval_comp c in
        (* let _ = begin print_stream str ; print_newline () end in *)
        (hd str)::(loop (n-1) (tl str))  in
    loop n (lookup comp output)

  let nth n comp =
    let Comp (_,_,output) = comp in
    let rec loop n c = 
      let str = eval_comp c in
      if n = 0 then hd str
      else
        loop (n-1) (tl str) in
    loop n (lookup comp output)
    
  let run comp comps =
    let Comp (c,inps,out) = comp  in
    let _ = if List.length inps <> List.length comps then failwith "Wrong number of streams to pass to network" else () in
    let all = List.map2 (fun x y -> (x, NC (P_Component(y,[]),ref (Comp([],[],""))))) inps comps in
    let me = ref (Comp([],[],""))  in
    let result = Comp (all@( List.map (fun (n,x) -> (n,smash_comp x me)) c),[],out) in
    let _ = (me := result) in
    result

  let network inputs output lst =
    let me = ref (Comp ([],[],"")) in
    let rec convert s renv =
      match s with
      | Cst i -> NC (P_Cst (i),renv)
      | Fby (w1,w2) -> NC (P_Fby (NC (P_Wire w1, renv), NC (P_Wire w2, renv)),renv)
      | Map (f,w1) -> NC (P_Map (f,NC (P_Wire w1, renv)),renv)
      | Map2 (f,w1,w2) -> NC (P_Map2 (f,NC (P_Wire w1, renv), NC (P_Wire w2, renv)),renv)
      | Drop (w1) -> NC (P_Drop (NC (P_Wire w1, renv)), renv)
      | Filter (p,w1,w2) -> NC (P_Filter (p,NC (P_Wire w1, renv), NC (P_Wire w2, renv)),renv)
      | Component (c,cl) -> NC (P_Component(c,List.map (fun (w) -> NC(P_Wire w,renv)) cl),renv)
      | RecComponent (cl) -> NC (P_RecComponent(me,List.map (fun (w) -> NC(P_Wire w, renv)) cl),renv)  in
    let env = List.map (fun (n,p) -> (n,convert p me)) lst in
    let _ = (me := Comp (env,inputs,output))  in
    Comp (env,inputs,output)

  let cst (x:'a):'a syntax = Cst x
  let fby (s:string) (t:string):'a syntax = Fby (s,t)
  let map (f:'a -> 'a) (s:string):'a syntax = Map (f,s)
  let map2 (f:'a -> 'a -> 'a) (s:string) (t:string):'a syntax = Map2 (f,s,t)
  let drop (s:string):'a syntax = Drop (s)
  let filter (p:'a -> 'a -> bool) (s:string) (t:string) = Filter (p,s,t)
  let component (c:'a network) (args:string list):'a syntax = Component (c,args)
  let rcomponent (args:string list):'a syntax = RecComponent (args)

end


(*
 * 
 * THESE ARE THE FUNCTIONS YOU GET TO USE
 *
 *)

type 'a syntax = 'a S.syntax
type 'a network = 'a S.network
type 'a stream = 'a S.stream

let cst : 'a -> 'a syntax = S.cst
let fby : string -> string -> 'a syntax = S.fby
let map : ('a -> 'a) -> string -> 'a syntax = S.map
let map2 : ('a -> 'a -> 'a) -> string -> string -> 'a syntax = S.map2
let drop : string -> 'a syntax = S.drop
let filter : ('a -> 'a -> bool) -> string -> string -> 'a syntax = S.filter
let component : 'a network -> string list -> 'a syntax = S.component
let rcomponent : string list -> 'a syntax = S.rcomponent

let network : string list -> string -> (string * 'a syntax) list -> 'a network = S.network
let run : 'a network -> 'a stream list -> 'a stream = S.run
let prefix : int -> 'a stream -> 'a list = S.prefix
let nth : int -> 'a stream -> 'a = S.nth

(* 
 *
 * Some sample dataflow networks
 *
 * Mostly from class
 *
 *)

let from n = network [] "result" [
               ("input", cst n);
               ("result", fby "input" "add1");
               ("add1", map (fun x->x+1) "result")
             ]

let nats = network [] "result" [
               ("input", cst 0);
               ("result", fby "input" "add1");
               ("add1", map (fun x->x+1) "result")
             ]

let evens = network [] "result" [
                ("input", cst 0);
                ("result", fby "input" "add2");
                ("add2", map (fun x->x+2) "result")
              ]

let odds = network [] "result" [
               ("input", component evens []);
               ("result", map (fun x -> x+1) "input")
             ]

let add = network ["input1";"input2"] "result" [
              ("result", map2 (fun x y -> x+y) "input1" "input2")
            ]

let addf = network ["input1";"input2"] "result" [
              ("result", map2 (fun x y -> x+.y) "input1" "input2")
            ]

let squares = network [] "result" [
                  ("result", component add ["odds"; "fby"]);
                  ("odds", component odds []);
                  ("fby", fby "zero" "result");
                  ("zero", cst 0)
                ]

let fib = network [] "result" [
              ("result", fby "zero" "rest");
              ("zero", cst 0);
              ("rest", fby "one" "feedback");
              ("one", cst 1);
              ("feedback", component add ["result";"drop"]);
              ("drop", drop "result")
            ]

let psums = network ["input"] "result" [
                ("feedback", component add ["drop";"result"]);
                ("drop", drop "input");
                ("result", fby "input" "feedback")
              ]

let stairs n = network [] "result" [
                   ("input", cst 0);
                   ("result", fby "input" "add");
                   ("add", map (fun x -> if x  = n-1 then 0 else x+1) "result")
                 ]



let dummyIntNetwork : int network = network [] "result" []
let dummyFloatNetwork : float network = network [] "result" []




(* 
 * QUESTION 1 
 * 
 * As usual, replace the placeholder answers by your own implementation
 *
 *)

let mult : int network = network ["input1";"input2"] "result" [
              ("result", map2 (fun x y -> x*y) "input1" "input2")
            ]

let mult : float network = network ["input1";"input2"] "result" [
              ("result", map2 (fun x y -> x*.y) "input1" "input2")
            ]

let scale (n:int) : int network = network ["input"] "result" [
              ("result", map (fun x -> x*n) "input")
            ]

let scalef (n:float) : float network = network ["input"] "result" [
              ("result", map (fun x -> x*.n) "input")
            ]

let rep : int network = network ["input1"] "result" [
              ("result", fby "input1" "a");
              ("a", map (fun x -> x) "result")
            ]

let mult1 : int network = network ["input1";"input2"] "result" [
              ("result", map2 (fun x y -> x*y) "a" "input1");
              ("a", component rep ["input2"])
            ]

let mult2 : int network = network ["input1";"input2"] "result" [
              ("result", map2 (fun x y -> x*y) "input1" "rep2_stream");
              ("a", drop "input2");
              ("b", fby "input2" "r");
              ("r", fby "a" "b");
              ("rep2_stream", fby "input2" "r")
            ]

let maxcomp : int network = network ["input1"; "input2"] "result" [
              ("result", map2 (fun x y -> max x y) "input1" "input2")
            ]

let running_max : int network = network ["s"] "result" [
              ("result", fby "s" "a");
              ("b", drop "s");
              ("a", component maxcomp ["result"; "b"])
            ]

                              
(*
 * QUESTION 2
 *
 * As usual, replace the placeholder answers by your own implementation
 *
 *)

let zero1One1 : int network = network [] "result" [
              ("a", drop "input0");
              ("b", fby "input1" "result");
              ("result", fby "a" "b");
              ("input0", cst 0);
              ("input1", cst 1)
            ]

let zero2One1 : int network = network [] "result" [
              ("a", drop "input0");
              ("b", fby "input1" "result");
              ("c", fby "a" "b");
              ("result", fby "input0" "c");
              ("input0", cst 0);
              ("input1", cst 1)
            ]


let zeroNOne1 (n:int) : int network = network [] "result" [
              ("zeros", cst 0);
              ("r", map (fun x -> (x+1) mod (n+1)) "s");
              ("s", fby "zeros" "r");
              ("result", map (fun x -> if x < n then 0 else 1) "s");
            ]


let zeroNOneM (n:int) (m:int) : int network = network [] "result" [
              ("zeros", cst 0);
              ("r", map (fun x -> (x+1) mod (n+m)) "s");
              ("s", fby "zeros" "r");
              ("result", map (fun x -> if x < n then 0 else 1) "s");
            ]

  
(*
 * QUESTION 3
 * 
 * As usual, replace the placeholder answers by your own implementation
 *
 *)

let evensf = network [] "result" [
                ("input", cst 0.0);
                ("result", fby "input" "add2");
                ("add2", map (fun x->x+.2.0) "result")
              ]

let oddsf = network [] "result" [
               ("input", component evensf []);
               ("result", map (fun x -> x+.1.0) "input")
             ]

let pos1neg1f : float network = network [] "result" [
              ("a", drop "inputpos");
              ("b", fby "inputneg" "result");
              ("result", fby "a" "b");
              ("inputpos", cst 1.0);
              ("inputneg", cst (-1.0))
            ]

let arctan (z:float): float network = network [] "result" [
              ("feedback", map2 (fun x y -> x+.y) "drop" "result");
              ("drop", drop "stream");
              ("result", fby "stream" "feedback");
              ("stream", map2 (fun x y -> (y*.(Float.pow z x)/.x)) "odds_stream" "pos1neg1stream");
              ("odds_stream", component oddsf []);
              ("pos1neg1stream", component pos1neg1f [])
            ]

let pi : float network = network [] "result" [
              ("result", map2 (fun x y -> 16.0 *. x -. 4.0 *. y) "stream1" "stream2");
              ("stream1", component (arctan (1.0/.5.0)) []);
              ("stream2", component (arctan (1.0/.239.0)) [])
            ]


let newton (f : float -> float) (df : float -> float) (guess: float): float network = network [] "result" [
              ("feedback", map (fun x -> x-.((f x)/.(df x))) "result");
              ("result", fby "stream" "feedback");
              ("stream", cst guess)
            ]

let natsf = network [] "result" [
               ("input", cst 0.0);
               ("result", fby "input" "add1");
               ("add1", map (fun x->x+.1.0) "result")
             ]

let derivative (f:float -> float) (x:float): float network = network [] "result" [
              ("result", map (fun y -> (f (x+.(1.0/.y)) -. f x) /. (1.0/.y)) "nats_stream");
              ("stream", cst ((f (x+.1.0) -. f x) /. 1.0));
              ("nats_stream", component natsf [])
            ]

                                                           
let limit (epsilon:float): float network = network ["s"] "result" [
              ("drop", drop "s");
              ("result", filter (fun x y -> abs_float(x -. y) < epsilon) "drop" "s")
            ]
