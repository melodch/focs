(* 

HOMEWORK 7

Due: Thu Apr 7, 2022 (23h59)

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
 * The implementation of parsing and simplication of lambda terms
 *
 * I've isolated it away into a module that you don't need to peer into
 *
 *)

module Lambda = struct

  type lterm = 
      LIdent of string
    | LLam of string * lterm
    | LApp of lterm * lterm

  let fresh =
    let tag = ref 0 in
    fun id ->
      let new_id = id^"_"^(string_of_int (!tag)) in
      ( tag := (!tag) + 1; new_id)

  let lexer = Genlex.make_lexer ["(";")";"<";"->";">"]

  let explode str = 
    let rec acc index result = 
      if (index<0) then result
      else acc (index-1) ((String.sub str index 1)::result) in
    acc (String.length(str)-1) []

  let canonicalize s = 
    (* urgh! dirty hack, but the aim is to produce something quickly *)
    (* replace every ">" and "<" by "> " and "> " so that the lexer  *)
    (* actually works since it doesn't want to split ">>" *)
   let sl = explode s  in
   let sl' = List.map (fun s -> match s with "<" -> "< " | ">" -> "> " | _ -> s) sl in
   List.fold_left (fun acc s -> acc^s) "" sl'

  let lex s = 
    let str = lexer (Stream.of_string s)  in
    let rec loop () = 
      match (Stream.peek str) with
      | None -> []
      | Some _ -> let elt = Stream.next str in elt::(loop())  in
    loop ()

  let expect elt cs = 
    match cs with
    | f::cs when f = elt -> Some cs
    | _ -> None

  let expect_ident cs = 
    match cs with
    | (Genlex.Ident id)::cs -> Some (id,cs)
    | _ -> None

  let rec makeApps terms = 
    match terms with
    | [] -> failwith "Parsing error! [makeApps with empty list]"
    | [t] -> t
    | t1::t2::ts -> makeApps (LApp(t1,t2)::ts)

  let rec parse_terms cs = 
    match parse_term cs with
    | Some (t,cs) -> 
	(match parse_terms cs with
	| Some (ts,cs) -> Some (t::ts,cs)
        | None -> Some ([t],cs))
    | None -> None

  and parse_term cs = 
    match parse_ident cs with
    | Some x -> Some x
    | None -> 
	(match parse_lambda cs with
	|	Some x -> Some x
	|	None ->
	    (match parse_group cs with
	    | Some x -> Some x
	    | None -> None))

  and parse_ident cs =
    match expect_ident cs with
    | None -> None
    | Some (id,cs) -> Some (LIdent id,cs)

  and parse_lambda cs = 
    match expect (Genlex.Kwd "<") cs with
    | None -> None
    | Some cs -> 
	(match expect_ident cs with
	|	None -> None
	|	Some (id,cs) -> 
	    (match expect (Genlex.Kwd "->") cs with
	    | None -> None
	    | Some cs -> 
		(match parse_terms cs with
		|	None -> None
		|	Some (terms,cs) ->
		    (match expect (Genlex.Kwd ">") cs with
		    | None -> None
		    | Some cs -> Some (LLam (id,makeApps(terms)),cs)))))

  and parse_group cs =
    match expect (Genlex.Kwd "(") cs with
    | None -> None
    | Some cs ->
	(match parse_terms cs with
	|	None -> None
	|	Some (terms,cs) ->
	    (match expect (Genlex.Kwd ")") cs with
	    | None -> None
	    | Some cs -> Some (makeApps(terms),cs)))


  let parse str = 
    match parse_terms (lex (canonicalize str)) with
    | Some (terms,[]) -> makeApps(terms)
    | _ -> failwith ("Cannot parse "^str)

  let rec pp term = 
    match term with
    | LIdent x -> x
    | LLam (x,t) -> "<"^x^" -> "^(pp t)^">"
    | LApp (t1,t2) -> 
       let t2' = (match t2 with
       | LApp _ -> "("^(pp t2)^")"
       | _ -> (pp t2))  in
       (pp t1)^" "^t2'

  let rec rename term old nw = 
    match term with
    | LIdent x when x = old -> LIdent nw
    | LIdent x -> LIdent x
    | LLam (x,t) when x = old  -> LLam (x,t)
    | LLam (x,t) -> LLam (x, rename t old nw)
    | LApp (t1,t2) -> LApp (rename t1 old nw,
			    rename t2 old nw)

  let rec fv m = 
    match m with
    | LIdent x -> [x]
    | LLam (x,t) -> List.filter (fun y -> x <> y) (fv t)
    | LApp (t1,t2) -> (fv t1) @ (fv t2)

  let rec substitute m s n = 
    match m with
    | LIdent x when x = s -> n
    | LIdent x -> LIdent x
    | LLam (x,t) when x = s -> LLam (x,t)
    | LLam (x,t) when List.mem x (fv n) -> 
	let x_ = fresh x in
	substitute (LLam (x_,rename t x x_)) s n
    | LLam (x,t) -> LLam (x,substitute t s n)
    | LApp (t1,t2) -> LApp (substitute t1 s n,
			    substitute t2 s n)

  let rec reduce term = 
    match term with
    | LIdent s -> None
    | LLam (s,term) -> 
	(match reduce term with
	|	None -> None
	|	Some t -> Some (LLam(s,t)))
    | LApp (LLam (s,term1),term2) -> 
	Some (substitute term1 s term2)
    | LApp (term1,term2) -> 
	(match reduce term1 with
	|	None -> (match reduce term2 with
      	  | None -> None
	  | Some t2 -> Some (LApp (term1,t2)))
	|	Some t1 -> Some (LApp (t1,term2)))

  let expand_all defs = 
    let expand defs t = List.fold_left (fun r (n,d) -> substitute r n d) t defs in
    let rec loop done_defs defs = 
      match defs with
      | [] -> done_defs
      | (name,df)::dfs -> loop ((name,expand done_defs df)::done_defs) dfs  in
    loop [] defs

  let threshold = 5000

  let simplify' print_term defs term = 
    let term = parse term  in
    let expand defs t = List.fold_left (fun r (n,d) -> substitute r n d) t defs in
    let defs = expand_all (List.map (fun (n,d) -> (n,parse d)) defs) in
    let term = expand defs term  in
    let rec loop n term =  
      let _ = print_term (" => "^(pp term)) in
      if n > threshold
      then failwith ("failed to find normal form after "^(string_of_int threshold)^" simplifications")
      else 
	match (reduce term) with
	| None -> pp term
	|	Some term -> loop (n+1) term  in
    match reduce term with
    | None -> let _ = print_endline "Term already in normal form" in (pp term)
    | Some next -> let _ = print_term ("   "^(pp term))  in loop 0 next

end




(* 
 * The default definitions from class
 *
 * PLEASE DO NOT MODIFY THIS LIST
 * When I test your code, I will use my own, and
 * I will not see your modifications
 *
 *)

let default_defs = [ 

   ("true", "<x -> <y -> x>>");
   ("false", "<x -> <y -> y>>");
   ("if", "<c -> <x -> <y -> c x y>>>");
   
   ("_0","<f -> <x -> x>>");
   ("_1","<f -> <x -> f x>>");
   ("_2","<f -> <x -> f (f x)>>");
   ("_3","<f -> <x -> f (f (f x))>>");
   ("_4","<f -> <x -> f (f (f (f x)))>>");
   ("_5","<f -> <x -> f (f (f (f (f x))))>>");
   ("succ","<n -> <f -> <x -> (n f) (f x)>>>");
   ("plus","<m -> <n -> <f -> <x -> (m f) (n f x)>>>>");
   ("times","<m -> <n -> <f -> <x -> m (n f) x>>>>");
   ("iszero","<n -> n <x -> false> true>");
   ("pred","<n -> <f -> <x -> n <g -> <h -> h (g f)>> <u -> x> <u -> u>>>>");

   ("pair","<x -> <y -> <s -> s x y>>>");
   ("first", "<p -> p <x -> <y -> x>>>");
   ("second", "<p -> p <x -> <y -> y>>>");
  ]



(*
 * Simplification of lambda terms
 *
 * One version that simply returns the result
 * One version that prints all intermediate terms
 *
 *)

let simplify_verbose defs term = Lambda.simplify' print_endline (default_defs@defs) term

let simplify defs term = Lambda.simplify' (fun t -> ()) (default_defs@defs) term


(*
 * By default, all of these are implemented as an identifier "not_implemented"
 * 
 * Just replace those "not_implemented" with your own definition
 * 
 * You can add in whatever helper functions your solutions need
 *
 * Remember that if definition X uses definition Y, then Y must appear before
 * X in the list
 *
 *)


let q12_defs = [
    

    (*************************************************************
     * Question 1
     *
     *************************************************************)

    ("and", "not_implemented");

    ("or", "not_implemented");

    ("not", "not_implemented");
    
    ("minus", "not_implemented");
    
    ("ge", "not_implemented");

    ("gt", "not_implemented");
    
    ("max", "not_implemented");

    ("min", "not_implemented");


    (*************************************************************
     * Question 2
     *
     *************************************************************)
    
    ("int", "not_implemented");
    
    ("neg_int", "not_implemented");

    ("abs", "not_implemented");
    
    ("plus_int", "not_implemented");

    ("times_int", "not_implemented");

  ]    
