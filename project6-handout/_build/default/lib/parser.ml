open AstTypes
open Utils
open TokenTypes

(* Parsing helpers (you do not need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is
   empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match
   the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)


(*
unambiguous grammar

E ---> or
or --> and || or  X
     | and
and -> eqa && and X
     | eqa
eqa -> rel (==|!=) eqa X
     | eqa
rel -> add (<|<=|>|>=) rel X
     | add
add -> mul + add X
     | mul
mul -> pow * mul X
     | pow
pow -> una ^ pow X 
     | una
una -> !una X
     | pri
pri -> Tok_Int | Tok_Bool | Tok_ID | (E) X
*)
let rec parse_expr toks : expr_result =
  let rec parse_Or ts = 
    let (t1, a1) = parse_And ts in
    match lookahead ts with
    | Tok_Or -> 
      let m1 = match_token t1 Tok_Or in
      let (t2, a2) = parse_Or m1 in (t2, Or(a1, a2))
    | _ -> (t1, a1)
  and parse_And ts =
    let (t1, a1) = parse_Equality ts in
    match lookahead ts with
    | Tok_And -> 
      let m1 = match_token t1 Tok_And in
      let (t2, a2) = parse_And m1 in (t2, And(a1,a2))
    | _ -> (t1, a1)
  and parse_Equality ts =
    let (t1, a1) = parse_Rel ts in
    match lookahead ts with
    | Tok_Equal -> 
      let m1 = match_token t1 Tok_Equal in
      let (t2, a2) = parse_Equality m1 in (t2, Equal(a1,a2))
    | Tok_NotEqual ->
      let m1 = match_token t1 Tok_NotEqual in
      let (t2, a2) = parse_Equality m1 in (t2, NotEqual(a1,a2))
    | _ -> (t1,a1)
  and parse_Rel ts = 
    let (t1, a1) = parse_Add ts in
    match lookahead ts with
    | Tok_Greater -> 
      let m1 = match_token t1 Tok_Greater in
      let (t2, a2) = parse_Rel m1 in (t2, Greater(a1,a2))
    | Tok_GreaterEqual -> 
      let m1 = match_token t1 Tok_GreaterEqual in
      let (t2, a2) = parse_Rel m1 in (t2, GreaterEqual(a1,a2))
    | Tok_Less -> 
      let m1 = match_token t1 Tok_Less in
      let (t2, a2) = parse_Rel m1 in (t2, Less(a1,a2))
    | Tok_LessEqual -> 
      let m1 = match_token t1 Tok_LessEqual in
      let (t2, a2) = parse_Rel m1 in (t2, LessEqual(a1,a2))
    | _ -> (t1,a1)
  and parse_Add ts = 
    let (t1, a1) = parse_Mult ts in
    match lookahead t1 with
    | Tok_Add -> 
      let m1 = match_token t1 Tok_Add in
      let (t2, a2) = parse_Add m1 in (t2, Add(a1, a2))
    | Tok_Sub -> 
      let m1 = match_token t1 Tok_Sub in
      let (t2, a2) = parse_Add m1 in (t2, Sub(a1, a2))
    | _ -> (t1, a1)   
  and parse_Mult ts = 
    let (t1, a1) = parse_Pow ts in
    match lookahead t1 with
    | Tok_Mult -> 
      let m1 = match_token t1 Tok_Mult in
      let (t2, a2) = parse_Mult m1 in (t2, Mult(a1,a2))
    | Tok_Div -> 
      let m1 = match_token t1 Tok_Div in
      let (t2, a2) = parse_Mult m1 in (t2, Div(a1,a2))
    | _ -> (t1, a1)
  and parse_Pow ts = 
    let (t1, a1) = parse_Una ts in
    match lookahead t1 with
    | Tok_Pow -> 
      let m1 = match_token t1 Tok_Pow in
      let (t2, a2) = parse_Pow m1 in (t2, Pow(a1, a2))
    | _ -> (t1, a1)
  and parse_Una ts = 
    match lookahead ts with
    | Tok_Not -> 
      let m1 = match_token ts Tok_Not in
      let (t1, a1) = parse_Una m1 in (t1, Not a1)
    | _ -> parse_Primary ts
  and parse_Primary ts =
    match lookahead ts with
    | Tok_ID id ->
      let m1 = match_token ts (Tok_ID id) in (m1, ID id)
    | Tok_Int i ->
      let m1 = match_token ts (Tok_Int i) in (m1, Int i)
    | Tok_Bool b ->
      let m1 = match_token ts (Tok_Bool b) in (m1, Bool b)
    | _ -> failwith "unrecognized token"
  in parse_Or toks
  
(*
where it says "Expr" that is the parse_Expr function

stmt -> opt
opt --> decl
      | assi
      | prin
      | if
      | for
      | whil
Decl -> bsTy Tok_ID ;
bsty -> int|bool
Assi -> Tok_ID = Expr ;
Prin -> print ( Expr ) ;
if ---> if ( Expr ) { stmt } Else
Else -> else { stmt }
      | (nothing b/c don't need else statement)
For --> for ( Tok_ID from Expr to Expr ) { stmt }
Whil -> while ( Expr ) { stmt } 

The idea is to take a list of tokens and search for the statement token it matches
Then, go to that parsing function. In the function, match the tokens to get the values
then return the type it is (check astTypes)

*)
let rec parse_stmt toks : stmt_result =
  let rec parse_Options ts =
    match lookahead ts with
    | Tok_Int_Type -> parse_Declare_Int ts Tok_Int_Type
    | Tok_Bool_Type -> failwith "unimplemented"
    | Tok_ID v -> failwith "unimplemented"
    | Tok_Print -> failwith "unimplemented"
    | Tok_If -> failwith "unimplemented"
    | Tok_While -> failwith "unimplemented"
    | _ -> failwith "no tokens to parse"
  and parse_Declare_Int ts v_type =
    let m1 = match_token ts v_type in
    match lookahead m1 with
    | Tok_ID i -> 
      let m2 = match_token m1 (Tok_ID i) in 
      let m3 = match_token m2 Tok_Semi in parse_Options m3
      (* let m3 = match_token m2 Tok_Semi in (m3, Declare(Int_Type, i)) WORKS NO RECURSION TEST WITH ts/t.c*)
      (* (Seq(Declare(Int_Type, i), parse_Options m3)) DOESN'T WORK BUT FEELS RIGHT*)
    | _ -> failwith "Expected identifier for variable"
  in parse_Options toks
  
(* let parse_main toks : stmt 
and change line 25 in main.ml to:
print_sting @@ string_of_stmt @@ Project6.Parser.parse_main toks;
replace parser.mli with val parse_main : token list -> stmt
*)
let parse_main toks : stmt =
  let (t1, a1)  = parse_stmt toks in
    match lookahead t1 with
    | EOF -> a1
    | _ -> failwith "parse_prog"
