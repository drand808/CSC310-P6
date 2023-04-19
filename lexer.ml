open TokenTypes

(* Combine all string options and then regex on those. 
ex) "while0" -> Tok_ID("while0") since a while loop MUST be only "while"
*)
let tokenize (input : string) : token list =
  let re_white = Str.regexp "[ \t\n\r]" in
  let re_lparen = Str.regexp "(" in
  let re_rparen = Str.regexp ")" in
  let re_lbrace = Str.regexp "{" in
  let re_rbrace = Str.regexp "}" in
  let re_equal = Str.regexp "==" in
  let re_notequal = Str.regexp "!=" in
  let re_assign = Str.regexp "=" in
  let re_greater = Str.regexp ">" in
  let re_less = Str.regexp "<" in
  let re_greaterEqual = Str.regexp ">=" in
  let re_lessEqual = Str.regexp "<=" in
  let re_or = Str.regexp "||" in
  let re_and = Str.regexp "&&" in
  let re_not = Str.regexp "!" in
  let re_semi = Str.regexp ";" in
  let re_int_type = Str.regexp "int" in
  let re_bool_type = Str.regexp "bool" in
  let re_print = Str.regexp "print" in
  let re_main = Str.regexp "main" in
  let re_if = Str.regexp "if" in
  let re_else = Str.regexp "else" in
  let re_for = Str.regexp "for" in
  let re_from = Str.regexp "from" in
  let re_to = Str.regexp "to" in
  let re_while = Str.regexp "while" in
  let re_add = Str.regexp "+" in
  let re_sub = Str.regexp "-" in
  let re_mult = Str.regexp "*" in
  let re_div = Str.regexp "/" in
  let re_pow = Str.regexp "\\^" in
  let re_bool = Str.regexp "true\\|false" in
  let re_int = Str.regexp "-?[1-9][0-9]*" in
  let re_id = Str.regexp "[a-zA-z][a-zA-Z0-9]*" in
  let rec tok pos s =
    if pos >= String.length s then [EOF]
    else if Str.string_match re_white s pos then tok (pos + 1) s
    else if Str.string_match re_lparen s pos then Tok_LParen :: tok (pos + 1) s
    else if Str.string_match re_rparen s pos then Tok_RParen :: tok (pos + 1) s
    else if Str.string_match re_lbrace s pos then Tok_LBrace :: tok (pos + 1) s
    else if Str.string_match re_rbrace s pos then Tok_RBrace :: tok (pos + 1) s
    else if Str.string_match re_equal s pos then Tok_Equal :: tok (pos+2) s
    else if Str.string_match re_notequal s pos then Tok_NotEqual :: tok (pos+2) s
    else if Str.string_match re_assign s pos then Tok_Assign :: tok (pos+1) s
    else if Str.string_match re_greater s pos then Tok_Greater :: tok (pos+1) s
    else if Str.string_match re_less s pos then Tok_Less :: tok (pos+1) s
    else if Str.string_match re_greaterEqual s pos then Tok_GreaterEqual :: tok (pos+2) s
    else if Str.string_match re_lessEqual s pos then Tok_LessEqual :: tok (pos+2) s
    else if Str.string_match re_or s pos then Tok_Or :: tok (pos+2) s
    else if Str.string_match re_and s pos then Tok_And :: tok (pos+2) s
    else if Str.string_match re_not s pos then Tok_Not :: tok (pos+1) s
    else if Str.string_match re_semi s pos then Tok_Semi :: tok (pos+1) s
    else if Str.string_match re_add s pos then Tok_Add :: tok (pos+1) s
    else if Str.string_match re_sub s pos then Tok_Sub :: tok (pos+1) s
    else if Str.string_match re_mult s pos then Tok_Mult :: tok (pos+1) s
    else if Str.string_match re_div s pos then Tok_Div :: tok (pos+1) s
    else if Str.string_match re_pow s pos then Tok_Pow :: tok (pos+1) s
    else if Str.string_match re_int s pos then 
      let value = Str.matched_string s
      in Tok_Int (int_of_string value) :: tok (pos+(String.length value)) s
    else if Str.string_match re_bool s pos then
      match Str.matched_string s with
      | "true" -> Tok_Bool true :: tok (pos+4) s
      | "false" -> Tok_Bool false :: tok (pos+5) s
      | _ -> failwith "expected boolean"
    else if Str.string_match re_id s pos then
      let value = Str.matched_string s in
      match value with
      | "while" -> Tok_While :: tok (pos+5) s
      | "print" -> Tok_Print :: tok (pos+5) s
      | "bool" -> Tok_Bool_Type :: tok (pos+4) s
      | "main" -> Tok_Main :: tok (pos+4) s
      | "else" -> Tok_Else :: tok (pos+4) s
      | "from" -> Tok_From :: tok (pos+4) s
      | "int" -> Tok_Int_Type :: tok (pos+3) s
      | "for" -> Tok_For :: tok (pos+3) s
      | "to" -> Tok_To :: tok (pos+2) s
      | "if" -> Tok_If :: tok (pos+2) s
      | _ -> Tok_ID value :: tok (pos+(String.length value)) s 
      
    (*
    else if Str.string_match re_id s pos then 
      let token = Str.matched_string s in Tok_ID token :: tok (pos + (String.length s)) s
    else if Str.string_match re_while s pos then Tok_While :: tok (pos + 5) s
    else if Str.string_match re_print s pos then Tok_Print :: tok (pos + 5) s
    else if Str.string_match re_bool_type s pos then Tok_Bool_Type :: tok (pos+4) s
    else if Str.string_match re_main s pos then Tok_Main :: tok (pos+4) s
    else if Str.string_match re_else s pos then Tok_Else :: tok (pos+4) s
    else if Str.string_match re_from s pos then Tok_From :: tok (pos+4) s
    else if Str.string_match re_int_type s pos then Tok_Int_Type :: tok (pos+3) s
    else if Str.string_match re_for s pos then Tok_For :: tok (pos+3) s
    else if Str.string_match re_to s pos then Tok_To :: tok (pos+2) s
    else if Str.string_match re_if s pos then Tok_If :: tok (pos+3) s
    *)
    else raise (InvalidInputException ("tokenize: " ^ s))
  in
  tok 0 input
