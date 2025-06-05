open Astring
open Printf

type positionned_token =
  {token: Parser.token; startpos: Lexing.position; endpos: Lexing.position}

(** Unused, use for debugging *)
let name_of_token (token : Parser.token) =
  match token with
  | WITH ->
      "WITH"
  | WHILE ->
      "WHILE"
  | WHEN ->
      "WHEN"
  | VIRTUAL ->
      "VIRTUAL"
  | VAL ->
      "VAL"
  | UNDERSCORE ->
      "UNDERSCORE"
  | UIDENT s ->
      sprintf "UIDENT %S" s
  | TYPE ->
      "TYPE"
  | TRY ->
      "TRY"
  | TRUE ->
      "TRUE"
  | TO ->
      "TO"
  | TILDE ->
      "TILDE"
  | THEN ->
      "THEN"
  | STRUCT ->
      "STRUCT"
  | STRING (s, _loc, _sopt) ->
      sprintf "STRING %S" s
  | STAR ->
      "STAR"
  | SIG ->
      "SIG"
  | SEMISEMI ->
      "SEMISEMI"
  | SEMI ->
      "SEMI"
  | RPAREN ->
      "RPAREN"
  | REC ->
      "REC"
  | RBRACKET ->
      "RBRACKET"
  | RBRACE ->
      "RBRACE"
  | QUOTED_STRING_ITEM _ ->
      "QUOTED_STRING_ITEM"
  | QUOTED_STRING_EXPR _ ->
      "QUOTED_STRING_EXPR"
  | QUOTE ->
      "QUOTE"
  | QUESTION ->
      "QUESTION"
  | PRIVATE ->
      "PRIVATE"
  | PREFIXOP _ ->
      "PREFIXOP"
  | PLUSEQ ->
      "PLUSEQ"
  | PLUSDOT ->
      "PLUSDOT"
  | PLUS ->
      "PLUS"
  | PERCENT ->
      "PERCENT"
  | OR ->
      "OR"
  | OPTLABEL _ ->
      "OPTLABEL"
  | OPEN ->
      "OPEN"
  | OF ->
      "OF"
  | OBJECT ->
      "OBJECT"
  | NONREC ->
      "NONREC"
  | NEW ->
      "NEW"
  | MUTABLE ->
      "MUTABLE"
  | MODULE ->
      "MODULE"
  | MINUSGREATER ->
      "MINUSGREATER"
  | MINUSDOT ->
      "MINUSDOT"
  | MINUS ->
      "MINUS"
  | METHOD ->
      "METHOD"
  | METAOCAML_ESCAPE ->
      "METAOCAML_ESCAPE"
  | METAOCAML_BRACKET_OPEN ->
      "METAOCAML_BRACKET_OPEN"
  | METAOCAML_BRACKET_CLOSE ->
      "METAOCAML_BRACKET_CLOSE"
  | MATCH ->
      "MATCH"
  | LPAREN ->
      "LPAREN"
  | LIDENT s ->
      sprintf "LIDENT %S" s
  | LETOP s ->
      sprintf "LETOP %S" s
  | LET ->
      "LET"
  | LESSMINUS ->
      "LESSMINUS"
  | LESS ->
      "LESS"
  | LBRACKETPERCENTPERCENT ->
      "LBRACKETPERCENTPERCENT"
  | LBRACKETPERCENT ->
      "LBRACKETPERCENT"
  | LBRACKETLESS ->
      "LBRACKETLESS"
  | LBRACKETGREATER ->
      "LBRACKETGREATER"
  | LBRACKETBAR ->
      "LBRACKETBAR"
  | LBRACKETATATAT ->
      "LBRACKETATATAT"
  | LBRACKETATAT ->
      "LBRACKETATAT"
  | LBRACKETAT ->
      "LBRACKETAT"
  | LBRACKET ->
      "LBRACKET"
  | LBRACELESS ->
      "LBRACELESS"
  | LBRACE ->
      "LBRACE"
  | LAZY ->
      "LAZY"
  | LABEL s ->
      sprintf "LABEL %S" s
  | INT _ ->
      "INT"
  | INITIALIZER ->
      "INITIALIZER"
  | INHERIT ->
      "INHERIT"
  | INFIXOP4 _ ->
      "INFIXOP4"
  | INFIXOP3 _ ->
      "INFIXOP3"
  | INFIXOP2 _ ->
      "INFIXOP2"
  | INFIXOP1 _ ->
      "INFIXOP1"
  | INFIXOP0 _ ->
      "INFIXOP0"
  | INCLUDE ->
      "INCLUDE"
  | IN ->
      "IN"
  | IF ->
      "IF"
  | HASHOP _ ->
      "HASHOP"
  | HASH ->
      "HASH"
  | GREATERRBRACKET ->
      "GREATERRBRACKET"
  | GREATERRBRACE ->
      "GREATERRBRACE"
  | GREATER ->
      "GREATER"
  | FUNCTOR ->
      "FUNCTOR"
  | FUNCTION ->
      "FUNCTION"
  | FUN ->
      "FUN"
  | FOR ->
      "FOR"
  | FLOAT _ ->
      "FLOAT"
  | FALSE ->
      "FALSE"
  | EXTERNAL ->
      "EXTERNAL"
  | EXCEPTION ->
      "EXCEPTION"
  | EQUAL ->
      "EQUAL"
  | EOL ->
      "EOL"
  | EOF ->
      "EOF"
  | END ->
      "END"
  | ELSE ->
      "ELSE"
  | EFFECT ->
      "EFFECT"
  | DOWNTO ->
      "DOWNTO"
  | DOTOP s ->
      sprintf "DOTOP %S" s
  | DOTDOT ->
      "DOTDOT"
  | DOT ->
      "DOT"
  | DONE ->
      "DONE"
  | DOCSTRING _ ->
      "DOCSTRING"
  | DO ->
      "DO"
  | CONSTRAINT ->
      "CONSTRAINT"
  | COMMENT _ ->
      "COMMENT"
  | COMMA ->
      "COMMA"
  | COLONGREATER ->
      "COLONGREATER"
  | COLONEQUAL ->
      "COLONEQUAL"
  | COLONCOLON ->
      "COLONCOLON"
  | COLON ->
      "COLON"
  | CLASS ->
      "CLASS"
  | CHAR c ->
      sprintf "CHAR '%c'" c
  | BEGIN ->
      "BEGIN"
  | BARRBRACKET ->
      "BARRBRACKET"
  | BARBAR ->
      "BARBAR"
  | BAR ->
      "BAR"
  | BANG ->
      "BANG"
  | BACKQUOTE ->
      "BACKQUOTE"
  | ASSERT ->
      "ASSERT"
  | AS ->
      "AS"
  | ANDOP s ->
      sprintf "ANDOP %S" s
  | AND ->
      "AND"
  | AMPERSAND ->
      "AMPERSAND"
  | AMPERAMPER ->
      "AMPERAMPER"

(** compare token but ignore locations *)
let compare_tok (tok : Parser.token) (tok' : Parser.token) =
  let ( let$ ) i f = match i with 0 -> f () | i -> i in
  match (tok, tok') with
  | COMMENT (s, _loc), COMMENT (s', _loc') ->
      String.compare s s'
  | STRING (s, _loc, delim), STRING (s', _loc', delim') ->
      let$ () = String.compare s s' in
      Option.compare String.compare delim delim'
  | ( QUOTED_STRING_ITEM (s, _loc, s2, _loc2, s_opt)
    , QUOTED_STRING_ITEM (s', _loc', s2', _loc2', s_opt') ) ->
      let$ () = String.compare s s' in
      let$ () = String.compare s2 s2' in
      Option.compare String.compare s_opt s_opt'
  | ( QUOTED_STRING_EXPR (s, _loc, s2, _loc2, s_opt)
    , QUOTED_STRING_EXPR (s', _loc', s2', _loc2', s_opt') ) ->
      let$ () = String.compare s s' in
      let$ () = String.compare s2 s2' in
      Option.compare String.compare s_opt s_opt'
  | _ ->
      compare tok tok'

let string_of_token ~source {token= _; startpos; endpos} =
  let r =
    String.(
      source
      |> sub ~start:startpos.pos_cnum ~stop:endpos.pos_cnum
      |> Sub.to_string )
  in
  r

(** [has_to_be_separated tok tok'] is true if :
    - [s] and [s'] are the string corresponding to [tok] and [tok']
    - Lexing [s ^ s'] gives tokens that are different from [tok; tok']
    This means that we are forced to display `[s ^ " " ^ s'], for correctness. *)
let has_to_be_separated ~source (tok : positionned_token)
    (tok' : positionned_token) =
  match tok'.token with
  | EOF | EOL ->
      false
  | _ ->
      let string_of_tok = String.trim (string_of_token ~source tok) in
      let string_of_tok' = String.trim (string_of_token ~source tok') in
      let text = string_of_tok ^ string_of_tok' in
      Location.formatter_for_warnings :=
        Format.make_formatter (fun _ _ _ -> ()) (fun () -> ()) ;
      let lexbuf = Lexing.from_string text in
      begin
        try
          let new_tok = Lexer.token lexbuf in
          let new_tok' = Lexer.token lexbuf in
          (* printf "text:%S toks: [%s; %s] new_toks: [%s; %s]\n%!" text
            (name_of_token tok.token) (name_of_token tok'.token)
            (name_of_token new_tok) (name_of_token new_tok') ; *)
          List.compare compare_tok [new_tok; new_tok'] [tok.token; tok'.token]
          <> 0
        with Lexer.Error _ -> true
      end

(** [true] when [previous_token] is [[DOT; LIDENT _; ...; LIDENT _, LIDENT "type"; ...]] which corresponds to Ocaml [type a b c.]*)
let type_quantifier_dot (previous_tokens : positionned_token list) =
  match previous_tokens with
  | {token= DOT; _} :: {token= LIDENT n; _} :: previous_tokens when n <> "type"
    ->
      let rec loop = function
        | {token= Parser.LIDENT "type"; _} :: _ ->
            true
        | {token= LIDENT _; _} :: previous_tokens ->
            loop previous_tokens
        | _ ->
            false
      in
      loop previous_tokens
  | _ ->
      false

let separated_by_whitespace ~source (previous_toks : positionned_token list)
    (ptok' : positionned_token) =
  let tok' = ptok'.token in
  match (previous_toks, tok') with
  | [], _ ->
      false
  | {token= LIDENT _; _} :: {token= LIDENT ("val" | "let"); _} :: _, COLON
  | {token= LIDENT _; _} :: {token= TILDE; _} :: _, COLON ->
      true
  | ({token; _} as ptok) :: _, _ ->
      if has_to_be_separated ~source ptok ptok' then true
      else begin
        match (token, tok') with
        | _ when type_quantifier_dot previous_toks ->
            true
        | ( LPAREN
          , ( STAR
            | PLUS
            | MINUS
            | INFIXOP0 _
            | INFIXOP1 _
            | INFIXOP2 _
            | INFIXOP3 _
            | INFIXOP4 _
            | LETOP _
            | ANDOP _ ) )
        | ( ( STAR
            | PLUS
            | MINUS
            | INFIXOP0 _
            | INFIXOP1 _
            | INFIXOP2 _
            | INFIXOP3 _
            | INFIXOP4 _
            | LETOP _
            | ANDOP _ )
          , RPAREN )
        | MINUSGREATER, DOT
        | LBRACKET, BAR
        | GREATER, RBRACKET ->
            true
        | ( _
          , ( RPAREN
            | SEMI
            | COMMA
            | DOT
            | RBRACKET
            | PERCENT
            | COLON
            | HASH
            | EOL
            | EOF ) )
        | ( ( LPAREN
            | DOT
            | TILDE
            | LBRACKET
            | PREFIXOP _
            | BANG
            | LABEL _
            | DOTOP _
            | BACKQUOTE
            | PERCENT
            | LBRACKETPERCENT
            | LBRACKETATATAT
            | LBRACKETATAT
            | LBRACKETAT
            | QUOTE
            | HASH
            | QUESTION )
          , _ ) ->
            false
        | _ ->
            true
      end

let whitespace_and_other str =
  let rec loop whitespaces i =
    if i >= String.length str then (whitespaces, i)
    else
      let ch = str.[i] in
      if Char.Ascii.is_white str.[i] then loop (ch :: whitespaces) (i + 1)
      else (whitespaces, i)
  in
  let whitespaces, i = loop [] 0 in
  ( whitespaces |> List.rev_map String.of_char |> String.concat
  , String.sub ~start:i str |> String.Sub.to_string )

let lexer lexbuf () =
  let startpos = Lexing.lexeme_end_p lexbuf in
  let token = Lexer.token_with_comments lexbuf in
  let endpos = Lexing.lexeme_end_p lexbuf in
  {token; startpos; endpos}

let rec format ~debug ~output ~source ~lexer ~previous_token ~previous_tokens =
  let previous_tokens = previous_token :: previous_tokens in
  let token = lexer () in
  if debug then print_endline (name_of_token token.token) ;
  let str_prev_tok =
    String.trim
      ~drop:(function ' ' -> true | _ -> false)
      (string_of_token ~source previous_token)
  in
  let whitespaces, _ = whitespace_and_other (string_of_token ~source token) in
  let should_separate_by_withespace =
    separated_by_whitespace ~source previous_tokens token
  in
  let has_line_break =
    match (previous_token.token, token.token) with
    | EOL, (EOL | EOF) ->
        false
    | EOL, _ ->
        true
    | _ ->
        false
  in
  output str_prev_tok ;
  if has_line_break then begin
    output whitespaces
  end
  else if should_separate_by_withespace then output " " ;
  match token.token with
  | EOF ->
      (* no need to print ["\n"], there is an EOL token for that. *)
      ()
  | _ ->
      format ~debug ~output ~source ~lexer ~previous_token:token
        ~previous_tokens

let format ~debug ~output ~source ~lexer =
  try
    let first_token = lexer () in
    format ~debug ~output ~source ~lexer ~previous_token:first_token
      ~previous_tokens:[] ;
    Ok ()
  with Lexer.Error _ as e -> (
    match Location.error_of_exn e with
    | Some `Already_displayed | None ->
        assert false
    | Some (`Ok report) ->
        Error (Format.asprintf "%a" Location.print_report report) )
