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
  | STRING _ ->
      "STRING"
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

let separated_by_whitespace (tok : Parser.token) (tok' : Parser.token) =
  match (tok, tok') with
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
    , RPAREN ) ->
      true
  | _, (RPAREN | SEMI | COMMA | DOT | RBRACKET | EOL | EOF)
  | (LPAREN | DOT | TILDE | LBRACKET | PREFIXOP _ | BANG | LABEL _ | DOTOP _), _
    ->
      false
  | _ ->
      true

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

let lexer lexbuf =
 fun () ->
  let startpos = Lexing.lexeme_end_p lexbuf in
  let token = Lexer.token_with_comments lexbuf in
  let endpos = Lexing.lexeme_end_p lexbuf in
  {token; startpos; endpos}

let string_of_token ~source {token= _; startpos; endpos} =
  let r =
    String.(
      source
      |> sub ~start:startpos.pos_cnum ~stop:endpos.pos_cnum
      |> Sub.to_string )
  in
  r

let rec format ~debug ~output ~source ~lexer ~previous_token =
  let token = lexer () in
  if debug then print_endline (name_of_token token.token) ;
  let str_prev_tok =
    String.trim
      ~drop:(function ' ' -> true | _ -> false)
      (string_of_token ~source previous_token)
  in
  let whitespaces, _ = whitespace_and_other (string_of_token ~source token) in
  let should_separate_by_withespace =
    separated_by_whitespace previous_token.token token.token
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

let format ~debug ~output ~source ~lexer =
  try
    let first_token = lexer () in
    format ~debug ~output ~source ~lexer ~previous_token:first_token ;
    Ok ()
  with Lexer.Error _ as e -> (
    match Location.error_of_exn e with
    | Some `Already_displayed | None ->
        assert false
    | Some (`Ok report) ->
        Error (Format.asprintf "%a" Location.print_report report) )
