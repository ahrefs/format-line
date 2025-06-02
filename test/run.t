  $ dune exec format-line -- ok/test.ml
  let a = f ~x:y
  
  let ( * ) = Float.mul
  
  let ( *. ) = Int.mul
  
  let ( / ) = Float.div
  
  let ( - ) = Float.div
  
  let a = ( - 3)
  
  let a = (~-3)
  
  let a = !myref
  $ cat out
  cat: out: No such file or directory
  [1]
Error handling it not great
  $ dune exec format-line -- errors/string.ml -g
  LIDENT
  EQUAL
  format-line: internal error, uncaught exception:
               Lexer.Error(0, _)
               
  [125]
  $ dune exec format-line -- errors/comment.ml -g
  LIDENT
  EQUAL
  format-line: internal error, uncaught exception:
               Lexer.Error(_, _)
               
  [125]
  $ dune exec format-line -- ok/ahrefs_whitespace.ml -o ahrefs_whitespace_2.ml
  $ diff ok/ahrefs_whitespace.ml ahrefs_whitespace_2.ml
  5c5
  <   {token: Parser.token; startpos: Lexing.position; endpos: Lexing.position}
  ---
  >   { token : Parser.token; startpos : Lexing.position; endpos : Lexing.position }
  267,268c267,268
  <   | ( LPAREN
  <     , ( STAR
  ---
  >   | (LPAREN
  >     , (STAR
  275,276c275,276
  <       | INFIXOP4 _ ) )
  <   | ( ( STAR
  ---
  >       | INFIXOP4 _))
  >   | ((STAR
  283,284c283,284
  <       | INFIXOP4 _ )
  <     , RPAREN ) ->
  ---
  >       | INFIXOP4 _)
  >     , RPAREN) ->
  302,303c302,303
  <   ( whitespaces |> List.rev_map String.of_char |> String.concat
  <   , String.sub ~start:i str |> String.Sub.to_string )
  ---
  >   (whitespaces |> List.rev_map String.of_char |> String.concat
  >   , String.sub ~start:i str |> String.Sub.to_string)
  310c310
  <   {token; startpos; endpos}
  ---
  >   { token; startpos; endpos }
  312c312
  < let string_of_token ~source {token= _; startpos; endpos} =
  ---
  > let string_of_token ~source { token = _; startpos; endpos } =
  317c317
  <       |> Sub.to_string )
  ---
  >       |> Sub.to_string)
  323c323
  <   if debug then print_endline (name_of_token token.token) ;
  ---
  >   if debug then print_endline (name_of_token token.token);
  342c342
  <   output str_prev_tok ;
  ---
  >   output str_prev_tok;
  346c346
  <   else if should_separate_by_withespace then output " " ;
  ---
  >   else if should_separate_by_withespace then output " ";
  [1]
