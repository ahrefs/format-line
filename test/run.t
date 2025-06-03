  $ dune exec format-line -- ok/test.ml -g
  LIDENT "a"
  EQUAL
  LIDENT "f"
  LABEL "x"
  LIDENT "y"
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  LIDENT "f"
  TILDE
  LIDENT "x"
  LIDENT "y"
  EOL
  EOL
  LIDENT "let"
  LPAREN
  STAR
  RPAREN
  EQUAL
  UIDENT "Float"
  DOT
  LIDENT "mul"
  EOL
  EOL
  LIDENT "let"
  LPAREN
  INFIXOP3
  RPAREN
  EQUAL
  UIDENT "Int"
  DOT
  LIDENT "mul"
  EOL
  EOL
  LIDENT "let"
  LPAREN
  INFIXOP3
  RPAREN
  EQUAL
  UIDENT "Float"
  DOT
  LIDENT "div"
  EOL
  EOL
  LIDENT "let"
  LPAREN
  MINUS
  RPAREN
  EQUAL
  UIDENT "Float"
  DOT
  LIDENT "div"
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  LPAREN
  MINUS
  INT
  RPAREN
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  LPAREN
  PREFIXOP
  INT
  RPAREN
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  BANG
  LIDENT "myref"
  EOL
  EOL
  LIDENT "let"
  LPAREN
  LETOP "let*"
  RPAREN
  EQUAL
  UIDENT "Result"
  DOT
  LIDENT "bind"
  EOL
  EOL
  LIDENT "let"
  LPAREN
  ANDOP "and+"
  RPAREN
  EQUAL
  UIDENT "Cmdliner"
  DOT
  UIDENT "Term"
  DOT
  UIDENT "Arg"
  DOT
  LPAREN
  ANDOP "and+"
  RPAREN
  EOL
  EOL
  LIDENT "let"
  LPAREN
  DOTOP "%"
  LBRACKET
  RBRACKET
  RPAREN
  EQUAL
  UIDENT "Bytes"
  DOT
  LIDENT "get"
  EOF
  let a = f ~x:y
  
  let a = f ~x y
  
  let ( * ) = Float.mul
  
  let ( *. ) = Int.mul
  
  let ( / ) = Float.div
  
  let ( - ) = Float.div
  
  let a = ( - 3)
  
  let a = (~-3)
  
  let a = !myref
  
  let ( let* ) = Result.bind
  
  let ( and+ ) = Cmdliner.Term.Arg.( and+ )
  
  let (.%[]) = Bytes.get
  $ dune exec format-line -- errors/string.ml -g
  LIDENT "mystring"
  EQUAL
  format-line: File "errors/string.ml", line 1, characters 15-16:
               Error: String literal not terminated
               
  [123]
  $ dune exec format-line -- errors/comment.ml -g
  LIDENT "a"
  EQUAL
  format-line: File "errors/comment.ml", line 1, characters 8-10:
               Error: Comment not terminated
               
  [123]
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
  276,277c276,277
  <       | LETOP _ ) )
  <   | ( ( STAR
  ---
  >       | LETOP _))
  >   | ((STAR
  285,286c285,286
  <       | LETOP _ )
  <     , RPAREN ) ->
  ---
  >       | LETOP _)
  >     , RPAREN) ->
  304,305c304,305
  <   ( whitespaces |> List.rev_map String.of_char |> String.concat
  <   , String.sub ~start:i str |> String.Sub.to_string )
  ---
  >   (whitespaces |> List.rev_map String.of_char |> String.concat
  >   , String.sub ~start:i str |> String.Sub.to_string)
  312c312
  <   {token; startpos; endpos}
  ---
  >   { token; startpos; endpos }
  314c314
  < let string_of_token ~source {token= _; startpos; endpos} =
  ---
  > let string_of_token ~source { token = _; startpos; endpos } =
  319c319
  <       |> Sub.to_string )
  ---
  >       |> Sub.to_string)
  325c325
  <   if debug then print_endline (name_of_token token.token) ;
  ---
  >   if debug then print_endline (name_of_token token.token);
  344c344
  <   output str_prev_tok ;
  ---
  >   output str_prev_tok;
  348c348
  <   else if should_separate_by_withespace then output " " ;
  ---
  >   else if should_separate_by_withespace then output " ";
  [1]
  $ dune exec format-line -- **/*.ml
  format-line: --inline needs to be specified when giving multiple inputs
  [123]
  $ dune exec format-line -- -i **/*.ml
  Formatted ok/ahrefs_whitespace.ml. Changes were made.
  Formatted ok/test.ml. Changes were made.
  format-line: File "errors/comment.ml", line 1, characters 8-10:
               Error: Comment not terminated
               
               
               File "errors/string.ml", line 1, characters 15-16:
               Error: String literal not terminated
               
               Error: ok/ahrefs_whitespace.ml: Permission denied
               Error: ok/test.ml: Permission denied
  [123]
  $ mkdir files
  $ cp **/*.ml files
  $ ls files
  ahrefs_whitespace.ml
  comment.ml
  string.ml
  test.ml
  $ chmod 777 files/*.ml
  $ cat files/string.ml
  let mystring = "start
  $ dune exec format-line -- -i files/*.ml
  Formatted files/ahrefs_whitespace.ml. Changes were made.
  Formatted files/test.ml. Changes were made.
  format-line: File "files/comment.ml", line 1, characters 8-10:
               Error: Comment not terminated
               
               
               File "files/string.ml", line 1, characters 15-16:
               Error: String literal not terminated
               
  [123]
  $ cat files/string.ml
  let mystring = "start
  $ dune exec format-line -- -i files/*.ml
  Formatted files/ahrefs_whitespace.ml. No changes were made.
  Formatted files/test.ml. No changes were made.
  format-line: File "files/comment.ml", line 1, characters 8-10:
               Error: Comment not terminated
               
               
               File "files/string.ml", line 1, characters 15-16:
               Error: String literal not terminated
               
  [123]
  $ cat files/string.ml
  let mystring = "start




