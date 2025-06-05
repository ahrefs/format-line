Check for syntax errors with ocamlformat
  $ ocamlformat --enable-outside-detected-project ok/test.ml > ignore
  $ dune exec format-line -- -g ok/test.ml -o test_out.ml
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
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  EOL
  LIDENT "match"
  LIDENT "gadt"
  LIDENT "with"
  EOL
  BAR
  UIDENT "A"
  MINUSGREATER
  STRING "A"
  EOL
  BAR
  UNDERSCORE
  MINUSGREATER
  DOT
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  BACKQUOTE
  UIDENT "A"
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  EOL
  LIDENT "let"
  PERCENT
  LIDENT "lwt"
  LIDENT "a"
  EQUAL
  UIDENT "Lwt"
  DOT
  LIDENT "return"
  INT
  LIDENT "in"
  EOL
  LIDENT "a"
  EOL
  EOL
  LIDENT "let"
  LIDENT "f"
  COLON
  LIDENT "type"
  LIDENT "a"
  DOT
  OPTLABEL
  UNDERSCORE
  MINUSGREATER
  LIDENT "a"
  LIDENT "kind"
  MINUSGREATER
  LIDENT "unit"
  MINUSGREATER
  LIDENT "a"
  LIDENT "t"
  EQUAL
  LIDENT "f"
  EOL
  EOL
  LIDENT "let"
  LIDENT "f"
  COLON
  LIDENT "type"
  LIDENT "a"
  DOT
  LIDENT "locale"
  COLON
  UNDERSCORE
  MINUSGREATER
  LIDENT "a"
  LIDENT "kind"
  MINUSGREATER
  LIDENT "unit"
  MINUSGREATER
  LIDENT "a"
  LIDENT "t"
  EQUAL
  LIDENT "f"
  EOL
  EOL
  LIDENT "let"
  LIDENT "f"
  COLON
  LIDENT "type"
  LIDENT "a"
  LIDENT "b"
  LIDENT "c"
  LIDENT "d"
  DOT
  LIDENT "locale"
  COLON
  UNDERSCORE
  MINUSGREATER
  LIDENT "a"
  LIDENT "kind"
  MINUSGREATER
  LIDENT "unit"
  MINUSGREATER
  LIDENT "a"
  LIDENT "t"
  EQUAL
  LIDENT "f"
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  LPAREN
  UIDENT "Typ"
  DOT
  LIDENT "alias"
  LBRACKETPERCENT
  LIDENT "type"
  COLON
  LESS
  DOTDOT
  GREATER
  RBRACKET
  LIDENT "t"
  RPAREN
  EOL
  EOL
  LIDENT "type"
  LIDENT "t"
  EQUAL
  LBRACKETGREATER
  BACKQUOTE
  UIDENT "A"
  LIDENT "of"
  LBRACKET
  BAR
  LIDENT "t"
  RBRACKET
  RBRACKET
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  LIDENT "func"
  LABEL "arg"
  STRING "my string"
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  LIDENT "func"
  LABEL "arg"
  STRING "my string"
  EOL
  EOL
  LIDENT "let"
  LIDENT "l"
  EQUAL
  LBRACKETPERCENT
  LIDENT "ext"
  STRING "aaaa"
  RBRACKET
  EOL
  EOL
  LBRACKETATATAT
  LIDENT "attr"
  STRING "my attr"
  RBRACKET
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  LIDENT "f"
  LBRACKETATAT
  LIDENT "attr"
  STRING "my attr"
  RBRACKET
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  LIDENT "begin"
  LBRACKETAT
  LIDENT "attr"
  STRING "my attr"
  RBRACKET
  LIDENT "f"
  LIDENT "end"
  EOL
  EOL
  LIDENT "let"
  LIDENT "f"
  QUESTION
  LPAREN
  LIDENT "optionnal"
  EQUAL
  LIDENT "default"
  RPAREN
  EQUAL
  LIDENT "g"
  LIDENT "optionnal"
  EOL
  EOL
  LIDENT "module"
  LIDENT "type"
  UIDENT "S"
  EQUAL
  LIDENT "sig"
  EOL
  LIDENT "val"
  LIDENT "f"
  COLON
  LIDENT "arg"
  COLON
  LIDENT "t"
  MINUSGREATER
  LIDENT "unit"
  EOL
  EOL
  LIDENT "type"
  QUOTE
  LIDENT "a"
  LIDENT "t"
  EQUAL
  QUOTE
  LIDENT "a"
  LIDENT "list"
  EOL
  LIDENT "end"
  EOL
  EOL
  LIDENT "let"
  LIDENT "a"
  EQUAL
  LIDENT "obj"
  HASH
  LIDENT "meth"
  EOL
  EOL
  LIDENT "let"
  LIDENT "f"
  QUESTION
  LIDENT "arg1"
  QUESTION
  LIDENT "arg2"
  EQUAL
  LIDENT "g"
  LIDENT "arg1"
  LIDENT "arg2"
  EOF
Check that the output has no syntax error
  $ ocamlformat --enable-outside-detected-project test_out.ml > ignore
  $ cat test_out.ml
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
  
  let a =
    match gadt with
    | A -> "A"
    | _ -> .
  
  let a = `A
  
  let a =
    let%lwt a = Lwt.return 1 in
    a
  
  let f : type a. ?locale: _ -> a kind -> unit -> a t = f
  
  let f : type a. locale: _ -> a kind -> unit -> a t = f
  
  let f : type a b c d. locale: _ -> a kind -> unit -> a t = f
  
  let a = (Typ.alias [%type: < .. > ] t)
  
  type t = [> `A of [ | t]]
  
  let a = func ~arg:"my string"
  
  let a = func ~arg:{|my string|}
  
  let l = [%ext {|aaaa|}]
  
  [@@@attr "my attr"]
  
  let a = f [@@attr "my attr"]
  
  let a = begin [@attr "my attr"] f end
  
  let f ?(optionnal = default) = g optionnal
  
  module type S = sig
    val f : arg: t -> unit
  
    type 'a t = 'a list
  end
  
  let a = obj#meth
  
  let f ?arg1 ?arg2 = g arg1 arg2
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
  >   { token: Parser.token; startpos: Lexing.position; endpos: Lexing.position }
  8c8
  < let name_of_token (token : Parser.token) =
  ---
  > let name_of_token (token: Parser.token) =
  265c265
  < let separated_by_whitespace (tok : Parser.token) (tok' : Parser.token) =
  ---
  > let separated_by_whitespace (tok: Parser.token) (tok': Parser.token) =
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

$   [123]
  $ cat files/string.ml
  let mystring = "start




