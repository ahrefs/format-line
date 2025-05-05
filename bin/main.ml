open Ahrefs_whitespace

let () =
  let source = In_channel.(with_open_bin Sys.argv.(1) input_all) in
  let lexbuf = Lexing.from_string source in
  let lexer = lexer lexbuf in
  let output =
    if Array.length Sys.argv > 2 then begin
      let ch = Out_channel.open_bin Sys.argv.(2) in
      output_string ch
    end
    else print_string
  in
  let first_token = lexer () in
  format ~output ~source ~lexer ~previous_token:first_token
