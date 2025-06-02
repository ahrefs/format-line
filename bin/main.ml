open Ahrefs_whitespace

let format ~source ~output =
  let lexbuf = Lexing.from_string source in
  let lexer = lexer lexbuf in
  let output = output_string output in
  let first_token = lexer () in
  format ~output ~source ~lexer ~previous_token:first_token

open Cmdliner

let ( let* ) = Result.bind

let cmd =
  let term =
    let open Term.Syntax in
    let+ source =
      Arg.(required & pos 0 (some file) None (info ~doc:"input file" []))
    and+ output =
      Arg.(value & opt (some file) None (info ~doc:"output" ["o"; "output"]))
    and+ inline =
      Arg.(value & flag (info ~doc:"inline formatting" ["i"; "inline"]))
    and+ format = Term.const format in
    let source_content = In_channel.(with_open_bin source input_all) in
    let* output =
      match (output, inline) with
      | Some _, true ->
          Error "--output and --inline cannot be specified at the same time."
      | Some output, false ->
          Ok (Out_channel.open_bin output)
      | None, true ->
          Ok (Out_channel.open_bin source)
      | None, false ->
          Ok stdout
    in
    Ok (format ~source:source_content ~output)
  in
  Cmd.v (Cmd.info "format-line") term

let () = exit (Cmd.eval_result cmd)
