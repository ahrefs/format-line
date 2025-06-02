open Ahrefs_whitespace

let format ~debug ~source ~output =
  let lexbuf = Lexing.from_string source in
  let lexer = lexer lexbuf in
  let out_buffer = Buffer.create (String.length source) in
  let output_f = Buffer.add_string out_buffer in
  let first_token = lexer () in
  format ~debug ~output:output_f ~source ~lexer ~previous_token:first_token ;
  out_buffer |> Buffer.contents |> output_string output

open Cmdliner

let ( let* ) = Result.bind

let cmd =
  let term =
    let open Term.Syntax in
    let+ source =
      Arg.(required & pos 0 (some file) None (info ~doc:"input file" []))
    and+ output =
      Arg.(value & opt (some string) None (info ~doc:"output" ["o"; "output"]))
    and+ inline =
      Arg.(value & flag (info ~doc:"inline formatting" ["i"; "inline"]))
      and+ debug = Arg.(value & flag (info ~doc:"debug mode" ["g"; "debug"]))
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
    Ok (format ~debug ~source:source_content ~output)
  in
  Cmd.v (Cmd.info "format-line") term

let () = exit (Cmd.eval_result cmd)
