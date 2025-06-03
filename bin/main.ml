open Ahrefs_whitespace
open Printf

let ( let* ) = Result.bind

let format ~inline ~debug ~source ~source_name ~output =
  let lexbuf = Lexing.from_string source in
  Lexing.set_filename lexbuf source_name ;
  let lexer = lexer lexbuf in
  let out_buffer = Buffer.create (String.length source) in
  let output_f = Buffer.add_string out_buffer in
  let* () = format ~debug ~output:output_f ~source ~lexer in
  let formatted_source = Buffer.contents out_buffer in
  if inline then
    printf "Formatted %s. %s\n%!" source_name
      begin if formatted_source <> source then "Changes were made."
      else "No changes were made."
      end ;
  let* output_ch = output () in
  output_string output_ch formatted_source ;
  Ok ()

open Cmdliner

let open_result file () =
  try Ok (Out_channel.open_bin file)
  with Sys_error s -> Error (sprintf "Error: %s" s)

let cmd =
  let term =
    let open Term.Syntax in
    let+ sources = Arg.(value & pos_all file [] (info ~doc:"input file" []))
    and+ output =
      Arg.(value & opt (some string) None (info ~doc:"output" ["o"; "output"]))
    and+ inline =
      Arg.(value & flag (info ~doc:"inline formatting" ["i"; "inline"]))
    and+ debug = Arg.(value & flag (info ~doc:"debug mode" ["g"; "debug"]))
    and+ format = Term.const format in
    let sources_content =
      sources
      |> List.map (fun source ->
          (In_channel.(with_open_bin source input_all), source) )
    in
    let* outputs =
      match (sources, output, inline) with
      | [], _, _ ->
          Error "No input file given"
      | _, Some _, true ->
          Error "--output and --inline cannot be specified at the same time."
      | [_], Some output, false ->
          Ok [open_result output]
      | _ :: _, None, true ->
          Ok (List.map open_result sources)
      | [_], None, false ->
          Ok [(fun () -> Ok stdout)]
      | _ :: _, _, false ->
          Error "--inline needs to be specified when giving multiple inputs"
    in
    let _, errors =
      List.map2
        begin fun (source, source_name) output ->
          format ~inline ~debug ~source ~source_name ~output
        end
        sources_content outputs
      |> List.partition_map (function Ok () -> Left () | Error s -> Right s)
    in
    match errors with
    | [] ->
        Ok ()
    | errors ->
        Error (String.concat "\n" errors)
  in
  Cmd.v (Cmd.info "format-line") term

let () = exit (Cmd.eval_result cmd)
