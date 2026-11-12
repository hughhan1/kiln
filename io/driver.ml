module Json = Kiln_planning.Json
module Planner = Kiln_planning.Planner
module Provider = Kiln_planning.Provider
module Request = Kiln_planning.Request
module Source = Kiln_planning.Source
module Support = Kiln_planning.Support
module Error = Kiln_planning.Error
open Result.Syntax

let filesystem action =
  try action () with
  | Sys_error message -> Error (Error.Filesystem message)
  | Unix.Unix_error (error, operation, path) ->
      Error
        (Error.Filesystem
           (operation ^ " " ^ path ^ ": " ^ Unix.error_message error))

let read path = In_channel.with_open_bin path In_channel.input_all
let check cancelled = if cancelled () then Error Error.Cancelled else Ok ()

let contained exclude root path =
  (path = root || String.starts_with ~prefix:(Filename.concat root "") path)
  && not
       (List.exists
          (fun part -> List.mem part exclude)
          (String.split_on_char '/'
             (String.drop_first (String.length root) path)))

let rec walk cancelled exclude root relative =
  let* () = check cancelled in
  let dir = Filename.concat root relative in
  let entries = List.sort String.compare (Array.to_list (Sys.readdir dir)) in
  let visit entry =
    let* () = check cancelled in
    let path = Filename.concat dir entry in
    let rel = Filename.concat relative entry in
    if List.mem entry exclude then Ok ([], false)
    else
      match (Unix.lstat path).Unix.st_kind with
      | Unix.S_DIR -> walk cancelled exclude root rel
      | Unix.S_REG -> Ok ([ rel ], false)
      | Unix.S_LNK ->
          if contained exclude root (Unix.realpath path) then Ok ([ rel ], true)
          else Error (Error.Containment rel)
      | Unix.S_CHR | Unix.S_BLK | Unix.S_FIFO | Unix.S_SOCK ->
          Error (Error.Unsupported "source contains non-regular file")
  in
  let* results = Support.traverse visit entries in
  Ok (List.concat_map fst results, List.exists snd results)

let source cancelled root exclude files links =
  let resolve path =
    let* () = check cancelled in
    let absolute =
      Unix.realpath
        (if Filename.is_relative path then Filename.concat root path else path)
    in
    if contained exclude root absolute then Ok absolute
    else Error (Error.Containment path)
  in
  {
    Source.files;
    links;
    read =
      (fun path ->
        filesystem (fun () ->
            let* path = resolve path in
            let+ () = check cancelled in
            read path));
    resolve_reference =
      (fun ~from reference ->
        filesystem (fun () ->
            let path =
              if Filename.is_relative reference then
                Filename.concat (Filename.dirname from) reference
              else reference
            in
            resolve path));
  }

let prepare ?(providers = Kiln_providers.Builtin.all)
    ?(cancelled = fun () -> false) fields =
  let* request = Request.decode fields in
  filesystem @@ fun () ->
  let* () = check cancelled in
  if
    Filename.is_relative request.Request.source
    || not (Sys.is_directory request.Request.source)
  then Error (Error.Filesystem "source must be an existing absolute directory")
  else
    let root = Unix.realpath request.Request.source in
    let files = Array.to_list (Sys.readdir root) |> List.sort String.compare in
    let config = request.Request.config_file in
    let files =
      if Sys.file_exists (Filename.concat root config) then config :: files
      else files
    in
    let probe = source cancelled root [ ".git" ] files false in
    let* request = Request.configure request probe in
    let* selection =
      Provider.select providers ~requested:request.Request.provider probe
    in
    let (module P) = selection.Provider.provider in
    let exclude = ".git" :: P.exclude in
    let* files, links = walk cancelled exclude root "" in
    let source = source cancelled root exclude files links in
    let* prepared = Planner.prepare selection request source in
    let+ () = check cancelled in
    prepared

let plan ?providers ?cancelled bytes =
  let* fields = Json.decode "request" Json.object_ bytes in
  let+ prepared = prepare ?providers ?cancelled fields in
  Planner.encode prepared
