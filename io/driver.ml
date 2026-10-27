module Json = Kiln_planning.Json
module Planner = Kiln_planning.Planner
module Provider = Kiln_planning.Provider
module Registry = Kiln_planning.Registry
module Request = Kiln_planning.Request
module Source = Kiln_planning.Source
module Support = Kiln_planning.Support
module Error = Kiln_core.Error
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
  Support.inside root path
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

type discovery = Registry.fetch

let prepare_request ~providers ~cancelled ~today ~fetch (request : Request.t) =
  let prepare () =
    let* () = check cancelled in
    if
      Filename.is_relative request.Request.source
      || not (Sys.is_directory request.Request.source)
    then
      Error (Error.Filesystem "source must be an existing absolute directory")
    else
      let context = Unix.realpath request.Request.source in
      let root =
        Unix.realpath (Filename.concat context request.Request.directory)
      in
      let* () =
        if Support.inside context root then Ok ()
        else Error (Error.Containment request.Request.directory)
      in
      let files =
        Array.to_list (Sys.readdir root) |> List.sort String.compare
      in
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
      let fetch url token =
        let* () = check cancelled in
        let response = fetch url token in
        let* () = check cancelled in
        response
      in
      let* prepared =
        Planner.prepare selection ~today ~fetch:(Registry.memoize fetch) request
          source
      in
      let+ () = check cancelled in
      prepared
  in
  filesystem prepare

let prepare_with ?(providers = Kiln_providers.Builtin.all)
    ?(cancelled = fun () -> false) ~today ~fetch bytes =
  let* () = check cancelled in
  let* fields = Json.decode "request" Json.object_ bytes in
  let* request = Request.decode fields in
  prepare_request ~providers ~cancelled ~today ~fetch request

let plan_with ?providers ?cancelled ~today ~fetch bytes =
  let+ prepared = prepare_with ?providers ?cancelled ~today ~fetch bytes in
  Planner.encode prepared

let fetch handle cancelled url token =
  try
    let buffer = Buffer.create 4096 in
    Curl.set_url handle url;
    Curl.set_timeout handle 30;
    Curl.set_nosignal handle true;
    Curl.set_noprogress handle false;
    Curl.set_xferinfofunction handle (fun _ _ _ _ -> cancelled ());
    Curl.set_followlocation handle true;
    Curl.set_maxredirs handle 5;
    Curl.set_sslverifypeer handle true;
    Curl.set_sslverifyhost handle Curl.SSLVERIFYHOST_HOSTNAME;
    Curl.set_httpheader handle
      ([
         "Accept: application/vnd.oci.image.index.v1+json, \
          application/vnd.docker.distribution.manifest.list.v2+json, \
          application/json";
       ]
      @
      match token with
      | None -> []
      | Some token -> [ "Authorization: Bearer " ^ token ]);
    Curl.set_writefunction handle (fun chunk ->
        if Buffer.length buffer + String.length chunk > 64 * 1024 * 1024 then 0
        else (
          Buffer.add_string buffer chunk;
          String.length chunk));
    Curl.perform handle;
    let status = Curl.get_responsecode handle in
    if status = 200 then Ok (Buffer.contents buffer)
    else
      let message = Printf.sprintf "%s: HTTP %d" url status in
      Error
        (if List.mem status [ 408; 429 ] || status / 100 = 5 then
           Error.Transient message
         else Error.Discovery message)
  with Curl.CurlException (code, _, message) ->
    Error
      (let message = url ^ ": " ^ message in
       if
         List.mem code
           Curl.
             [
               CURLE_COULDNT_RESOLVE_PROXY;
               CURLE_COULDNT_RESOLVE_HOST;
               CURLE_COULDNT_CONNECT;
               CURLE_PARTIAL_FILE;
               CURLE_OPERATION_TIMEOUTED;
               CURLE_GOT_NOTHING;
               CURLE_SEND_ERROR;
               CURLE_RECV_ERROR;
             ]
       then Error.Transient message
       else Error.Discovery message)

let prepare_live ~cancelled request =
  let today = Unix.gmtime (Unix.time ()) in
  let handle = lazy (Curl.init ()) in
  Fun.protect
    ~finally:(fun () ->
      if Lazy.is_val handle then Curl.cleanup (Lazy.force handle))
    (fun () ->
      prepare_request ~providers:Kiln_providers.Builtin.all ~cancelled
        ~today:
          (Printf.sprintf "%04d-%02d-%02d"
             (today.Unix.tm_year + 1900)
             (today.Unix.tm_mon + 1) today.Unix.tm_mday)
        ~fetch:(fun url token -> fetch (Lazy.force handle) cancelled url token)
        request)

let prepare ?(cancelled = fun () -> false) bytes =
  let* () = check cancelled in
  let* fields = Json.decode "request" Json.object_ bytes in
  let* request = Request.decode fields in
  prepare_live ~cancelled request

let plan ?cancelled bytes =
  let+ prepared = prepare ?cancelled bytes in
  Planner.encode prepared
