open Result.Syntax

type argv = { executable : string; arguments : string list }

let clean value = not (String.contains value '\000')

let single_line value =
  clean value
  && not (String.exists (function '\n' | '\r' -> true | _ -> false) value)

let docker_quote value =
  "\""
  ^ String.concat ""
      (List.map
         (function
           | ('\\' | '"' | '$') as c -> "\\" ^ String.make 1 c
           | c -> String.make 1 c)
         (List.of_seq (String.to_seq value)))
  ^ "\""

let argv = function
  | executable :: arguments
    when executable <> "" && List.for_all clean (executable :: arguments) ->
      Ok { executable; arguments }
  | [] | _ :: _ ->
      Error
        (Error.Metadata
           ("command", "expected nonempty executable-and-arguments without NUL"))

let arguments argv = argv.executable :: argv.arguments

let shell script =
  if String.trim script = "" then
    Error (Error.Metadata ("command", "expected nonempty shell command"))
  else argv [ "/bin/sh"; "-c"; script ]

type image = string

let image value =
  match String.split_last ~sep:"@sha256:" value with
  | Some (repository, digest)
    when repository <> ""
         && String.length digest = 64
         && String.for_all
              (function 'a' .. 'f' | '0' .. '9' -> true | _ -> false)
              digest
         && not
              (String.exists
                 (function ' ' | '\n' | '\r' | '\000' -> true | _ -> false)
                 repository) ->
      Ok value
  | None | Some _ ->
      Error
        (Error.Metadata ("image", "expected repository pinned by sha256 digest"))

let image_ref image = image

type stage = { base : base; operations : operation list }
and base = Image of image | Native of image | Stage of stage | Scratch
and input = Context | Output of stage | External of image_reference
and image_reference = string

and operation =
  | Run of argv
  | Copy of {
      input : input;
      paths : string list;
      exclude : string list;
      destination : string;
      owner : (int * int) option;
    }
  | Workdir of string
  | Env of (string * string) list
  | User of int * int
  | Entrypoint
  | Start of argv option
  | Prepend_path of string
  | Isolated of { command : argv; mounts : (input * string) list }

let validate_operations operations =
  let valid = function
    | Copy { paths; exclude; destination; owner; _ } ->
        paths <> []
        && List.for_all (fun s -> s <> "" && clean s) (destination :: paths)
        && List.for_all
             (fun s ->
               clean s
               && not
                    (String.exists
                       (function ' ' | '\n' | '\r' -> true | _ -> false)
                       s))
             exclude
        && Option.fold ~none:true ~some:(fun (u, g) -> u >= 0 && g >= 0) owner
    | Workdir path | Prepend_path path -> path <> "" && single_line path
    | User (u, g) -> u >= 0 && g >= 0
    | Isolated { mounts; _ } ->
        List.for_all
          (fun (_, target) ->
            String.starts_with ~prefix:"/" target
            && single_line target
            && not
                 (String.exists
                    (function ',' | ' ' -> true | _ -> false)
                    target))
          mounts
    | Run _ | Env _ | Entrypoint | Start _ -> true
  in
  if List.for_all valid operations then Ok ()
  else Error (Error.Metadata ("build plan", "invalid operation"))

let stage base operations =
  let+ () = validate_operations operations in
  { base; operations }

type t = stage

let base stage = stage.base
let operations stage = stage.operations

let stages root =
  let rec visit seen stage =
    if List.exists (fun other -> stage == other) seen then seen
    else
      let seen =
        match stage.base with
        | Image _ | Native _ | Scratch -> seen
        | Stage parent -> visit seen parent
      in
      let input seen = function
        | Output stage -> visit seen stage
        | Context | External _ -> seen
      in
      let seen =
        List.fold_left
          (fun seen -> function
            | Copy { input = from; _ } -> input seen from
            | Isolated { mounts; _ } ->
                List.fold_left
                  (fun seen (from, _) -> input seen from)
                  seen mounts
            | Run _ | Workdir _ | Env _ | User _ | Entrypoint | Start _
            | Prepend_path _ ->
                seen)
          seen stage.operations
      in
      stage :: seen
  in
  List.rev (visit [] root)

let run args =
  let+ args = argv args in
  Run args

let context ?(exclude = []) paths destination =
  Copy { input = Context; paths; exclude; destination; owner = None }

let copy ?(exclude = []) ?owner stage paths destination =
  Copy { input = Output stage; paths; exclude; destination; owner }

let external_image reference =
  if
    reference <> ""
    && (not (String.starts_with ~prefix:"-" reference))
    && String.for_all
         (function
           | 'a' .. 'z'
           | 'A' .. 'Z'
           | '0' .. '9'
           | '/' | ':' | '@' | '.' | '_' | '-' ->
               true
           | _ -> false)
         reference
  then Ok (External reference)
  else Error (Error.Metadata ("image", "invalid image reference"))

let dockerfile root =
  let rendered = ref [] in
  let output = Buffer.create 1024 in
  Buffer.add_string output "# syntax=docker/dockerfile:1.19\n";
  let json values = Json.encode (Json.string_array values) in
  (* Render dependencies before assigning a name, so every reference is already
     defined. Physical identity preserves sharing without merging equal stages. *)
  let rec name stage =
    match List.find_opt (fun (other, _) -> stage == other) !rendered with
    | Some (_, name) -> name
    | None ->
        let base =
          match stage.base with
          | Image image -> image_ref image
          | Native image -> "--platform=$BUILDPLATFORM " ^ image_ref image
          | Stage parent -> name parent
          | Scratch -> "scratch"
        in
        let operations = List.map operation stage.operations in
        let name = "s" ^ string_of_int (List.length !rendered) in
        rendered := (stage, name) :: !rendered;
        Buffer.add_string output ("FROM " ^ base ^ " AS " ^ name ^ "\n");
        (match stage.base with
        | Native _ -> Buffer.add_string output "ARG TARGETARCH\n"
        | Image _ | Stage _ | Scratch -> ());
        List.iter
          (fun line -> Buffer.add_string output (line ^ "\n"))
          operations;
        name
  and from = function
    | Context -> None
    | Output stage -> Some (name stage)
    | External reference -> Some reference
  and operation = function
    | Run args -> "RUN " ^ json (arguments args)
    | Isolated { command; mounts } ->
        "RUN --network=none "
        ^ String.concat ""
            (List.map
               (fun (input, target) ->
                 "--mount=type=bind,"
                 ^ Option.fold ~none:""
                     ~some:(fun from -> "from=" ^ from ^ ",")
                     (from input)
                 ^ "target=" ^ target ^ ",readonly ")
               mounts)
        ^ json (arguments command)
    | Copy { input; paths; exclude; destination; owner } ->
        let prefix =
          Option.fold ~none:""
            ~some:(fun from -> "--from=" ^ from ^ " ")
            (from input)
        in
        let owner =
          Option.fold ~none:""
            ~some:(fun (u, g) -> Printf.sprintf "--chown=%d:%d " u g)
            owner
        in
        "COPY " ^ prefix ^ owner
        ^ String.concat ""
            (List.mapi
               (fun i p ->
                 (if i > 0 && i mod 64 = 0 then "\\\n  " else "")
                 ^ "--exclude=" ^ p ^ " ")
               exclude)
        ^ json (paths @ [ destination ])
    | Workdir path -> "WORKDIR " ^ Json.encode (`String path)
    | Env pairs ->
        "ENV "
        ^ String.concat " "
            (List.map
               (fun (key, value) -> key ^ "=" ^ docker_quote value)
               pairs)
    | User (u, g) -> Printf.sprintf "USER %d:%d" u g
    | Entrypoint -> "ENTRYPOINT []"
    | Start args -> "CMD " ^ json (Option.fold ~none:[] ~some:arguments args)
    | Prepend_path path -> "ENV PATH=" ^ docker_quote path ^ ":\"${PATH}\""
  in
  ignore (name root);
  Buffer.contents output
