open Result.Syntax

type fetch = string -> string option -> (string, Kiln_core.Error.t) result

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))

let memoize fetch =
  let cache = Hashtbl.create 8 in
  fun url token ->
    match Hashtbl.find_opt cache (url, token) with
    | Some value -> Ok value
    | None ->
        let+ value = fetch url token in
        Hashtbl.add cache (url, token) value;
        value

let get fetch repository resource =
  let* bytes =
    fetch
      ("https://auth.docker.io/token?service=registry.docker.io&scope=repository:"
     ^ repository ^ ":pull")
      None
  in
  let* fields = Json.decode "registry token" Json.object_ bytes in
  let* token = Json.field "token" Json.string fields in
  fetch
    ("https://registry-1.docker.io/v2/" ^ repository ^ "/" ^ resource)
    (Some token)

let tags fetch repository =
  let* bytes = get fetch repository "tags/list" in
  let* fields = Json.decode "image tags" Json.object_ bytes in
  Json.field "tags" (Json.list Json.string) fields

let select fetch ~repository ~exact ~parse ~compare ~matches ~unavailable =
  let* candidates =
    match exact with
    | Some version -> Ok [ version ]
    | None ->
        let+ tags = tags fetch repository in
        List.filter_map parse tags
  in
  Support.maximum ~compare (List.filter matches candidates)
  |> Option.to_result ~none:unavailable

let image fetch ~platforms ~repository tag =
  let* bytes = get fetch repository ("manifests/" ^ tag) in
  let platform label value =
    let* fields = Json.object_ label value in
    let* fields = Json.field "platform" Json.object_ fields in
    let* os = Json.field "os" Json.string fields in
    let+ arch = Json.field "architecture" Json.string fields in
    os ^ "/" ^ arch
  in
  let* fields = Json.decode "image index" Json.object_ bytes in
  let* available = Json.field "manifests" (Json.list platform) fields in
  if
    List.for_all
      (fun p -> List.mem p available)
      (Platforms.to_strings platforms)
  then Build_plan.image ("docker.io/" ^ repository ^ "@sha256:" ^ sha256 bytes)
  else Error (Kiln_core.Error.Target_unavailable (repository ^ ":" ^ tag))
