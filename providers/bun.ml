module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Package = Kiln_planning.Package
module Platforms = Kiln_planning.Platforms
module Provider = Kiln_planning.Provider
module Request = Kiln_planning.Request
module Source = Kiln_planning.Source
module Support = Kiln_planning.Support
open Result.Syntax
module Error = Kiln_core.Error

let id = "bun"
let public_name = "node"
let exclude = [ "node_modules"; ".bun" ]
let locks = [ "bun.lock"; "bun.lockb" ]

let detect source =
  if List.exists (Source.has source) (".bun-version" :: locks) then
    Ok Provider.Explicit
  else if not (Source.has source "package.json") then Ok Provider.No_match
  else
    let+ pkg = Javascript.root source in
    if
      List.exists
        (fun d -> d.Javascript.name = id)
        (pkg.Javascript.managers @ pkg.Javascript.runtimes)
    then Provider.Explicit
    else Provider.No_match

type application = {
  platforms : Platforms.t;
  requirements : Semver_image.requirements;
  install : Build_plan.argv;
  build : Build_plan.operation list;
  start : Build_plan.argv option;
}

type resolution = Semver_image.t

let load request source =
  let* packages = Javascript.load source in
  let* root =
    Option.to_result
      ~none:(Error.Metadata ("Bun", "requires a root package.json"))
      (List.assoc_opt "package.json" packages)
  in
  let package_requirements (path, pkg) =
    let* requirements = Javascript.requirements ~path ~runtime:id pkg in
    let+ () =
      match pkg.Javascript.declared_integrity with
      | None -> Ok ()
      | Some _ ->
          Json.metadata "Bun"
            "packageManager checksum format is unsupported; use a version \
             declaration (the upstream image is digest pinned)"
    in
    requirements
  in
  let* requirements = Support.traverse package_requirements packages in
  let* file_version = Source.version source ".bun-version" in
  let declarations =
    Request.defaults request id
      (Package.optional ".bun-version" file_version @ List.concat requirements)
  in
  let* requirements = Semver_image.requirements declarations in
  let* build_argv = Build_plan.argv [ "bun"; "run"; "build" ] in
  let* start =
    Javascript.startup root ~override:(Request.start request)
      ~script:[ "bun"; "run"; "start" ] ~main:[ "bun"; "run" ]
  in
  let+ install =
    Request.install_command request
      ([ "bun"; "install" ]
      @
      if List.exists (Source.has source) locks then [ "--frozen-lockfile" ]
      else [])
  in
  {
    platforms = request.Request.platforms;
    requirements;
    install;
    build =
      Request.build_command request
        (if Javascript.has_script root "build" then Some build_argv else None);
    start;
  }

let startup app = app.start
let fingerprint app = Semver_image.fingerprint app.requirements

let resolve ~today:_ ~fetch app =
  Semver_image.resolve ~fetch ~repository:"oven/bun" ~platforms:app.platforms
    ~accept:(fun _ -> true)
    app.requirements

let encode = Semver_image.encode
let decode = Semver_image.decode
let legacy _ _ = Error Error.Invalid_resolution

let plan app resolved =
  let open Build_plan in
  let image = Semver_image.image resolved in
  let* install_env = environment [ ("BUN_INSTALL_GLOBAL_STORE", "0") ] in
  let* runtime_env = environment [ ("NODE_ENV", "production") ] in
  let* builder =
    stage (Image image)
      ([
         User (0, 0);
         Workdir "/app";
         Env install_env;
         Source.copy ~exclude [ "." ] "/app/";
         Run app.install;
       ]
      @ app.build)
  in
  stage (Image image)
    [
      Workdir "/app";
      Env runtime_env;
      copy ~owner:(1000, 1000) builder [ "/app/" ] "/app/";
      User (1000, 1000);
      Entrypoint None;
      Start app.start;
    ]

let packages app r = [ Semver_image.package id app.requirements r ]
let metadata _ = [ ("nodeRuntime", "bun"); ("nodePackageManager", "bun") ]
