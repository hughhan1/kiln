open Result.Syntax

type prepared = {
  plan : Build_plan.t;
  secrets : Environment.Names.t;
  resolution : Yojson.Basic.t;
  detected : string list;
  packages : Package.t list;
  metadata : (string * string) list;
}

let prepare { Provider.provider = (module P); detected } (request : Request.t)
    source =
  let* app = P.load request source in
  let* () =
    match (P.startup app, request.Request.error_missing_start) with
    | None, true -> Error Kiln_core.Error.Missing_start
    | Some _, _ | None, false -> Ok ()
  in
  let* resolved = P.resolve app in
  let* plan = P.plan app resolved in
  let runtime =
    List.map
      (fun vars -> Build_plan.Env vars)
      (Option.to_list request.Request.variables)
    @ List.map
        (fun path -> Build_plan.Prepend_path path)
        (List.concat_map
           (Option.fold ~none:[] ~some:Commands.paths)
           [ request.Request.install_steps; request.Request.build ])
  in
  let+ plan =
    Build_plan.stage (Build_plan.base plan)
      (Build_plan.operations plan @ runtime)
  in
  {
    plan;
    secrets = request.Request.secrets;
    detected;
    packages = P.packages app resolved;
    metadata = P.metadata app resolved;
    resolution =
      `Assoc
        [
          ("format", `Int 1);
          ("provider", `String P.id);
          ("data", P.encode resolved);
        ];
  }

let log level message =
  `Assoc
    [
      ("Level", `String level);
      ("Msg", `String message);
      ("DocsPath", `String "");
    ]

let warnings prepared =
  match Build_plan.startup prepared.plan with
  | Some _ -> []
  | None -> [ Kiln_core.Error.show Kiln_core.Error.Missing_start ]

let info prepared =
  [
    ("success", `Bool true);
    ("secrets", Json.string_array (Environment.Names.elements prepared.secrets));
    ("kilnVersion", `String Version.current);
    ( "metadata",
      `Assoc
        (List.map (fun (key, value) -> (key, `String value)) prepared.metadata)
    );
    ("logs", `List (List.map (log "warn") (warnings prepared)));
    ("detectedProviders", Json.string_array prepared.detected);
    ("resolvedPackages", `Assoc (List.map Package.encode prepared.packages));
    ("resolution", prepared.resolution);
  ]

let failure error =
  [
    ("success", `Bool false);
    ("logs", `List [ log "error" (Kiln_core.Error.show error) ]);
  ]

let dockerfile prepared =
  Build_plan.dockerfile ~secrets:prepared.secrets prepared.plan

let encode prepared =
  Json.encode
    (`Assoc (("dockerfile", `String (dockerfile prepared)) :: info prepared))
