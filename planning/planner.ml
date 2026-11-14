open Result.Syntax

type prepared = { plan : Build_plan.t; resolution : Yojson.Basic.t }

let develop (module P : Provider.S) source =
  let+ { Provider.install; build } = P.develop source in
  let commands steps =
    `List
      (List.map
         (fun command -> Json.string_array (Build_plan.arguments command))
         steps)
  in
  Json.encode
    (`Assoc [ ("install", commands install); ("build", commands build) ])

let prepare (module P : Provider.S) (request : Request.t) source =
  let* output = P.prepare request source in
  let+ plan =
    Build_plan.stage
      (Build_plan.base output.Provider.plan)
      (Build_plan.operations output.Provider.plan
      @ List.map
          (fun path -> Build_plan.Prepend_path path)
          (List.concat_map
             (Option.fold ~none:[] ~some:Commands.paths)
             [ request.Request.install_steps; request.Request.build ]))
  in
  {
    plan;
    resolution =
      `Assoc
        [ ("provider", `String P.id); ("data", output.Provider.resolution) ];
  }

let failure error = [ ("error", `String (Error.show error)) ]
let dockerfile prepared = Build_plan.dockerfile prepared.plan

let encode prepared =
  Json.encode (`Assoc [ ("dockerfile", `String (dockerfile prepared)) ])
