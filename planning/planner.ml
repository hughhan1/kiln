open Result.Syntax

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
  let* plan = P.prepare request source in
  Build_plan.stage (Build_plan.base plan)
    (Build_plan.operations plan
    @ List.map
        (fun path -> Build_plan.Prepend_path path)
        (Commands.paths request.Request.install_steps
        @ Commands.paths request.Request.build))

let failure error =
  Json.encode (`Assoc [ ("error", `String (Error.show error)) ])

let encode plan =
  Json.encode (`Assoc [ ("dockerfile", `String (Build_plan.dockerfile plan)) ])
