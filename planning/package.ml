open Result.Syntax

type requirement = { version : string; source : string }

let optional source version =
  Option.to_list (Option.map (fun version -> { source; version }) version)

let versions requirements = List.map (fun r -> r.version) requirements

let select ~tool ~declarations ~version ~parse ~accept rows =
  let* candidates =
    Support.traverse
      (fun row ->
        let+ parsed = parse (version row) in
        (parsed, row))
      rows
  in
  List.find_opt (fun (parsed, row) -> accept parsed row) (List.rev candidates)
  |> Option.to_result
       ~none:
         (Error.No_version
            {
              tool;
              requirements = String.concat " && " (versions declarations);
              supported = List.map version rows;
            })
