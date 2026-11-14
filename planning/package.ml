open Result.Syntax

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
              requirements = String.concat " && " declarations;
              supported = List.map version rows;
            })
