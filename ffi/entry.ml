external cancelled : unit -> bool = "kiln_cancelled"
external input : unit -> string = "kiln_input"

let failure error =
  ( Kiln_core.Error.native_status error,
    Kiln_planning.Json.encode (`Assoc (Kiln_planning.Planner.failure error)) )

let execute () =
  let result =
    try
      let bytes = input () in
      Kiln_io.Driver.plan ~cancelled bytes
    with exn -> Error (Kiln_core.Error.Internal (Printexc.to_string exn))
  in
  Result.fold ~ok:(fun bytes -> (0, bytes)) ~error:failure result

let () =
  let _thread = Thread.self () in
  Callback.register "kiln.execute" execute
