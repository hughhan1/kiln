external cancelled : unit -> bool = "kiln_cancelled"
external input : unit -> string = "kiln_input"

let execute () =
  let result =
    try
      let bytes = input () in
      Kiln_io.Driver.plan ~cancelled bytes
    with exn -> Error (Kiln_planning.Error.Internal (Printexc.to_string exn))
  in
  Result.fold
    ~ok:(fun bytes -> (0, bytes))
    ~error:(fun error ->
      ( Kiln_planning.Error.native_status error,
        Kiln_planning.Planner.failure error ))
    result

let () =
  let _thread = Thread.self () in
  Callback.register "kiln.execute" execute
