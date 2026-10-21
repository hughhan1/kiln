type requirement = { version : string; source : string }
type origin = Default of string | Declared of requirement * requirement list
type t = { name : string; resolved : string; origin : origin }

let require source version = { source; version }

let optional source version =
  Option.to_list (Option.map (require source) version)

let versions requirements = List.map (fun r -> r.version) requirements

let resolve ?(default_source = "kiln default") name requirements resolved =
  let origin =
    match requirements with
    | [] -> Default default_source
    | first :: rest -> Declared (first, rest)
  in
  { name; resolved; origin }

let encode package =
  let source, requested =
    match package.origin with
    | Default source -> (source, [])
    | Declared (first, rest) ->
        let requirements = first :: rest in
        ( String.concat ", "
            (List.sort_uniq String.compare
               (List.map (fun r -> r.source) requirements)),
          [
            ( "requestedVersion",
              `String (String.concat " && " (versions requirements)) );
          ] )
  in
  ( package.name,
    `Assoc
      ([
         ("name", `String package.name);
         ("resolvedVersion", `String package.resolved);
         ("source", `String source);
       ]
      @ requested) )
