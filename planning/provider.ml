open Result.Syntax

type detection = No_match | Fallback | Explicit

type steps = {
  inputs : string list;
  install : Build_plan.argv list;
  build : Build_plan.argv list;
  launcher : string list;
}

module type S = sig
  val id : string
  val public_name : string
  val includes : string list
  val detect : Source.t -> (detection, Error.t) result
  val prepare : Request.t -> Source.t -> (Build_plan.t, Error.t) result
  val develop : Source.t -> (steps, Error.t) result
end

type t = (module S)

let select providers source =
  let* matches =
    Support.traverse
      (fun ((module P : S) as provider) ->
        let+ evidence = P.detect source in
        (evidence, provider))
      providers
  in
  let only evidence = List.filter (fun (e, _) -> e = evidence) matches in
  let strongest =
    match only Explicit with [] -> only Fallback | explicit -> explicit
  in
  match
    List.filter
      (fun (_, (module P : S)) ->
        not
          (List.exists
             (fun (_, (module Q : S)) -> List.mem P.public_name Q.includes)
             strongest))
      strongest
  with
  | [ (_, provider) ] -> Ok provider
  | [] -> Error (Error.Unsupported "no provider detected")
  | _ :: _ :: _ as candidates ->
      Error
        (Error.Conflict
           ("providers: "
           ^ String.concat ", "
               (List.map (fun (_, (module P : S)) -> P.id) candidates)))
