open Result.Syntax

type detection = No_match | Fallback | Explicit

type output = {
  plan : Build_plan.t;
  packages : Package.t list;
  metadata : (string * string) list;
  resolution : Yojson.Basic.t;
}

module type S = sig
  val id : string
  val public_name : string
  val exclude : string list
  val detect : Source.t -> (detection, Error.t) result
  val prepare : Request.t -> Source.t -> (output, Error.t) result
end

type t = (module S)
type selection = { provider : t; detected : string list }

let select providers ~requested source =
  let* matches =
    Support.traverse
      (fun ((module P : S) as provider) ->
        let+ evidence = P.detect source in
        (evidence, provider))
      providers
  in
  let strongest matches =
    let only evidence = List.filter (fun (e, _) -> e = evidence) matches in
    match only Explicit with [] -> only Fallback | explicit -> explicit
  in
  let detected =
    List.map (fun (_, (module P : S)) -> P.public_name) (strongest matches)
    |> List.sort_uniq String.compare
  in
  let candidates =
    match requested with
    | None -> strongest matches
    | Some name -> (
        let candidates =
          List.filter (fun (_, (module P : S)) -> P.public_name = name) matches
        in
        match strongest candidates with [] -> candidates | found -> found)
  in
  let+ provider =
    match candidates with
    | [ (_, provider) ] -> Ok provider
    | [] ->
        Error
          (Error.Unsupported
             (Option.fold
                ~none:"no provider detected; select a provider explicitly"
                ~some:(fun name -> "provider " ^ name)
                requested))
    | _ :: _ :: _ ->
        Error
          (Error.Conflict
             ("providers: "
             ^ String.concat ", "
                 (List.map (fun (_, (module P : S)) -> P.id) candidates)))
  in
  { provider; detected }
