module Error = Kiln_core.Error
open Result.Syntax

type detection = No_match | Fallback | Explicit

module type S = sig
  type application
  type resolution

  val id : string
  val public_name : string
  val exclude : string list
  val detect : Source.t -> (detection, Error.t) result
  val load : Request.t -> Source.t -> (application, Error.t) result
  val startup : application -> Build_plan.argv option
  val fingerprint : application -> string

  val resolve :
    today:string ->
    fetch:Registry.fetch ->
    application ->
    (resolution, Error.t) result

  val decode : Yojson.Basic.t -> (resolution, Error.t) result
  val encode : resolution -> Yojson.Basic.t
  val packages : application -> resolution -> Package.t list
  val metadata : application -> (string * string) list

  val legacy :
    application -> Yojson.Basic.t -> (resolution option, Error.t) result

  val plan : application -> resolution -> (Build_plan.t, Error.t) result
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
    let rec partition = function
      | [] -> ([], [])
      | ((evidence, _) as candidate) :: rest -> (
          let explicit, fallback = partition rest in
          match evidence with
          | No_match -> (explicit, fallback)
          | Explicit -> (candidate :: explicit, fallback)
          | Fallback -> (explicit, candidate :: fallback))
    in
    match partition matches with
    | [], fallback -> fallback
    | (_ :: _ as explicit), _ -> explicit
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
