module Build_plan = Kiln_planning.Build_plan
module Json = Kiln_planning.Json
module Package = Kiln_planning.Package
module Registry = Kiln_planning.Registry
module Support = Kiln_planning.Support
open Result.Syntax
module Semver = Kiln_core.Semver
module Error = Kiln_core.Error

type requirements = {
  declarations : Package.requirement list;
  ranges : Semver.range list;
}

type t = { version : Semver.version; image : Build_plan.image }

let parse parser value =
  Result.map_error
    (fun message -> Error.Metadata ("version " ^ value, message))
    (parser value)

let requirements declarations =
  let versions =
    List.sort_uniq String.compare (Package.versions declarations)
  in
  let+ ranges = Support.traverse (parse Semver.range) versions in
  { declarations; ranges }

let fingerprint requirements =
  Json.encode
    (Json.string_array
       (List.sort_uniq String.compare
          (Package.versions requirements.declarations)))

let image resolved = resolved.image

let resolve ~fetch ~repository ~platforms ~accept requirements =
  let version value = Result.to_option (Semver.version value) in
  let* version =
    Registry.select fetch ~repository
      ~exact:
        (List.find_map version (Package.versions requirements.declarations))
      ~parse:version ~compare:Semver.compare
      ~matches:(fun v ->
        Semver.stable v && accept v
        && List.for_all
             (fun range -> Semver.matches range v)
             requirements.ranges)
      ~unavailable:
        (Error.No_version
           {
             tool = repository;
             requirements =
               String.concat ", " (Package.versions requirements.declarations);
           })
  in
  let+ image =
    Registry.image fetch ~platforms ~repository (Semver.show version)
  in
  { version; image }

let encode resolved =
  `Assoc
    [
      ("version", `String (Semver.show resolved.version));
      ("image", `String (Build_plan.image_ref resolved.image));
    ]

let decode value =
  let* fields = Json.object_ "runtime resolution" value in
  let* version =
    Json.field "version" (Json.text (parse Semver.version)) fields
  in
  let+ image = Json.field "image" (Json.text Build_plan.image) fields in
  { version; image }

let version r = Semver.show r.version

let package name requirements resolved =
  Package.resolve name requirements.declarations (version resolved)
