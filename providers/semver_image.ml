module Build_plan = Kiln_planning.Build_plan
module Package = Kiln_planning.Package
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

let image resolved = resolved.image

let resolve ~tool ?(accept = fun _ -> true) ~pins requirements =
  let* version, pin =
    Support.select
      ~parse:(fun pin -> parse Semver.version pin.Pins.version)
      ~compare:Semver.compare
      ~accept:(fun version _ ->
        accept version
        && List.for_all
             (fun range -> Semver.matches range version)
             requirements.ranges)
      ~none:
        (Package.no_version ~tool requirements.declarations
           (List.map (fun pin -> pin.Pins.version) pins))
      pins
  in
  let+ image = Build_plan.image pin.Pins.image in
  { version; image }

let encode resolved =
  `Assoc
    [
      ("version", `String (Semver.show resolved.version));
      ("image", `String (Build_plan.image_ref resolved.image));
    ]

let version r = Semver.show r.version

let package name requirements resolved =
  Package.resolve name requirements.declarations (version resolved)
