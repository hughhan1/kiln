module Build_plan = Kiln_planning.Build_plan
module Package = Kiln_planning.Package
module Support = Kiln_planning.Support
open Result.Syntax
module Semver = Kiln_planning.Semver
module Error = Kiln_planning.Error

type requirements = { declarations : string list; ranges : Semver.range list }

let parse parser value =
  Result.map_error
    (fun message -> Error.Metadata ("version " ^ value, message))
    (parser value)

let requirements declarations =
  let versions = List.sort_uniq String.compare declarations in
  let+ ranges = Support.traverse (parse Semver.range) versions in
  { declarations; ranges }

let resolve ~tool ?(accept = fun _ -> true) ~pins requirements =
  let* _, pin =
    Package.select ~tool ~declarations:requirements.declarations
      ~version:(fun pin -> pin.Pins.version)
      ~parse:(parse Semver.version)
      ~accept:(fun version _ ->
        accept version
        && List.for_all
             (fun range -> Semver.matches range version)
             requirements.ranges)
      pins
  in
  Build_plan.image pin.Pins.image
