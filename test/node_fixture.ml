open Contract_support

let releases =
  {|[{"version":"v20.19.0","npm":"10.0.0","lts":"Iron"},{"version":"v22.20.0","npm":"10.9.3","lts":"Jod"},{"version":"v24.0.0","npm":"11.0.0","lts":"Krypton"},{"version":"v26.1.0","npm":"12.0.0","lts":false}]|}

let schedule =
  {|{"v20":{"start":"2023-04-18","end":"2026-04-30"},"v22":{"start":"2024-04-24","end":"2027-04-30"},"v24":{"start":"2025-05-06","end":"2028-04-30"},"v26":{"start":"2026-05-05","end":"2029-04-30"}}|}

let registry =
  {|{"versions":{"9.0.0":{"engines":{"node":">=18"}},"10.1.0":{"engines":{"node":">=20"}},"11.0.0":{"deprecated":"retired"},"12.0.0":{"engines":{"node":">=30"}}}}|}

let image_index tag =
  let fields =
    Yojson.Basic.from_string registry_index |> Yojson.Basic.Util.to_assoc
  in
  Yojson.Basic.to_string
    (`Assoc
       (( "annotations",
          `Assoc [ ("org.opencontainers.image.version", `String tag) ] )
       :: fields))

let image_urls =
  List.concat_map
    (fun version ->
      List.map
        (fun suffix ->
          let tag = version ^ suffix in
          ( "https://registry-1.docker.io/v2/library/node/manifests/" ^ tag,
            image_index tag ))
        [ "-bookworm"; "-bookworm-slim" ])
    [ "22.20.0"; "24.0.0"; "24.21.0" ]

let metadata =
  [
    ("https://nodejs.org/dist/index.json", releases);
    ( "https://raw.githubusercontent.com/nodejs/Release/main/schedule.json",
      schedule );
    ("https://registry.npmjs.org/npm", registry);
    ("https://registry.npmjs.org/pnpm", registry);
    ( "https://auth.docker.io/token?service=registry.docker.io&scope=repository:library/node:pull",
      {|{"token":"fixture"}|} );
  ]
  @ image_urls

let discovery responses url token =
  check
    ("authorization for " ^ url)
    (token = if List.mem_assoc url image_urls then Some "fixture" else None);
  match List.assoc_opt url (responses @ metadata) with
  | Some bytes -> Ok bytes
  | None -> failwith ("unexpected discovery URL: " ^ url)

let fetch = discovery []
