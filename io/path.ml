let rec absolute path =
  if Sys.file_exists path then Unix.realpath path
  else
    let parent = absolute (Filename.dirname path) in
    match Filename.basename path with
    | "." -> parent
    | ".." -> Filename.dirname parent
    | name -> Filename.concat parent name

let rec mkdir ~perm path =
  if not (Sys.file_exists path) then (
    mkdir ~perm (Filename.dirname path);
    try Unix.mkdir path perm with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
