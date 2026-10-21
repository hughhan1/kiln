open Angstrom

type identifier = Numeric of int | Textual of string

type version = {
  major : int;
  minor : int;
  patch : int;
  pre : identifier list;
  build : string list;
}

type comparison = Lt | Le | Eq | Ge | Gt
type constraint_ = comparison * version
type range = constraint_ list list
type partial = Any | Major of int | Minor of int * int | Exact of version

let number =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>= fun s ->
  if String.length s > 1 && s.[0] = '0' then fail "leading zero"
  else
    match int_of_string_opt s with
    | Some n when n <= 9007199254740991 -> return n
    | Some _ | None -> fail "version component exceeds npm safe integer bound"

let identifier_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
  | _ -> false

let identifier = take_while1 identifier_char

let prerelease =
  identifier >>= fun s ->
  if String.for_all (function '0' .. '9' -> true | _ -> false) s then
    match int_of_string_opt s with
    | Some n when String.length s = 1 || s.[0] <> '0' -> return (Numeric n)
    | Some _ | None -> fail "invalid numeric prerelease"
  else return (Textual s)

let qualifiers =
  lift2
    (fun pre build -> (pre, build))
    (option [] (char '-' *> sep_by1 (char '.') prerelease))
    (option [] (char '+' *> sep_by1 (char '.') identifier))

let core major minor patch pre build = { major; minor; patch; pre; build }

let exact =
  option () (char 'v' *> return ())
  *> lift3
       (fun a b c -> (a, b, c))
       (number <* char '.')
       (number <* char '.')
       number
  >>= fun (a, b, c) ->
  qualifiers >>| fun (pre, build) -> core a b c pre build

let version text = parse_string ~consume:Consume.All exact text
let major v = v.major
let stable v = v.pre = []
let show_id = function Numeric n -> string_of_int n | Textual s -> s

let show v =
  Printf.sprintf "%d.%d.%d%s%s" v.major v.minor v.patch
    (if v.pre = [] then "" else "-" ^ String.concat "." (List.map show_id v.pre))
    (if v.build = [] then "" else "+" ^ String.concat "." v.build)

let compare_identifier a b =
  match (a, b) with
  | Numeric a, Numeric b -> Int.compare a b
  | Textual a, Textual b -> String.compare a b
  | Numeric _, Textual _ -> -1
  | Textual _, Numeric _ -> 1

let compare a b =
  let c =
    Stdlib.compare (a.major, a.minor, a.patch) (b.major, b.minor, b.patch)
  in
  if c <> 0 then c
  else
    match (a.pre, b.pre) with
    | [], [] -> 0
    | [], _ :: _ -> 1
    | _ :: _, [] -> -1
    | _ :: _, _ :: _ -> List.compare compare_identifier a.pre b.pre

let zero a b c = core a b c [] []
let ceiling a b c = core a b c [ Numeric 0 ] []

let minimum = function
  | Any -> zero 0 0 0
  | Major a -> zero a 0 0
  | Minor (a, b) -> zero a b 0
  | Exact v -> v

let partial =
  let wildcard =
    satisfy (function '*' | 'x' | 'X' -> true | _ -> false) *> return None
  in
  let component = number >>| Option.some <|> wildcard in
  option () (char 'v' *> return ()) *> sep_by1 (char '.') component
  >>= fun components ->
  match components with
  | [ None ] | [ None; _ ] | [ None; _; _ ] -> return Any
  | [ Some a ] | [ Some a; None ] | [ Some a; None; _ ] -> return (Major a)
  | [ Some a; Some b ] | [ Some a; Some b; None ] -> return (Minor (a, b))
  | [ Some a; Some b; Some c ] ->
      qualifiers >>| fun (pre, build) -> Exact (core a b c pre build)
  | [] | _ :: _ -> fail "expected up to three version components"

let interval = function
  | Any -> []
  | Exact v -> [ (Eq, v) ]
  | Major a -> [ (Ge, zero a 0 0); (Lt, ceiling (a + 1) 0 0) ]
  | Minor (a, b) -> [ (Ge, zero a b 0); (Lt, ceiling a (b + 1) 0) ]

let primitive op p =
  match (op, p) with
  | Eq, _ -> interval p
  | (Lt | Gt), Any -> [ (Lt, ceiling 0 0 0) ]
  | Lt, _ -> [ (Lt, minimum p) ]
  | Ge, _ -> [ (Ge, minimum p) ]
  | (Le | Gt), Exact v -> [ (op, v) ]
  | Le, Any -> []
  | Le, Major a -> [ (Lt, ceiling (a + 1) 0 0) ]
  | Le, Minor (a, b) -> [ (Lt, ceiling a (b + 1) 0) ]
  | Gt, Major a -> [ (Ge, zero (a + 1) 0 0) ]
  | Gt, Minor (a, b) -> [ (Ge, zero a (b + 1) 0) ]

let tilde p =
  match p with
  | Exact v -> [ (Ge, v); (Lt, ceiling v.major (v.minor + 1) 0) ]
  | Any | Major _ | Minor _ -> interval p

let caret p =
  match p with
  | Any | Major _ | Minor (0, _) -> interval p
  | Minor (a, _) -> [ (Ge, minimum p); (Lt, ceiling (a + 1) 0 0) ]
  | Exact v ->
      [
        (Ge, v);
        ( Lt,
          if v.major <> 0 then ceiling (v.major + 1) 0 0
          else if v.minor <> 0 then ceiling 0 (v.minor + 1) 0
          else ceiling 0 0 (v.patch + 1) );
      ]

let space =
  skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let space1 =
  satisfy (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false) *> space

let simple =
  choice
    [
      string ">=" *> return Ge;
      string "<=" *> return Le;
      char '>' *> return Gt;
      char '<' *> return Lt;
      char '=' *> return Eq;
    ]
  >>= (fun op -> space *> partial >>| primitive op)
  <|> ((string "~>" <|> string "~") *> space *> partial >>| tilde)
  <|> (char '^' *> space *> partial >>| caret)
  <|> (partial >>| interval)

let hyphen =
  partial >>= fun lo ->
  space1 *> char '-' *> space1 *> partial >>| fun hi ->
  (Ge, minimum lo) :: primitive Le hi

let branch = hyphen <|> (sep_by1 space1 simple >>| List.concat) <|> return []

let range text =
  parse_string ~consume:Consume.All
    (space *> sep_by1 (space *> string "||" <* space) branch <* space)
    text

let holds version (op, bound) =
  let c = compare version bound in
  match op with
  | Lt -> c < 0
  | Le -> c <= 0
  | Eq -> c = 0
  | Ge -> c >= 0
  | Gt -> c > 0

let matches ranges version =
  let allows_prerelease (_, bound) =
    bound.pre <> []
    && (bound.major, bound.minor, bound.patch)
       = (version.major, version.minor, version.patch)
  in
  let matches_branch terms =
    List.for_all (holds version) terms
    && (stable version || List.exists allows_prerelease terms)
  in
  List.exists matches_branch ranges
