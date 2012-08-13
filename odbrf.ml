#!/usr/bin/env ocaml
(* Permission is granted to use and modify this program under WTFPL *)
#use "topfind";;
#require "str";;
#require "unix";;
#require "findlib";;

(* micro-stdlib *)
module Fn = Filename

module Ht = struct
  include Hashtbl
  let keys ht = fold (fun key  _   keys ->  key :: keys) ht []
  let vals ht = fold (fun _  value vals -> value:: vals) ht []
end

let (</>) = Fn.concat
open Printf
let (|>) x f = f x
let (|-) f g x = g (f x)
let tap f x = f x; x
let (//) x y = if x = "" then y else x
let iff p f x = if p x then f x else x
let flip f x y = f y x
let flat_map f xs = List.map f xs |> List.concat
let do_list xs f = List.iter f xs
let from_some = function Some x -> x | None -> invalid_arg "None"
let de_exn f x = try Some (f x) with _ -> None
let de_exn2 f x y = try Some (f x y) with _ -> None
let mkdir d = if not (Sys.file_exists d) then Unix.mkdir d 0o755
let mkdirs dirs = List.iter mkdir dirs
let rec uniques = function []->[] | h::t -> if List.mem h t then uniques t else h::uniques t
let push xsref x = xsref := x :: !xsref
let end_sub str n = String.sub str n (String.length str - n)
let rx_whitespace = Str.regexp "[ \t]+"
let iter_map f xs = List.fold_left (fun ys x -> f x :: ys) [] xs |> List.rev

let getenv ?default v =
  try Sys.getenv v
  with Not_found -> begin
    match default with
    | Some def -> def
    | None -> failwith ("undefined environment variable: " ^ v)
  end

let starts_with s p = Str.string_match (Str.regexp ("^" ^ p)) s 0

let rec str_next str off want =
  if off >= String.length str then None
  else if String.contains want str.[off] then Some(str.[off], off)
  else str_next str (off+1) want

let slice str st en = String.sub str st (en-st) (* from offset st to en-1 *)
let split str chr = let i = String.index str chr in
(slice str 0 i, slice str (i+1) (String.length str))

type main_act = Default | Download | Info | Clean | InstallAll (* | Packages *)

module Conf = struct
(*let webroots = ["http://mutt.cse.msu.edu:8081/"] *)
  let webroots = Str.split
      (Str.regexp "|") (getenv "ODB_PACKAGE_ROOT" ~default:"http://oasis.ocamlcore.org/dev/odb/")
  let default_base =Sys.getenv "HOME" </> ".odb"
  let odb_home        = getenv "ODB_INSTALL_DIR"  ~default: default_base
  let build_dir  = ref (getenv  "ODB_BUILD_DIR"   ~default:(default_base </> "build"))
(* ?? odb doesn't create the build dir. *)
  let odb_bin         = getenv   "ODB_BIN_DIR"    ~default:(odb_home </> "bin")
  let odb_lib         = getenv   "ODB_LIB_DIR"    ~default:(odb_home </> "lib")
  let odb_stubs       = getenv  "ODB_STUBS_DIR"   ~default:(odb_lib  </> "stublibs")
  let sudo             = ref (Unix.geteuid () = 0) (* true if root *)
  let pids_to_install  = ref []
  let debug            = ref false
  let force            = ref false
  let force_all        = ref false
  let force_users      = ref false
  let repo             = ref "stable"
  let got_perms        = ref false (* auto-detected in main *)
  let ignore_unknown   = ref false
  let base             = ref (getenv "GODI_LOCALBASE" ~default:"" // getenv "OCAML_BASE" ~default:"")
  let config_flags     = ref ""
  let config_flags_global = ref ""
  let main                = ref Default
end

let dprintf fmt = if !Conf.debug then printf (fmt ^^ "\n%!") else ifprintf stdout fmt
let dtap f x = if !Conf.debug then f x; x

let expand_tilde_slash p =
  if starts_with p "~/" then
    let home_dir = getenv "HOME" in
    Str.replace_first (Str.regexp "^~") home_dir p
  else p

let in_dir d f =
  let     here = Sys.getcwd ()
  in let there = expand_tilde_slash d in
  if there = "" then invalid_arg "dir name is empty";
  dprintf "Changing dirs from %s to %s\n%!" here there;
  Sys.chdir there;
  let res = f there in
  dprintf "Returning to %s\n%!" here;
  Sys.chdir here;
  res

let todevnull ?err cmd =
  let err = match err with Some () -> "2" | None -> "" in
  if Sys.os_type = "Win32" then cmd ^ " >NUL" else cmd ^ " " ^ err ^ "> /dev/null"

let detect_exe exe = Sys.command (todevnull ("which \"" ^ exe ^ "\"")) = 0

let get_exe () = (* returns the full path and name of the current program *)
  Sys.argv.(0) |> iff Fn.is_relative (fun e -> Sys.getcwd () </> e)
|> iff (fun e -> Unix.((lstat e).st_kind = S_LNK)) Unix.readlink

let run cmd ~err = dprintf "R:%s" cmd; if Sys.command cmd <> 0 then raise err
let chomp s = let l = String.length s in if l = 0 || s.[l-1] != '\r' then s else slice s 0 (l-1)
let print_strings l = List.iter (printf "%s ") l; print_newline ()
let rec mapi f i = function [] -> [] | h::t -> let a=f i h in a::mapi f (i+1) t
let rec unopt = function []->[] | Some x::t -> x::unopt t | None::t -> unopt t

let read_lines fn =
  let ic = open_in fn and lst = ref [] in
  try while true do lst := input_line ic :: !lst done; assert false
  with End_of_file -> close_in ic; List.rev !lst

let first_line_output cmd =
  let ic = Unix.open_process_in cmd in
  try let line = input_line ic in ignore(Unix.close_process_in ic); line
  with End_of_file -> ""

(* Useful types *)
module StringSet = struct (* extend type with more operations *)
  include Set.Make(struct type t = string let compare = Pervasives.compare end)
  let of_list l = List.fold_left (fun s e -> add e s) empty l
  let print s =	iter (printf "%s ") s; print_newline ()
end

(* Autodetect 'gnumake', 'gmake' and 'make' *)
let make = lazy begin
  if detect_exe "gnumake" then "gnumake"
  else if detect_exe "gmake" then "gmake"
  else if detect_exe "make" then "make"
  else failwith "No make executable found; cannot build"
end

(* Command-line argument handling *)
let push_install s = push Conf.pids_to_install s
let set_ref ref v = Arg.Unit (fun () -> ref := v)

let cmd_line =  Arg.align [
  "--clean"  , Arg.Unit (fun () -> Conf.main := Clean), " Clean up downloaded tarballs and install folders";
  "--no-base", Arg.Unit (fun () -> Conf.base :=  ""  ), " Don't auto-detect GODI/BASE";
  "--config-flags"    , Arg.Set_string Conf.config_flags  , " Flags for configuration of specified packages";
  "--config-flags-all", Arg.Set_string Conf.config_flags_global, " Flags for configuration of all packages";
  "--sudo"            , Arg.Set        Conf.sudo          , " Switch to root for installs";
  "--got-perms"       , Arg.Set        Conf.got_perms     , " Don't use --prefix even without sudo";
  "--force"           , Arg.Set        Conf.force         , " Force (re)installation of specified packages";
  "--force-all"       , Arg.Set        Conf.force_all     , " Force (re)installation of dependencies";
  "--force-users"     , Arg.Set        Conf.force_users   , " Auto-reinstall dependent packages on update";
  "--ignore"          , Arg.Set        Conf.ignore_unknown, " Don't fail on unknown package name";
  "--debug"           , Arg.Set        Conf.debug         , " Debug package dependencies";
  "--unstable", set_ref Conf.repo "unstable", " Use unstable repo";
  "--stable"  , set_ref Conf.repo "stable"  , " Use stable repo";
  "--testing" , set_ref Conf.repo "testing" , " Use testing repo [default]";
  "--get"     , set_ref Conf.main Download  , " Only download and extract packages; don't install";
  "--info"    , set_ref Conf.main Info   , " Only print the metadata for the packages listed; don't install";
  "--all"     , set_ref Conf.main InstallAll, " Install all packages from package files";
]

let parse_args () =
  Arg.parse cmd_line push_install "ocaml odb.ml [--sudo] [<packages>]";
  if !Conf.base <> "" then print_endline ("Installing to OCaml base: " ^ !Conf.base)

(* micro-http library *)
module Http = struct
  let dl_cmd =
    if detect_exe "curl" then
      (fun ~silent uri fn ->
	let s = if silent then " -s" else "" in
	"curl -f -k -L --url " ^ uri ^ " -o " ^ fn ^ s)
    else if detect_exe "wget" then
      (fun ~silent uri fn ->
	let s = if silent then " -q" else "" in
	"wget --no-check-certificate " ^ uri ^ " -O " ^ fn ^ s)
    else (fun ~silent:_ _uri _fn ->
      failwith "neither curl nor wget was found, cannot download")

  let get_fn ?(silent=true) ?fn uri =
    let fn = match fn with Some fn -> fn | None -> Fn.basename uri
    in dprintf "Getting URI: %s" uri;
    if Sys.command (dl_cmd ~silent uri fn) <> 0 then failwith ("failed to get " ^ uri);
    fn

  let get_contents uri =
    let fn = Fn.temp_file "odb" ".info" in
    ignore(get_fn uri ~fn);
    let ic = open_in fn in
    let len = in_channel_length ic in
    let ret = String.create len in
    really_input ic ret 0 len;
    close_in ic;
    Unix.unlink fn;
    ret
end

let is_uri str = Str.string_match (Str.regexp "^\\(http\\|ftp\\|https\\):") str 0

(* micro property-list library *)
module PL = struct

  let of_string str =
    let rec parse str acc =
      try let key, rest = split str '=' in
      if rest <> "" && rest.[0] = '{' then
        try let value, rest = split rest '}' in
        let value = String.sub value 1 (String.length value - 1) in (* remove '{' *)
        parse rest ((key, value)::acc)
        with Not_found -> failwith "Unclosed '{' in property list"
      else
        try let value, rest = split rest ' ' in
        parse rest ((key, value)::acc)
        with Not_found -> (key, rest)::acc
      with Not_found -> acc
    in
    let str = str  (* will break files with # not at head of line *)
|> Str.global_replace (Str.regexp "#[^\n]*\n") ""
|> Str.global_replace (Str.regexp " *= *") "="
|> Str.global_replace (Str.regexp "[\n \t\r]+") " " in
    parse str []

  let modify_assoc ~prop f pl =
    try let old_v = List.assoc prop pl in
    (prop, f old_v) :: List.remove_assoc prop pl with Not_found -> pl

  let print props =
    do_list props
      (fun (k,v) ->
	if (String.contains v ' ') then printf "%s={%s}\n" k v
	else printf "%s=%s\n" k v );
    printf "\n"

end

type comparison = GE | EQ | GT

module Version = struct
  type component = Num of int | Str of string
  type t = component list
  type req = (comparison * t) option

  let rec cmp : t -> t -> int = fun a b ->
    match a,b with
    | [],[] -> 0                                              (* ignore trailing .0's *)
    | Str"."::Num 0::t, [] -> cmp t [] | [], Str"."::Num 0::t -> cmp [] t
                                    (* longer version numbers are before shorter ones *)
    | _::_,[] -> 1 | [], _::_ -> -1
    | (x::xt), (y::yt) when x=y -> cmp xt yt
    | (Num x::_), (Num y::_) -> compare (x:int) y
	  (* extend with name ordering? *)
    | (Str x::_), (Str y::_) -> compare (x:string) y
    | (Num x::_), (Str y::_) -> -1
    | (Str x::_), (Num y::_) ->  1

  let to_ver = function
    | Str.Delim s -> Str s
    | Str.Text s -> Num (int_of_string s)

  let parse v =
    try Str.full_split (Str.regexp "[^0-9]+") v |> List.map to_ver
    with Failure _ -> eprintf "Could not parse version: %s" v; []

  let comp_to_string = function Str s -> s | Num n -> string_of_int n
  let to_string v = List.map comp_to_string v |> String.concat ""
end

type pkg_id = string

type pkg = {
    id: pkg_id;
    mutable vreq: Version.req;
    mutable props: (string * string) list;
    mutable deps: pkg_id list;
  }

module Pkg = struct
  let id {id} = id
  let prop pkg prop = try List.assoc prop pkg.props with Not_found -> ""
  let has_prop pkg k0 = List.mem_assoc k0 pkg.props
  let add_prop ~pkg k v = pkg.props <- (k,v) :: pkg.props
  let print p = printf "%s\n" p.id; PL.print p.props
  let make ?(props=[]) ?(vreq=None) ?(deps=[]) id = {id;props;vreq;deps}
  let add_dir pkg dir = add_prop ~pkg "dir" dir

  module Prop = struct
    let opt ~pkg ~prop = try Some (List.assoc prop pkg.props) with Not_found -> None
	
    let bool ~pkg ~prop =
      try let str = List.assoc prop pkg.props
      in try bool_of_string str
      with Invalid_argument "bool_of_string" ->
	failwith ("Can't convert "^pkg.id^"."^prop^" = \""^str^"\" to bool.")
      with Not_found -> false
	  
    let int ~pkg ~prop:n =
      try List.assoc n pkg.props |> int_of_string with
      | Not_found -> -1
      | Failure "int_of_string" ->
	  failwith (sprintf "Can't convert %s.%s=\"%s\" to int" pkg.id n (List.assoc n pkg.props))
  end
end

module Repo = struct (* wrapper functions to get data from server *)

  let info_cache = Hashtbl.create 10

  let   deps_uri  id webroot      = webroot ^ !Conf.repo ^ "/pkg/info/" ^ id
  let prefix_webroot webroot   fn = webroot ^ !Conf.repo ^ "/pkg/" ^ fn
  let prefix_webroot_backup wr fn =   wr    ^ !Conf.repo ^ "/pkg/backup/" ^ fn

  let get_pkg_list wr =
    try deps_uri "00list" wr |> Http.get_contents
    with Failure _ -> printf "%s is unavailable or not a repository\n\n" wr; ""

  let add_backup_dl webroot pl = (* create backup tarball location *)
    try let tarball = List.assoc "tarball" pl in
    let backup_addr = prefix_webroot_backup webroot tarball in
    ("tarball2", backup_addr) :: pl
    with Not_found -> pl

  let add_install_type pl =
    if List.mem_assoc "inst_type" pl then pl
    else match de_exn2 List.assoc "is_library" pl, de_exn2 List.assoc "is_program" pl with
    | Some "true", _ -> ("inst_type", "lib")::pl
    | _, Some "true" -> ("inst_type", "app")::pl
    | _ -> pl

  let get_info id = (* gets a package from the repo *)
    try Ht.find info_cache id
    with Not_found ->
      let rec find_uri = function
	| [] -> failwith ("Package "^id^" is not in the "^ !Conf.repo ^" repo.")
        | webroot :: tl ->
            try deps_uri id webroot |> Http.get_contents
          |> PL.of_string
          |> add_backup_dl webroot
          |> add_install_type (* convert is_* to inst_type *)
              (* prefix the tarball location by the server address *)
          |> PL.modify_assoc ~prop:"tarball" (prefix_webroot webroot)
	  |> (fun props -> Pkg.make id ~props)
          |> tap (Hashtbl.add info_cache id)
            with Failure _ -> find_uri tl
      in
      find_uri Conf.webroots

  let encache pkg = Ht.add info_cache pkg.id pkg
  let encache_list l = List.iter encache l
  let entries () = Ht.vals info_cache

end

let needs_new_vreq pkg vr_new =
  match pkg.vreq, vr_new with
  |  _  , None -> false
  | None, Some _ -> true
  | Some (ci,vi), Some (c,v) ->
      begin
	match Version.cmp v vi, c, ci with
	| 1,_,EQ | -1,EQ,_ | 0,EQ,GT | 0,GT,EQ -> failwith "Inconsistent requirements"
	| 1,_,_ | 0,GT,GE | 0,EQ,GE -> true
	| _,_,_ -> false
      end
	
let reconcile_vreq vreq pkg = if needs_new_vreq pkg vreq then pkg.vreq <- vreq

let make_pkg ?(vreq=None) id =
  dprintf "\nMaking pkg %s..." id;
  if is_uri id then (* remote URL *)
    if Fn.check_suffix id "git"                 (* "cli" means "command-line interface" *)
    then (* git clone URI  *)
      let id = Fn.basename id |> Fn.chop_extension
      in                          Pkg.make        id        ~props: [  "git"  ,id; "cli","yes"]
    else (* remote tarball *)     Pkg.make (Fn.basename id) ~props: ["tarball",id; "cli","yes"]
  else if Sys.file_exists id then
    if Sys.is_directory id
    then (* local directory *)    Pkg.make (Fn.basename id) ~props: ["dir"   , id; "cli","yes"]
    else (* local tarball   *)    Pkg.make (Fn.basename id) ~props: ["tarball",id; "cli","yes"]
  else
    let pkg = (* remote file *) Repo.get_info id
    in reconcile_vreq vreq pkg; pkg

(* some keywords handled in the packages file for user-defined actions
   to override the default ones *)
let usr_config_key = "config"

let parse_pkg_line fn linenum str =
  if chomp str = "" || str.[0] = '#' then None (* comments and blank lines *)
  else try
    (* extract user commands between braces, as in config={~/configure.sh} *)
    let id, rest = split str ' ' in
    Ht.add Repo.info_cache id (Pkg.make id ~props:(PL.of_string rest));
(* FIXME: abstract info_cache *)
    Some id
  with Failure s ->
    printf "W: packages file %s line %d is invalid: %s\n" fn linenum s; None
      
let parse_pkg_file path =
  if not (Sys.file_exists path) then []
  else
    let pkgs = read_lines path |> mapi (parse_pkg_line path) 1 |> unopt
    in dprintf "%d packages loaded from %s\n" (List.length pkgs) path; pkgs

let get_remote fn =
  if is_uri fn then Http.get_fn ~silent:false fn (* download to current dir *)
  else if Fn.is_relative fn then failwith "Only absolute filenames are allowed."
  else (dprintf "Local File %s" fn; fn)

(* [get_tarball_chk p idir] fetches the tarball associated to the package
   described by property list [pl] of the package [p] in directory [idir].
   The format of the tarball is checked, as well as its integrity
   if some hash information is provided in [pl].
 *)
let get_tarball_chk p idir =
  let attempt url =
    let fn = get_remote url in
    let hash_chk ~actual ~hash =
      if actual <> (Pkg.prop p hash) then
        failwith (sprintf "Tarball %s failed %s verification, aborting\n" fn hash)
      else printf "Tarball %s passed %s check\n" fn hash
    in
    (* checks that the downloaded file is a known archive type *)
    let known_types = ["application/x-gzip" ; "application/zip" ; "application/x-bzip2"] in
    let output = first_line_output ("file -b --mime " ^ fn) in
    ( match Str.split (Str.regexp ";") output with
    | mime::_ when List.mem mime known_types -> ()
    | _ -> failwith ("The format of the downloaded archive is not handled: " ^ output) );
    (* Check the package signature or hash *)
    if Pkg.has_prop p "gpg" then
      if not (detect_exe "gpg") then
        failwith ("gpg executable not found; cannot check signature for " ^ fn)
      else
        let sig_uri  = Pkg.prop p "gpg" in
        let sig_file = get_remote sig_uri in
        let cmd = Printf.sprintf "gpg --verify %s %s" sig_file (idir </> fn) in
        printf "gpg command: %s\n%!" cmd;
        if Sys.command cmd == 0 then
          printf "Tarball %s passed gpg check %s\n" fn sig_file
        else hash_chk ~hash:"gpg" ~actual:"gpg check failed"
    else if Pkg.has_prop p "sha1" then
      if not (detect_exe "sha1sum") then
        failwith ("sha1sum executable not found; cannot check sum for " ^ fn)
      else
        let out = first_line_output ("sha1sum " ^ fn) in
        match Str.split (Str.regexp " ") out with
        | [sum; _sep; _file] -> hash_chk ~hash:"sha1" ~actual:sum
        | _ -> failwith ("unexpected output from sha1sum: " ^ out)
    else if Pkg.has_prop p "md5" then
      hash_chk ~actual:(Digest.file fn |> Digest.to_hex) ~hash:"md5";
    dprintf "Tarball %s has md5 hash %s" fn (Digest.file fn |> Digest.to_hex);
    fn
  in
  try attempt (Pkg.prop p "tarball")
  with Failure s -> 
    printf "First attempt to download the package failed with the following error:\n" ;
    printf "==> %s\nTrying with backup location\n" s ;
    attempt (Pkg.prop p "tarball2")

module Dep = struct         (* Dependency comparisons *)
  module V = Version
  type t = pkg * V.req
  let comp vc = function GE -> vc >= 0 | EQ -> vc = 0 | GT -> vc > 0
  let comp_to_string = function GE -> ">=" | EQ -> "=" | GT -> ">"

  let req_to_string = function
    | None -> ""
    | Some (c,ver) -> (comp_to_string c) ^ (Version.to_string ver)

  let version p =
    match Pkg.prop p "inst_type" with
    | "lib" ->
	( try Some (Findlib.package_property [] p.id "version" |> V.parse)
        with Findlib.No_such_package _ -> None )
    | "app" -> (* can't detect version number of programs reliably *)
	if detect_exe p.id then Some [] else None
    | "clib" ->
	( try Some (V.parse (first_line_output ("pkg-config --modversion " ^ p.id)))
        with _ -> None )
    | "" | _ ->
	begin
          try Some (Findlib.package_property [] p.id "version" |> V.parse)
          with Findlib.No_such_package _ ->
            if not (detect_exe p.id) then None
            else Some (V.parse (first_line_output p.id))
	end

  let up_to_date (pkg,req) = (* pkg is installed and satisfies version requirement req*)
    match req, version pkg with
    | _, None -> dprintf "Package %s not installed" pkg.id; false
    | None, Some _ -> dprintf "Package %s installed" pkg.id; true
    | Some (c,vmin), Some vp -> comp (Version.cmp vp vmin) c
    |> dtap (printf "Package %s(%s) dep satisfied: %B\n%!" pkg.id (req_to_string req))

  let parse_vreq_rpar vr =
    let l = String.length vr in
    if vr.[l-1] <> ')' then failwith ("No ')' in vreq+rpar: \""^vr^"\"")
    else
      let vr = String.sub vr 0 (l-1) in
      if starts_with vr ">=" then (GE, V.parse (end_sub vr 2))
      else  if vr.[0] = '>'  then (GT, V.parse (end_sub vr 1))
      else  if vr.[0] = '='  then (EQ, V.parse (end_sub vr 1))
      else failwith ("Unknown comparator in dependency, cannot parse version requirement: " ^ vr)

  let make_vreq_pkg str =
    try
      let str = Str.global_replace rx_whitespace "" str
      in match Str.bounded_split (Str.regexp_string "(") str 2 with
      | [id; vstr] -> make_pkg id ~vreq:(Some (parse_vreq_rpar vstr))
      | _ -> make_pkg str
    with x -> if !Conf.ignore_unknown then Pkg.make "" else raise x

  let of_string s =
    Str.split (Str.regexp ",") s |> List.map make_vreq_pkg |> List.filter (fun p -> p.id <> "")

  let of_oasis dir = (* reads the [dir]/_oasis file *)
    let fn = dir </> "_oasis" in
    if not (Sys.file_exists fn) then [] else
    let lines = read_lines fn in
    flip flat_map lines
      (fun line ->
	match Str.bounded_split (Str.regexp_string ":") line 2 with
	| [("BuildDepends"|"    BuildDepends"); ds] -> (* FIXME, fragile *)
            [of_string ds]
	| _ -> [])

      (* We are being very conservative here - just match all the requires, strip everything after dot *)
      (* grepping META files is not the preferred way of getting dependencies *)
  let meta_rx = Str.regexp "META\\(\\.\\(.*\\)\\)?"
  let is_meta path = Str.string_match meta_rx (Fn.basename path) 0

  let annotate_meta path =      (* add package name from directory or suffix *)
    let dir = Fn.dirname path in
    let fn = Fn.basename path in
    let pid =
      try if Str.string_match meta_rx fn 0 then Str.matched_group 2 fn else dir
      with Not_found -> dir
    in pid,path

  let rec find_metas dir =
    let paths = Sys.readdir dir |> Array.to_list |> List.map ((</>) dir) in
    let is_dir path =
      try Sys.is_directory path with Sys_error _ -> false (* ignore sym-links *) in
    let dirs,files = List.partition is_dir paths in
    let metas = List.filter is_meta files in
    List.map annotate_meta metas @ flat_map find_metas dirs

  let requires_rx = Str.regexp "requires\\(([^)]*)\\)?[ \t]*\\+?=[ \t]*\"\\([^\"]*\\)\""

  let reqs (pid,meta_path) =
    let lines = read_lines meta_path in
    flip flat_map lines
      (fun line ->
        if Str.string_match requires_rx line 0 then
          try Str.split (Str.regexp "[ ,]") (Str.matched_group 2 line)
          with Not_found -> []    (* empty string *)
        else [])
  |> List.map (fun str -> Str.split (Str.regexp "\\.") str |> List.hd)  (* delete subpackages *)
  |> List.filter ((<>) pid)
  |> List.filter (fun p -> not (Str.string_match (Str.regexp "__") p 0))   (* autoconf vars *)

  let from_metas pid dir =
    flat_map reqs (find_metas dir) |> List.filter ((<>) pid) |> flat_map of_string
end

let extract_cmd fn =
  let suff = Fn.check_suffix fn in
  if      suff ".tar.gz"  || suff ".tgz" then "tar -zxf " ^ fn
  else if suff ".tar.bz2" || suff ".tbz" then "tar -jxf " ^ fn
  else if suff ".tar.xz"  || suff ".txz" then "tar -Jxf " ^ fn
  else if suff ".zip"                    then  "unzip "   ^ fn
  else failwith ("Don't know how to extract " ^ fn)

(* detect directory created by tarball extraction or git clone *)
let find_install_dir dir =
  let is_dir fn = Sys.is_directory (dir </> fn)
  in try dir </> (Sys.readdir dir |> Array.to_list |> List.find is_dir)
  with Not_found -> dir

let make_install_dir id =
  (* Set up the directory to install into *)
  let install_dir = !Conf.build_dir </> id in
  if Sys.file_exists install_dir then
    ignore(Sys.command ("rm -rf " ^ install_dir));
  Unix.mkdir install_dir 0o700;
  install_dir

let uninstall p =
  let as_root = Pkg.Prop.bool p "install_as_root" || !Conf.sudo
  in let install_prefix =
    if as_root then "sudo " else if !Conf.got_perms || !Conf.base <> "" then "" else
    "OCAMLFIND_DESTDIR=" ^ Conf.odb_lib ^ " "
  in print_endline ("Uninstalling forced library " ^ p.id);
  Sys.command (install_prefix ^ "ocamlfind remove " ^ p.id) |> ignore

let clone ~cmd act p =
  let idir = make_install_dir p.id in
  let err = Failure ("Could not " ^ act ^ " for " ^ p.id) in
  in_dir idir (fun _ -> run cmd ~err);
  find_install_dir idir

let extract_tarball p =
  dprintf "Extracting tarball %s" p.id;
  let idir = make_install_dir p.id in
  let err = Failure ("Could not extract tarball for " ^ p.id) in
  in_dir idir (fun idir -> run (extract_cmd (get_tarball_chk p idir)) ~err);
  dprintf "Extracted tarball for %s into %s" p.id idir;
  find_install_dir idir

let clone_git p =
  let cmd = "git clone --depth=1 " ^ Pkg.prop p "git" in
  let cmd = if Pkg.prop p "branch" <> "" then cmd ^ " --branch " ^ Pkg.prop p "branch" else cmd in
  clone ~cmd "clone git" p

let clone_svn   p = clone ~cmd:("svn checkout " ^ Pkg.prop p "svn") "checkout svn" p
let clone_hg    p = clone ~cmd:("hg clone " ^ Pkg.prop p "hg") "clone mercurial" p
let clone_darcs p = clone ~cmd:("darcs get --lazy " ^ Pkg.prop p "darcs") "get darcs" p

let clone_cvs p =
  let idir = make_install_dir p.id in
  run ("cvs -z3 -d" ^ Pkg.prop p "cvs" ^ " co " ^ Pkg.prop p "cvspath")
    ~err:(Failure ("Could not checkout cvs for " ^ p.id));
  idir </> (Pkg.prop p "cvspath") (* special dir for cvs *)

let extract_pkg p =         (* returns the directory that the package was extracted to *)
  if (Dep.version p <> None) && (Pkg.prop p "inst_type" = "lib") then
    uninstall p;
  if      Pkg.has_prop p "tarball" then extract_tarball p |> tap (Pkg.add_dir p)
  else if Pkg.has_prop p   "dir"   then Pkg.prop p "dir"
  else if Pkg.has_prop p   "git"   then clone_git    p |> tap (Pkg.add_dir p)
  else if Pkg.has_prop p   "svn"   then clone_svn    p |> tap (Pkg.add_dir p)
  else if Pkg.has_prop p   "cvs"   then clone_cvs    p |> tap (Pkg.add_dir p)
  else if Pkg.has_prop p   "hg"    then clone_hg     p |> tap (Pkg.add_dir p)
  else if Pkg.has_prop p  "darcs"  then clone_darcs  p |> tap (Pkg.add_dir p)
  else if Pkg.has_prop p  "deps"   then   "."          |> tap (Pkg.add_dir p)
  else failwith ("No download method available for: " ^ p.id)

let extract_pkg p =
  extract_pkg p |> tap (printf "package downloaded to %s\n%!")

module DepGraph = struct

  type pkg_type = Selected | Dep | User

  let exit_old_dep pid (c,v) versn =
    print_endline ("\nError: Dependency "^pid^"("^ Version.to_string v ^") needs to be upgraded.");
    print_endline  "Use --force-all to reinstall.";
    exit 1

  let warn pid v =
    print_string
      ("Warning: Package "^pid^"("^ Version.to_string v ^") is outdated; use --force to reinstall.")	

  let exit_bad_download p deps =
    print_endline ("Package "^p.id^" failed to download.");
    (* printf "Packages %a have not been processed yet.\n" print_strings deps... *)
    exit 1

  let rec add_pkgs graph pkgs ~ptype =
    let rec aux graph = function
      | [] -> graph
      | p::pkgs -> begin
	  let add_subgraph () =
	    let depstr = Pkg.prop p "deps"
	    in let dir = extract_pkg p
	    in let new_pkgs =
	      if depstr <> "?" then Dep.of_string depstr
	      else try in_dir dir (Dep.from_metas p.id)
	      with x -> exit_bad_download p pkgs
	    in
	    p.deps <- List.map Pkg.id new_pkgs;
	    match ptype with
	    | User -> aux graph pkgs
	    |  _   -> aux (add_pkgs (p::graph) new_pkgs ~ptype:Dep) pkgs
	  in try
	    reconcile_vreq p.vreq (List.find (fun pkg -> pkg.id = p.id) graph);
(*
 * TODO: ??
 * reconcile_vreq assumes that p is a new pkg with the same id as another one
 * how about a local_cache to prevent that?
 *)
	    aux graph pkgs
	  with Not_found -> begin
	    let ver_opt = Dep.version p in
	    if ver_opt = None || !Conf.force_all || (ptype <> Dep && !Conf.force)
	    then add_subgraph ()
	    else match p.vreq with
	    | None -> aux graph pkgs
	    | Some (c,v) ->
		let versn = from_some ver_opt in
		if Dep.comp (Version.cmp versn v) c then aux graph pkgs
		else if ptype = Dep then exit_old_dep p.id (c,v) versn
		else (warn p.id v; aux graph pkgs)
	  end
      end
    in aux graph (pkgs: pkg list)
      
  let from_pkgs ~ptype pkgs = add_pkgs [] pkgs ~ptype
      
  let in_deps pis pkg =
    let has_dep {deps} = List.exists ((=) pkg.id) deps
    in List.exists has_dep pis

  let rec sort pis =                              (* topological sort *)
    match List.partition (in_deps pis) pis with   (* slow but simple  *)
    | [],_ -> [pis]
    | _,[] -> failwith "Found circular dependencies."
    | dep_pis, main_pis ->
	main_pis :: sort dep_pis

end


(*** INSTALL ***)

(* exceptions for installation errors *)
let   oasis_fail pid = Failure ("Could not bootstrap from _oasis " ^ pid)
let  config_fail pid = Failure ("Could not configure " ^ pid)
let   build_fail pid = Failure ("Could not build "     ^ pid)
(* let test_fail pid = Failure ("Tests for package "   ^ pid ^ "did not complete successfully")*)
let install_fail pid = Failure ("Could not install package " ^ pid)

let is_installed id =
  try ignore (Fl_package_base.query id); true
  with Fl_package_base.No_such_package (_,_) -> false

let get_users ps =
  let ids = List.map Pkg.id ps |> List.filter is_installed
  in Fl_package_base.package_users [] ids
  |> List.filter (fun id -> not (List.exists (starts_with id) ids))

(* Installing a package *)
let rec install_from_cwd ~is_dep p =
  let pid = p.id
  in dprintf "Installing %s from %s" pid (Sys.getcwd ());
  (* configure installation parameters based on command-line flags *)
  let as_root = Pkg.Prop.bool p "install_as_root" || !Conf.sudo
  in let config_options =
    if as_root || !Conf.got_perms then ""
    else if !Conf.base <> "" then " --prefix " ^ !Conf.base
    else                     " --prefix " ^ Conf.odb_home
  in let config_options = config_options ^ if not is_dep then (" " ^ !Conf.config_flags) else ""
  in let config_options = config_options ^ " " ^ !Conf.config_flags_global
  in let install_prefix = if as_root then "sudo " else ""
  in let destdir = if not as_root && not !Conf.got_perms && !Conf.base = "" then Conf.odb_lib else ""
  in
  let try_build_using_omake pid =
    dprintf "Now installing with OMake";
    if not (detect_exe "omake") then failwith "OMake executable not found; cannot build";
    run "omake" ~err:(build_fail pid);
    (* TODO: MAKE TEST *)
    run (install_prefix ^ "omake install") ~err:(install_fail pid)
  in
  let try_build_using_oasis () =
    dprintf "Now installing with Oasis";
    run ("ocaml setup.ml -configure" ^ config_options) ~err:(config_fail pid);
    run  "ocaml setup.ml -build"                       ~err:(build_fail pid);
    Unix.putenv "OCAMLFIND_DESTDIR" destdir;
    (*          run "ocaml setup.ml -test" ~err:test_fail;*)
    run (install_prefix ^ "ocaml setup.ml -install") ~err:(install_fail pid)
  in
  let try_build_using_make () =
    dprintf "Now installing with Make";
    if Pkg.has_prop p usr_config_key then
      (* user configure command overrides default one *)
      let config_cmd = Pkg.prop p usr_config_key in
      run config_cmd ~err:(config_fail pid);
    else if Sys.file_exists "configure" then
      run ("./configure" ^ config_options) ~err:(config_fail pid);
    if Sys.command (Lazy.force make) <> 0 then (try_oasis_bootstrap pid; try_build_using_oasis ())
    else begin
      (* on windows, setting an environment variable to "" clears it. *)
      Unix.putenv "OCAMLFIND_DESTDIR" destdir;
      (* TODO: MAKE TEST *)
      run (install_prefix ^ Lazy.force make ^ " install") ~err:(install_fail pid)
    end
  in
  if      Sys.file_exists "setup.ml"  then try_build_using_oasis ()
  else if Sys.file_exists "OMakefile"
      &&  Sys.file_exists "OMakeroot" then try_build_using_omake pid
  else                                     try_build_using_make  ();
  (* test whether installation was successful *)
  if (Pkg.prop p "cli" <> "yes") && not (Dep.up_to_date (p,None)) then begin
    print_endline ("Problem with installed package: " ^ pid);
    print_endline ("Installed package is not available to the system");
    print_endline ("Make sure " ^ Conf.odb_lib ^ " is in your OCAMLPATH");
    print_endline ("and " ^ Conf.odb_bin ^ " is in your PATH");
    exit 1;
  end;
  print_endline ("Successfully installed " ^ pid)
and
    try_oasis_bootstrap pid =
  dprintf "Now installing Oasis";
  if not (Sys.file_exists "_oasis")
  then failwith "_oasis file not found in package, cannot bootstrap.";
  if not (detect_exe "oasis") then begin
    printf "This package (%s) most likely needs oasis on the path.\n" pid;
    print_endline "In general tarballs shouldn't require oasis.";
    print_endline "Trying to get oasis.";
    install_pids ["oasis"]
  end;
  run "oasis setup" ~err:(oasis_fail pid)
and
    do_installs ps = List.iter do_install ps
and do_install  p  =
  let dir = Pkg.prop p "dir"
  in Pkg.print p;
  in_dir dir (fun _ -> install_from_cwd p ~is_dep:false)
and
    find_pkgs pids =
  dprintf "Finding packages...%!";
  let pkgs = iter_map make_pkg pids in dprintf "done.\n"; pkgs
and
    install_pids pids = install_pkgs (find_pkgs pids)
and
    install_pkgs pkgs =
  let graph = DepGraph.(sort (from_pkgs pkgs ~ptype:Selected)) |> List.flatten
  in
  printf "done.\nInstalling selected packages...%!";
  do_installs (List.rev graph);
  print_endline "done.";
  let user_pids = get_users graph
  |> (List.filter (fun s -> not (String.contains s '.')))
  |> uniques
  in
  if !Conf.force_users
  then begin
    let    users = find_pkgs user_pids
    in let graph = DepGraph.(sort (from_pkgs users ~ptype:User)) |> List.flatten in
    do_installs (List.rev graph);
  end
  else begin
    print_endline "Some packages depend on the just installed packages and should be re-installed.";
    print_endline "The command to do this is:";
    print_string "  ocaml odb.ml --force "; (* ?? what happened to force-users? *)
    print_strings user_pids
  end

let ht_keys ht = Ht.fold (fun key _ xs -> key::xs) ht []

(* FIXME:  use info_cache prop lists *)

let install_all () =
(* FIXME: abstract info_cache *)
  if !Conf.force_all then install_pkgs (Ht.vals Repo.info_cache) (* ?? was keys *)
(*
  flip Ht.iter info_cache (fun id p -> install_full ~is_dep:false {id=id;props=p})
 *)
  else print_string
      "No package file given, use --force-all to install all packages from system package files\n"

let list_all () =
  let pkgs =
    List.map Repo.get_pkg_list Conf.webroots |> String.concat " "
  |> Str.split (Str.regexp " +") |> uniques
  in
  if pkgs = [] then print_endline "No packages available."
  else begin
    print_endline "Packages available from oasis:";
    print_strings (List.rev pkgs)
  end;
  print_string "Locally configured packages:";
  List.iter Pkg.print (Repo.entries ());
  print_newline ()

let main () =
  parse_args ();
  ignore (parse_pkg_file (Conf.odb_home </> "packages"));
  ignore (parse_pkg_file (Fn.dirname (get_exe ()) </> "packages"));
  if !Conf.sudo then Conf.build_dir := Fn.temp_dir_name
  else mkdirs Conf.([odb_home; odb_lib; odb_bin; odb_stubs]);
  Sys.chdir !Conf.build_dir;
  match !Conf.main with
  | Default ->
      if !Conf.pids_to_install = [] then list_all ()
      else install_pids (List.rev !Conf.pids_to_install)
  | Clean -> Sys.command ("rm -rvf install-*") |> ignore
  | Download ->
      do_list (List.rev !Conf.pids_to_install)
	(fun pid -> printf "Package %s downloaded to %s\n" pid (make_pkg pid |> extract_pkg))
  | Info -> List.iter (make_pkg |- Pkg.print) (List.rev !Conf.pids_to_install)
	(* TODO: TEST FOR CAML_LD_LIBRARY_PATH=odb_lib and warn if not set *)
  | InstallAll -> (* install all packages from package files *)
      if !Conf.pids_to_install <> []
      then begin
	let pkgs = flat_map (get_remote |- parse_pkg_file) !Conf.pids_to_install in
	print_string "Packages to install: "; print_strings pkgs;
	install_pids pkgs
      end
(*    
      else if !Conf.force_all then install_list (Repo.entries ())
      else
      print_endline
      "No package file given.\nUse --force-all to install all packages from system package files."

 *)
let () = main ()
