#!/usr/bin/env ocaml
(* Permission is granted to use and modify this program under WTFPL *)
#use "topfind";;
#require "str";;
#require "unix";;
#require "findlib";;

(* micro-stdlib *)
module Fn = Filename
module HT = Hashtbl

let (</>) = Fn.concat
open Printf
let (|>) x f = f x
let (|-) f g x = g (f x)
let tap f x = f x; x
let debug = ref false
let dtap f x = if !debug then f x; x
let dprintf fmt = if !debug then printf (fmt ^^ "\n%!") else ifprintf stdout fmt
let (//) x y = if x = "" then y else x
let iff p f x = if p x then f x else x
let flip f x y = f y x
let flat_map f xs = List.map f xs |> List.concat
let do_list xs f = List.iter f xs
let from_some = function Some x -> x | None -> invalid_arg "None"
let de_exn f x = try Some (f x) with _ -> None
let de_exn2 f x y = try Some (f x y) with _ -> None
let mkdir d = if not (Sys.file_exists d) then Unix.mkdir d 0o755
let getenv_def ?(def="") v = try Sys.getenv v with Not_found -> def

let getenv v =
  try Sys.getenv v
  with Not_found -> failwith ("undefined environment variable: " ^ v)

let starts_with s p = Str.string_match (Str.regexp ("^" ^ p)) s 0

let rec str_next str off want =
  if off >= String.length str then None
  else if String.contains want str.[off] then Some(str.[off], off)
  else str_next str (off+1) want

let slice str st en = String.sub str st (en-st) (* from offset st to en-1 *)
let split str chr = let i = String.index str chr in
(slice str 0 i, slice str (i+1) (String.length str))

let expand_tilde_slash p =
  if starts_with p "~/" then
    let home_dir = getenv "HOME" in
    Str.replace_first (Str.regexp "^~") home_dir p
  else p

let in_dir d f =
  let here = Sys.getcwd () in
  dprintf "Changing dirs from %s to %s\n%!" here (expand_tilde_slash d);
  let there = expand_tilde_slash d in
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

(* Configurable parameters, some by command line *)
let webroots = Str.split (Str.regexp "|")
    (getenv_def ~def:"http://oasis.ocamlcore.org/dev/odb/" "ODB_PACKAGE_ROOT")
(*let webroots = ["http://mutt.cse.msu.edu:8081/"] *)
let default_base = (Sys.getenv "HOME") </> ".odb"
let odb_home    = getenv_def ~def:default_base "ODB_INSTALL_DIR"
let odb_lib     = getenv_def ~def:(odb_home </> "lib") "ODB_LIB_DIR"
let odb_stubs   = getenv_def ~def:(odb_lib </> "stublibs") "ODB_STUBS_DIR"
let odb_bin     = getenv_def ~def:(odb_home </> "bin") "ODB_BIN_DIR"
let build_dir   = ref (getenv_def ~def:default_base "ODB_BUILD_DIR")
let sudo = ref (Unix.geteuid () = 0) (* true if root *)
let pkgs_to_install  = ref []
let forcing_selected = ref false
let forcing_all      = ref false
let forcing_users    = ref false
let repository       = ref "stable"
let have_perms       = ref false (* auto-detected in main *)
let ignore_unknown   = ref false
let base = ref (getenv_def "GODI_LOCALBASE" // getenv_def "OCAML_BASE")
let configure_flags = ref ""
let configure_flags_global = ref ""
type main_act = Default | Download | Info | Clean | InstallAll | Packages
let main = ref Default

(* Command-line argument handling *)
let push_install s = pkgs_to_install := s :: !pkgs_to_install
let set_ref ref v = Arg.Unit (fun () -> ref := v)

let cmd_line =  Arg.align [
  "--clean", Arg.Unit (fun () -> main := Clean), " Cleanup downloaded tarballs and install folders";
  "--sudo", Arg.Set sudo, " Switch to root for installs";
  "--have-perms", Arg.Set have_perms, " Don't use --prefix even without sudo";
  "--no-base", Arg.Unit (fun () -> base := ""), " Don't auto-detect GODI/BASE";
  "--configure-flags", Arg.Set_string configure_flags, " Flags to pass to explicitly installed packages' configure step";
  "--configure-flags-all", Arg.Set_string configure_flags_global, " Flags to pass to all packages' configure step";
  "--force"    , Arg.Set forcing_selected, " Force (re)installation of packages named";
  "--force-all", Arg.Set forcing_all, " Force (re)installation of dependencies";
  "--debug"    , Arg.Set debug, " Debug package dependencies";
  "--unstable", set_ref repository "unstable", " Use unstable repo";
  "--stable"  , set_ref repository "stable", " Use stable repo";
  "--testing" , set_ref repository "testing", " Use testing repo [default]";
  "--force-users", Arg.Set forcing_users, " Auto-reinstall dependent packages on update";
  "--ignore", Arg.Set ignore_unknown, " Don't fail on unknown package name";
  "--get"  , set_ref main Download, " Only download and extract packages; don't install";
  "--info" , set_ref main Info, " Only print the metadata for the packages listed; don't install";
  "--all"  , set_ref main InstallAll, " Install all packages from package files";
]

let parse_args () =
  Arg.parse cmd_line push_install "ocaml odb.ml [--sudo] [<packages>]";
  if !base <> "" then print_endline ("Installing to OCaml base: " ^ !base)

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

  let get_fn ?(silent=true) uri ?(fn=Fn.basename uri) () =
    dprintf "Getting URI: %s" uri;
    if Sys.command (dl_cmd ~silent uri fn) <> 0 then
      failwith ("failed to get " ^ uri);
    fn

  let get_contents uri =
    let fn = Fn.temp_file "odb" ".info" in
    ignore(get_fn uri ~fn ());
    let ic = open_in fn in
    let len = in_channel_length ic in
    let ret = String.create len in
    really_input ic ret 0 len;
    close_in ic;
    Unix.unlink fn;
    ret
end

type pid = string

(* Type of a package, with its information in a prop list *)
type pkg = { id: pid; mutable props: (string * string) list }

let prop pkg prop = try List.assoc prop pkg.props with Not_found -> ""

let pkg_id {id} = id

(* micro property-list library *)
module PL = struct
  let get_opt ~pkg ~prop:n = try Some (List.assoc n pkg.props) with Not_found -> None

  let get_b ~pkg ~prop:n =
    try List.assoc n pkg.props |> bool_of_string with
    | Not_found -> false
    | Invalid_argument "bool_of_string" ->
	failwith (sprintf "Cannot convert %s.%s=\"%s\" to bool" pkg.id n (List.assoc n pkg.props))

  let get_i ~pkg ~prop:n =
    try List.assoc n pkg.props |> int_of_string with
    | Not_found -> -1
    | Failure "int_of_string" ->
	failwith (sprintf "Cannot convert %s.%s=\"%s\" to int" pkg.id n (List.assoc n pkg.props))

  let of_string str =
    let rec parse str acc =
      try let key, rest = split str '=' in
      if rest <> "" && rest.[0] = '{' then
        try let value, rest = split rest '}' in
        let value = String.sub value 1 (String.length value - 1) in (* remove '{' *)
        parse rest ((key, value)::acc)
        with Not_found -> failwith "Unclosed { in property list"
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

  let add ~pkg k v = pkg.props <- (k,v) :: pkg.props

  let modify_assoc ~prop:n f pl =
    try let old_v = List.assoc n pl in
    (n, f old_v) :: List.remove_assoc n pl with Not_found -> pl

  let has_prop pkg k0 = List.mem_assoc k0 pkg.props

  let print p = printf "%s\n" p.id;
    do_list p.props
      (fun (k,v) ->
	if (String.contains v ' ') then printf "%s={%s}\n" k v
	else printf "%s=%s\n" k v );
    printf "\n"
end

let deps_uri    id webroot    = webroot ^ !repository ^ "/pkg/info/"   ^ id
let prefix_webroot webroot fn = webroot ^ !repository ^ "/pkg/"        ^ fn
let prefix_webroot_backup wr fn = wr    ^ !repository ^ "/pkg/backup/" ^ fn

let add_backup_dl webroot pl = (* create backup tarball location *)
  try
    let tarball = List.assoc "tarball" pl in
    let backup_addr = prefix_webroot_backup webroot tarball in
    ("tarball2", backup_addr) :: pl
  with
    Not_found -> pl

let add_install_type pl =
  if List.mem_assoc "inst_type" pl then pl
  else match de_exn2 List.assoc "is_library" pl, de_exn2 List.assoc "is_program" pl with
  | Some "true", _ -> ("inst_type", "lib")::pl
  | _, Some "true" -> ("inst_type", "app")::pl
  | _ -> pl

(* wrapper functions to get data from server *)
let info_cache = HT.create 10

let get_info id = (* gets a package's info from the repo *)
  try HT.find info_cache id
  with Not_found ->
    let rec find_uri = function
      | [] -> failwith ("Package "^id^" is not in the "^ !repository ^" repo.")
      | webroot :: tl ->
          try
	    deps_uri id webroot
	  |> Http.get_contents
          |> PL.of_string
          |> add_backup_dl webroot
          |> add_install_type (* convert is_* to inst_type *)
              (* prefix the tarball location by the server address *)
          |> PL.modify_assoc ~prop:"tarball" (prefix_webroot webroot)
          |> tap (HT.add info_cache id)
          with
	    Failure _ -> find_uri tl
    in
    find_uri webroots

(* some keywords handled in the packages file for user-defined actions
   to override the default ones *)
let usr_config_key = "config"

let parse_package_line fn linenum str =
  if chomp str = "" || str.[0] = '#' then None (* comments and blank lines *)
  else try
    (* extract user commands between braces, as in config={~/configure.sh} *)
    let id, rest = split str ' ' in
    HT.add info_cache id (PL.of_string rest);
    Some id
  with Failure s ->
    printf "W: packages file %s line %d is invalid: %s\n" fn linenum s; None
      
let parse_package_file path =
  if not (Sys.file_exists path) then []
  else
    let packages = read_lines path |> mapi (parse_package_line path) 1 |> unopt
    in dprintf "%d packages loaded from %s\n" (List.length packages) path; packages

let is_uri str =
  Str.string_match (Str.regexp "^\\(http\\|ftp\\|https\\):") str 0

let get_remote fn =
  if is_uri fn then Http.get_fn ~silent:false fn ()(* download to current dir *)
  else if Fn.is_relative fn then failwith "non-absolute filename not allowed"
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
      if actual <> (prop p hash) then
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
    if PL.has_prop p "gpg" then
      if not (detect_exe "gpg") then
        failwith ("gpg executable not found; cannot check signature for " ^ fn)
      else
        let sig_uri  = prop p "gpg" in
        let sig_file = get_remote sig_uri in
        let cmd = Printf.sprintf "gpg --verify %s %s" sig_file (idir </> fn) in
        printf "gpg command: %s\n%!" cmd;
        if Sys.command cmd == 0 then
          printf "Tarball %s passed gpg check %s\n" fn sig_file
        else hash_chk ~hash:"gpg" ~actual:"gpg check failed"
    else if PL.has_prop p "sha1" then
      if not (detect_exe "sha1sum") then
        failwith ("sha1sum executable not found; cannot check sum for " ^ fn)
      else
        let out = first_line_output ("sha1sum " ^ fn) in
        match Str.split (Str.regexp " ") out with
        | [sum; _sep; _file] -> hash_chk ~hash:"sha1" ~actual:sum
        | _ -> failwith ("unexpected output from sha1sum: " ^ out)
    else if PL.has_prop p "md5" then
      hash_chk ~actual:(Digest.file fn |> Digest.to_hex) ~hash:"md5";
    dprintf "Tarball %s has md5 hash %s" fn (Digest.file fn |> Digest.to_hex);
    fn
  in
  try attempt (prop p "tarball")
  with Failure s -> 
    printf "First attempt to download the package failed with the following error:\n" ;
    printf "==> %s\nTrying with backup location\n" s ;
    attempt (prop p "tarball2")

let make_pkg id =
  if is_uri id then (* remote URL *)
    if Fn.check_suffix id "git"                  (* "cli" means "command-line interface" *)
    then (* git clone URI  *)  {id=Fn.basename id |> Fn.chop_extension; props = ["git",id; "cli","yes"]}
    else (* remote tarball *)  {id=Fn.basename id; props = ["tarball",id; "cli","yes"]}
  else if Sys.file_exists id then
    if Sys.is_directory id
    then (* local directory *) {id=Fn.basename id; props = ["dir"   , id; "cli","yes"]}
    else (* local tarball   *) {id=Fn.basename id; props = ["tarball",id; "cli","yes"]}
  else (* remote file *)       {id;                props = get_info id}

(* Version number handling *)
module Ver = struct
  (* A version number is a list of (string or number) *)
  type ver_comp = Num of int | Str of string
  type version = ver_comp list

  let rec cmp : version -> version -> int = fun a b ->
    match a,b with
    | [],[] -> 0 (* each component was equal *)
	  (* ignore trailing .0's *)
    | Str"."::Num 0::t, [] -> cmp t [] | [], Str"."::Num 0::t -> cmp [] t
	  (* longer version numbers are before shorter ones *)
    | _::_,[] -> 1 | [], _::_ -> -1
    | (x::xt), (y::yt) when x=y -> cmp xt yt      (* compare tails when heads are equal *)
    | (Num x::_), (Num y::_) -> compare (x:int) y   (* just compare numbers *)
	  (* extend with name ordering? *)
    | (Str x::_), (Str y::_) -> compare (x:string) y
    | (Num x::_), (Str y::_) -> -1              (* a number is always before a string *)
    | (Str x::_), (Num y::_) ->  1              (* a string is always after a number *)

  let to_ver = function
    | Str.Delim s -> Str s
    | Str.Text s -> Num (int_of_string s)

  let parse_ver v =
    try Str.full_split (Str.regexp "[^0-9]+") v |> List.map to_ver
    with Failure _ -> eprintf "Could not parse version: %s" v; []

  let comp_to_string = function Str s -> s | Num n -> string_of_int n
  let to_string v = List.map comp_to_string v |> String.concat ""
end

(* Dependency comparison library *)
module Dep = struct
  open Ver
  type cmp = GE | EQ | GT (* Add more?  Add &&, ||? *)
  type vreq = (cmp * version) option
  type dep = pkg * vreq
  let comp vc = function GE -> vc >= 0 | EQ -> vc = 0 | GT -> vc > 0
  let comp_to_string = function GE -> ">=" | EQ -> "=" | GT -> ">"

  let id ({id},_) = id

  let req_to_string = function
    | None -> ""
    | Some (c,ver) -> (comp_to_string c) ^ (Ver.to_string ver)

  let version p =
    match prop p "inst_type" with
    | "lib" ->
	( try Some (Findlib.package_property [] p.id "version" |> parse_ver)
        with Findlib.No_such_package _ -> None )
    | "app" -> (* can't detect version number of programs reliably *)
	if detect_exe p.id then Some [] else None
    | "clib" ->
	( try Some (parse_ver (first_line_output ("pkg-config --modversion " ^ p.id)))
        with _ -> None )
    | "" | _ ->
	begin
          try Some (Findlib.package_property [] p.id "version" |> parse_ver)
          with Findlib.No_such_package _ ->
            if not (detect_exe p.id) then None
            else Some (parse_ver (first_line_output p.id))
	end

  let up_to_date (p,req) = (* package p is installed and satisfies version requirement req*)
    match req, version p with
    | _, None -> dprintf "Package %s not installed" p.id; false
    | None, Some _ -> dprintf "Package %s installed" p.id; true
    | Some (c,vmin), Some vp -> comp (Ver.cmp vp vmin) c
    |> dtap (printf "Package %s(%s) dep satisfied: %B\n%!" p.id (req_to_string req))

  let parse_vreq vr =
    let l = String.length vr in
    if starts_with vr ">=" then (GE, parse_ver (String.sub vr 2 (l-3)))
    else  if vr.[0] = '>'  then (GT, parse_ver (String.sub vr 1 (l-2)))
    else  if vr.[0] = '='  then (EQ, parse_ver (String.sub vr 1 (l-2)))
    else failwith ("Unknown comparator in dependency, cannot parse version requirement: " ^ vr)

  let whitespace_rx = Str.regexp "[ \t]+"

  let make_dep str =
    try
      let str = Str.global_replace whitespace_rx "" str in
      match Str.bounded_split (Str.regexp_string "(") str 2 with
      | [pid;vreq] -> make_pkg pid, Some (parse_vreq vreq)
      | _ -> make_pkg str, None
    with x -> if !ignore_unknown then {id="";props=[]}, None else raise x

  let of_string s =
    Str.split (Str.regexp ",") s |> List.map make_dep |> List.filter (fun (p,_) -> p.id <> "")

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

  let new_vreq vr_new vr_old =
    match vr_new, vr_old with
    | None, _ -> None
    | Some _, None -> Some vr_new
    | Some (c,v), Some (ci,vi) ->
	begin
	  match cmp v vi, c, ci with
	  | 1,_,EQ | -1,EQ,_ | 0,EQ,GT | 0,GT,EQ -> failwith "Inconsistent requirements"
	  | 1,_,_ | 0,GT,GE | 0,EQ,GE -> Some vr_new
	  | _,_,_ -> None
	end
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
  let is_dir fn = Sys.is_directory (dir </> fn) in
  try dir </> (Sys.readdir dir |> Array.to_list |> List.find is_dir)
  with Not_found -> dir

let make_install_dir id =
  (* Set up the directory to install into *)
  let install_dir = !build_dir </> ("install-" ^ id) in
  if Sys.file_exists install_dir then
    ignore(Sys.command ("rm -rf " ^ install_dir));
  Unix.mkdir install_dir 0o700;
  install_dir

let uninstall p =
  let as_root = PL.get_b p "install_as_root" || !sudo in
  let install_prefix =
    if as_root then "sudo " else if !have_perms || !base <> "" then "" else
    "OCAMLFIND_DESTDIR="^odb_lib^" " in
  print_endline ("Uninstalling forced library " ^ p.id);
  Sys.command (install_prefix ^ "ocamlfind remove " ^ p.id) |> ignore

let clone ~cmd act p =
  let idir = make_install_dir p.id in
  let err = Failure ("Could not " ^ act ^ " for " ^ p.id) in
  in_dir idir (fun _ -> run cmd ~err);
  find_install_dir idir

let extract_tarball p =
  let idir = make_install_dir p.id in
  let err = Failure ("Could not extract tarball for " ^ p.id) in
  in_dir idir (fun idir -> run (extract_cmd (get_tarball_chk p idir)) ~err);
  dprintf "Extracted tarball for %s into %s" p.id idir;
  find_install_dir idir

let clone_git p =
  let cmd = "git clone --depth=1 " ^ prop p "git" in
  let cmd = if prop p "branch" <> "" then cmd ^ " --branch " ^ prop p "branch" else cmd in
  clone ~cmd "clone git" p

let clone_svn   p = clone ~cmd:("svn checkout " ^ prop p "svn") "checkout svn" p
let clone_hg    p = clone ~cmd:("hg clone " ^ prop p "hg") "clone mercurial" p
let clone_darcs p = clone ~cmd:("darcs get --lazy " ^ prop p "darcs") "get darcs" p

let clone_cvs p =
  let idir = make_install_dir p.id in
  run ("cvs -z3 -d" ^ prop p "cvs" ^ " co " ^ prop p "cvspath")
    ~err:(Failure ("Could not checkout cvs for " ^ p.id));
  idir </> (prop p "cvspath") (* special dir for cvs *)

(* returns the directory that the package was extracted to *)
let get_package p =
  (* uninstall forced libraries *)
  if (Dep.version p <> None) && (prop p "inst_type" = "lib") then
    uninstall p;
  if      PL.has_prop p "tarball" then extract_tarball p
  else if PL.has_prop p   "dir"   then prop p "dir"
  else if PL.has_prop p   "git"   then clone_git    p
  else if PL.has_prop p   "svn"   then clone_svn    p
  else if PL.has_prop p   "cvs"   then clone_cvs    p
  else if PL.has_prop p   "hg"    then clone_hg     p
  else if PL.has_prop p  "darcs"  then clone_darcs  p
  else if PL.has_prop p  "deps"   then "."
  else failwith ("No download method available for: " ^ p.id)

let get_package p =
  get_package p |> tap (printf "package downloaded to %s\n%!")

type pkg_info = { pkg:pkg; mutable vreq: Dep.vreq; deps: pid list }

let info_pkg {pkg} = pkg

let make_deps pids = List.map Dep.make_dep pids

let reconcile_vreq vreq info =
  match Dep.new_vreq vreq info.vreq with
  | None -> ()
  | Some vreq -> info.vreq <- vreq

module DepGraph = struct (***************************************************************************)

  type pkg_type = Selected | Dep | User

  let exit_old_dep pid (c,v) versn =
    print_endline ("\nError: Dependency "^pid^"("^ Ver.to_string v ^") needs to be upgraded.");
    print_endline  "Use --force-all to reinstall.";
    exit 1

  let warn pid v =
    print_string
      ("Warning: Package "^pid^"("^ Ver.to_string v ^") is outdated; use --force to reinstall.")	

  let exit_bad_download p deps =
    print_endline ("Package "^p.id^" failed to download.");
    (* printf "Packages %a have not been processed yet.\n" print_strings deps... *)
    exit 1

  let rec add_pkgs graph deps ~ptype =
    let rec aux graph = function
      | [] -> graph
      | (p,vreq)::deps -> begin
	  let add_subgraph () =
	    let depstr = prop p "deps" in
	    let new_deps =
	      if depstr <> "?" then Dep.of_string depstr
	      else try in_dir (get_package p) (Dep.from_metas p.id)
	      with x -> exit_bad_download p deps
	    in let info = { pkg=p; vreq; deps = List.map Dep.id new_deps }
	    in match ptype with
	    | User -> aux graph deps
	    |  _   -> aux (add_pkgs (info::graph) new_deps ~ptype:Dep) deps
	  in try
	    reconcile_vreq vreq (List.find (fun {pkg} -> pkg.id = p.id) graph);
	    aux graph deps
	  with Not_found -> begin
	    let ver_opt = Dep.version p in
	    if ver_opt = None || !forcing_all || (ptype <> Dep && !forcing_selected)
	    then add_subgraph ()
	    else match vreq with
	    | None -> aux graph deps
	    | Some (c,v) ->
		let versn = from_some ver_opt in
		if Dep.comp (Ver.cmp versn v) c then aux graph deps
		else if ptype = Dep then exit_old_dep p.id (c,v) versn
		else (warn p.id v; aux graph deps)
	  end
      end
    in aux graph deps
      
  let from_pids ~ptype ids = add_pkgs [] (make_deps ids) ~ptype
      
  let in_deps pis {pkg} =
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
  let ids = List.map pkg_id ps |> List.filter is_installed in
  Fl_package_base.package_users [] ids
  |> List.filter (fun id -> not (List.exists (starts_with id) ids))

(* Installing a package *)
let rec install_from_current_dir ~is_dep p =
  let pid = p.id in
  dprintf "Installing %s from %s" pid (Sys.getcwd ());
  (* configure installation parameters based on command-line flags *)
  let as_root = PL.get_b p "install_as_root" || !sudo in
  let config_options =
    if as_root || !have_perms then ""
    else if !base <> "" then " --prefix " ^ !base
    else                     " --prefix " ^ odb_home in
  let config_options = config_options ^ if not is_dep then (" " ^ !configure_flags) else "" in
  let config_options = config_options ^ " " ^ !configure_flags_global in
  let install_prefix = if as_root then "sudo " else "" in
  let destdir = if not as_root && not !have_perms && !base = "" then odb_lib else ""
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
    if PL.has_prop p usr_config_key then
      (* user configure command overrides default one *)
      let config_cmd = prop p usr_config_key in
      run config_cmd ~err:(config_fail pid);
    else if Sys.file_exists "configure" then
      run ("./configure" ^ config_options) ~err:(config_fail pid);
    (* Autodetect 'gnumake', 'gmake' and 'make' *)
    let make =
      if      detect_exe "gnumake" then "gnumake"
      else if detect_exe  "gmake"  then "gmake"
      else if detect_exe   "make"  then "make"
      else failwith "No gnumake/gmake/make executable found; cannot build"
    in
    if Sys.command make <> 0 then (try_oasis_bootstrap pid; try_build_using_oasis ())
    else begin
      (* on windows, setting an environment variable to "" clears it. *)
      Unix.putenv "OCAMLFIND_DESTDIR" destdir;
      (* TODO: MAKE TEST *)
      run (install_prefix ^ make ^ " install") ~err:(install_fail pid)
    end
  in
  if      Sys.file_exists "setup.ml"  then try_build_using_oasis ()
  else if Sys.file_exists "OMakefile"
      &&  Sys.file_exists "OMakeroot" then try_build_using_omake pid
  else                                     try_build_using_make  ();
  (* test whether installation was successful *)
  if (prop p "cli" <> "yes") && not (Dep.up_to_date (p,None)) then begin
    print_endline ("Problem with installed package: " ^ pid);
    print_endline ("Installed package is not available to the system");
    print_endline ("Make sure "^odb_lib^" is in your OCAMLPATH");
    print_endline ("and "^odb_bin^" is in your PATH");
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
    install_list ["oasis"]
  end;
  run "oasis setup" ~err:(oasis_fail pid)
and
    install_packages ps =
  do_list ps (fun p -> in_dir (prop p "dir") (fun _ -> install_from_current_dir p ~is_dep:false))
and
    install_list pids =
  printf "Finding packages...%!";
  let infos = DepGraph.(sort (from_pids pids ~ptype:Selected)) |> List.flatten in
  printf "done.\nInstalling selected packages...%!";
  install_packages (List.rev infos |> List.map info_pkg);
  print_endline "done.";
  let user_pids =
    get_users (List.map info_pkg infos)
  |> (List.filter (fun s -> not (String.contains s '.')))
(*  |> StringSet.of_list *)
  in
  if !forcing_users
  then begin
    printf "Finding user packages...%!";
    print_endline "done.";
    let infos = DepGraph.(sort (from_pids user_pids ~ptype:User)) |> List.flatten in
    install_packages (List.rev infos |> List.map info_pkg);
  end
  else begin
    print_endline "Some packages depend on the just installed packages and should be re-installed.";
    print_endline "The command to do this is:";
    print_string "  ocaml odb.ml --force "; (* ?? what happened to force-users? *)
    print_strings user_pids
  end

let install_all () =
  if !forcing_all then install_list (HT.keys info_cache)
(*
  flip HT.iter info_cache (fun id p -> install_full ~is_dep:false {id=id;props=p})
 *)
  else print_string
      "No package file given, use --force-all to install all packages from system package files\n"

let list_all () =
  let pkgs = webroots
  |> String.concat " "
  |> List.map (fun wr ->
      try deps_uri "00list" wr |> Http.get_contents
      with Failure _ -> printf "%s is unavailable or not a valid repository\n\n" wr; "")
  |> Str.split (Str.regexp " +")
  in
  begin match pkgs with
  | [] -> print_endline "No packages available"
  | hd :: tl -> (* Remove duplicate entries (inefficiently) *)
      let pkgs = List.fold_left (fun accu p -> if List.mem p accu then accu else p :: accu) [hd] tl in
      print_string "Available packages from oasis: ";
      print_strings (List.rev pkgs)
  end;
  print_string "Locally configured packages:";
  HT.iter (fun k _v -> printf " %s" k) info_cache;
  print_newline ()
    
let () =
  parse_args ();
  ignore (parse_package_file (odb_home </> "packages"));
  ignore (parse_package_file (Fn.dirname (get_exe ()) </> "packages"));
  (* initialize build directory if needed *)
  if !sudo then build_dir := Fn.temp_dir_name
  else (mkdir odb_home; mkdir odb_lib; mkdir odb_bin; mkdir odb_stubs); (* ?? what about build_dir? *)
  Sys.chdir !build_dir;
  else match !main with
  | Default ->
      if !pkgs_to_install = [] then list_all ()
      else install_list (List.rev !pkgs_to_install)
  | Clean -> Sys.command ("rm -rvf install-*") |> ignore
  | Download ->
      do_list (List.rev !pkgs_to_install)
        (fun pid -> printf "Package %s downloaded to %s\n" pid (make_pkg pid |> get_package))
  | Info -> List.iter (make_pkg |- PL.print) (List.rev !pkgs_to_install)
      (* TODO: TEST FOR CAML_LD_LIBRARY_PATH=odb_lib and warn if not set *)
  | InstallAll -> (* install all packages from package files *)
      if !to_install <> []
      then begin
	let pkgs = flat_map (get_remote |- parse_package_file) !pkgs_to_install in
	print_string "Packages to install: "; print_strings pkgs;
	install_list pkgs
      end
      else if !force_all then
	Repo.entries () |> List.iter (install_full ~is_dep:false)
      else
	print_string
	  "No package file given, use --force-all to install all packages from system package files\n"

