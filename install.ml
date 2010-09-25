(*
 *  Haxe installer
 *  Copyright (c)2005 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)



(* USAGE:  try ocaml install.ml --help *)



#load "unix.cma"
#load "str.cma"
open Arg
open Str

(* ------------ tools ----------- *)
(* actions *)
type config = {
  zlib_headers:string; (* header dir *)
  zlib_lib:string; (* dir containing library *)
  obj_ext:string;
  ocamloptflags:string;
  exe_ext:string;
}
type action = { name:string; action: config -> unit; description: string; default: bool};;


type 'a option_with_deferred_default = Option of 'a | DeferredOption of (unit -> 'a);;
let eval_option x = match x with
	| Option x -> x
	| DeferredOption x -> x()

(* ----- BEGIN CONFIGURATION ---- *)


let zlib_locaction = ref None
let deps_dir = "./ocaml" (* use this var instead of hardcoded values everywhere? *)
let motiontwin = ":pserver:anonymous@cvs.motion-twin.com:/cvsroot"

(* ----- BEGIN OPTIONS ---- *)

let actions_by_user = ref []
let bytecode_default = false
let native_default = true

let bytecode = ref bytecode_default
let native = ref native_default

let os_type = Sys.os_type

let zlib_option = ref (DeferredOption 
	(fun () ->match os_type with
		| "Win32" -> ("../ocaml/extc/zlib/zlib.lib", "../ocaml/extc/zlib/zlib.lib")  (* TODO *)
		| _ ->
			try
                                let l = List.find Sys.file_exists ["/usr/lib/libz.dylib";"/usr/lib64/libz.so.1";"/usr/lib/libz.so.1";"/lib/libz.so.1";"/usr/lib/libz.so.4.1"] in
                                (Filename.dirname (Filename.dirname l) ^ "/include", l)
			with
				Not_found ->
					failwith "LibZ was not found on your system, please install it or modify the search directories in the install script"))





let bool_of_string s = match String.lowercase s with
  |"true" -> true
  |"false" -> false
  |s -> failwith ("can't parse bool" ^ s)

let bool_to_string b = match b with | true -> "true" | false -> "false"

let arg_spec = [
	("--bytecode", String (fun f -> bytecode := bool_of_string f),
		"compile to bytpecode (default: "^ bool_to_string bytecode_default ^")");
	("--native", String (fun f -> native := bool_of_string f),
		"compile to executable (default: "^ bool_to_string native_default ^") ");
	("--with-zlib", String (fun f -> zlib_option := Option ((Filename.dirname (Filename.dirname f))  ^ "/include", f ^ "/lib" ) ), "zlib (.so) location");
	("--actions", String (fun f -> actions_by_user := !actions_by_user @ Str.split (regexp ",") f) , "actions");
]

let argfiles = ref [];;

(* -- utilities -- *)


let indir dir act =
  let curr = Sys.getcwd() in
  Sys.chdir(dir);
  act();
  Sys.chdir(curr)

let msg m =
	prerr_endline m;
	flush stdout

let command c =
	msg ("> " ^ c);
        if Sys.command c <> 0 then failwith ("Error while running " ^ c);;

let cvs root cmd =
	command ("cvs -z3 -d" ^ root ^ " " ^ cmd)

let ocamlc file cfg =
	if !bytecode then command ("ocamlc -c " ^ file);
	if !native then command ("ocamlopt -c " ^ cfg.ocamloptflags ^ file)

let modules l ext =
	String.concat " " (List.map (fun f -> f ^ ext) l)

;;

(* data for compiling HaXe *)

let libs = [
        "ocaml/extLib";
        "ocaml/extc/extc";
        "ocaml/swflib/swflib";
        "ocaml/xml-light/xml-light";
        "unix";
        "str"
] @ (match !pa_deriving_lib with | None -> [] | Some f -> [f])
let neko = "neko/libs/include/ocaml"
let paths = [
        "ocaml";
        "ocaml/swflib";
        "ocaml/xml-light";
        "ocaml/extc";
        neko
]
let mlist = [
        "ast";"lexer";"type";"common";"parser";"typecore";
        "genxml";"typeload";"codegen";"optimizer";
        neko^"/nast";neko^"/binast";neko^"/nxml";
        "genneko";"genas3";"genjs";"genswf8";"genswf9";"genswf";"genphp";"gencpp";
        "interp";"typer";"main";
]
let path_str = String.concat " " (List.map (fun s -> "-I " ^ s) paths)
let libs_str ext = " " ^ String.concat " " (List.map (fun l -> l ^ ext) libs) ^ " "


(* -- ACTIONS IMPLEMENTATION -- *)

let action_fetch_deps(cfg) =
	cvs motiontwin "co ocaml/swflib";
	cvs motiontwin "co ocaml/extc";
	cvs motiontwin "co ocaml/extlib-dev";
	cvs motiontwin "co ocaml/xml-light";
        cvs motiontwin "co neko/libs/include/ocaml";;

let action_build_deps(config) =
        if (not (Sys.file_exists "ocaml/swflib"))
          then action_fetch_deps(config);

	(* EXTLIB *)
	Sys.chdir "ocaml/extlib-dev";
	command ("ocaml install.ml -nodoc -d .. " ^ (if !bytecode then "-b " else "") ^ (if !native then "-n" else ""));
	msg "";
	Sys.chdir "../..";

	(* EXTC *)
	Sys.chdir "ocaml/extc";
	let c_opts = (if Sys.ocaml_version < "3.08" then " -ccopt -Dcaml_copy_string=copy_string " else " ") in
	command ("ocamlc" ^ c_opts ^ " -I ../ -I " ^ config.zlib_headers ^ " extc_stubs.c");

	let options = "-cclib ocaml/extc/extc_stubs" ^ config.obj_ext ^ " -cclib " ^ (Filename.dirname config.zlib_lib) ^ " extc.ml" in
	if !bytecode then command ("ocamlc -a -I .. -o extc.cma " ^ options);
	if !native then command ("ocamlopt -a -I .. -o extc.cmxa " ^ options);
	Sys.chdir "../..";

	(* SWFLIB *)
	Sys.chdir "ocaml/swflib";
	let files = "-I .. -I ../extc as3.mli as3hl.mli as3code.ml as3parse.ml as3hlparse.ml swf.ml actionScript.ml swfParser.ml" in
	if !bytecode then command ("ocamlc -a -o swflib.cma " ^ files);
	if !native then command ("ocamlopt -a -o swflib.cmxa " ^ files);
	Sys.chdir "../..";

	(* XML-LIGHT *)
	Sys.chdir "ocaml/xml-light";
	command ("ocamlyacc	xml_parser.mly");
	command ("ocamlc xml.mli dtd.mli xml_parser.mli xml_lexer.mli");
	command ("ocamllex xml_lexer.mll");
	let files = "xml_parser.ml xml_lexer.ml dtd.ml xmlParser.mli xmlParser.ml xml.ml" in
	if !bytecode then command ("ocamlc -a -o xml-light.cma " ^ files);
	if !native then command ("ocamlopt -a -o xml-light.cmxa " ^ files);
        Sys.chdir "../..";;

let action_checkout_haxe(cfg) =
    match List.filter Sys.file_exists ["./.svn";"./.git"] with
      | [] -> command "svn co http://haxe.googlecode.com/svn/trunk ."
      | _ -> msg ("Either.svn or .git found in current directory.\n"
                        ^ "  Skipping checkout\n");;

let action_build_haxe(cfg) = 
	(try Unix.mkdir "bin" 0o740 with Unix.Unix_error(Unix.EEXIST,_,_) -> ());

        (* TODO: check that libs have been compiled *)

	(* HAXE *)
	(* Sys.chdir "haxe"; *)
	command "ocamllex lexer.mll";
	ocamlc (path_str ^ " -pp camlp4o " ^ modules mlist ".ml") cfg;
	if !bytecode then command ("ocamlc -custom -o bin/haxe-byte" ^ cfg.exe_ext ^ libs_str ".cma" ^ modules mlist ".cmo");
	if !native then command ("ocamlopt -o bin/haxe" ^ cfg.exe_ext ^ libs_str ".cmxa" ^ modules mlist ".cmx");;



let action_clean(cfg) =
	msg "clean: implementation incomplete\n";
        indir "ocaml/extc" (fun() -> command ("make clean"));
        indir "ocaml" (fun() -> command "rm *.cmx *.cmi *.cma *.a *.o || true");
        command "rm *.cmx *.cmi *.cma *.a *.o || true";
	msg "TODO";;

let action_dist_clean(cfg) =
	action_clean cfg;
	command "rm -fr ocaml"
	
(* --- action definictions --- *)

let actions = [
  { name= "fetch_deps";
    action= action_fetch_deps;
    description= "fetch deps into " ^ deps_dir;
    default= true
  };
  { name= "build_deps";
    action= action_build_deps;
    description= "build fetched dependencies in " ^ deps_dir;
    default= true
  };
  { name= "checkout_haxe_current_dir";
    action= action_checkout_haxe;
    description= "checks out haxe into the current directory - if there is no .svn or .git directory";
    default= true
  };
  { name= "haxe";
    action= action_build_haxe;
    description= "build HaXe";
    default= true
  };
  { name= "dist_clean";
    action= action_dist_clean;
    description= "make dist clean - clean as much as possible";
    default= false
  };
  { name= "ocamake_create_makefile";
    action= (fun(cfg) -> 
      command(" ocamake -o bin/haxe -mak -opt -pp camlp4o " ^ path_str ^ " " ^ modules mlist ".ml");
      let append =
          "ocaml_xml_light = ocaml/xml-light/xml_parser.cmx ocaml/xml-light/xml_lexer.cmx ocaml/xml-light/dtd.cmx ocaml/xml-light/xmlParser.cmx ocaml/xml-light/xml.cmx\n"
        ^ "ocaml_swf_lib = ocaml/swflib/swf.cmx  ocaml/swflib/actionScript.cmx ocaml/swflib/as3code.cmx ocaml/swflib/as3parse.cmx ocaml/swflib/as3hlparse.cmx ocaml/swflib/swfParser.cmx \n"
        ^ "LIBS := $(LIBS) ocaml/extc/extc.cmxa unix.cmxa str.cmxa ./ocaml/extLib.cmxa ocaml/extc/extc.cmx $(ocaml_swf_lib) $(ocaml_xml_light) \n"
        ^ "LFLAGS := -shared $(LFLAGS)\n"
      in
        let chan = open_out_gen [Open_append] 0 "Makefile" in
        output_string chan append;
        close_out chan;
      );
    description= "use ocamake to create a makefile - TODO - verify that everything is build. -shared segfaults here. So maybe remove it and pass path to libz.so in LIBS";
    default= false
  };
  { name= "haxe_make";
    action= (fun(cfg) -> command("make"));
    description= "build HaXe by incremental makefile generated by action ocamake_create_makefile";
    default= false
  };
  { name= "clean";
    action= action_clean;
    description= "clean all build directories";
    default= false
  };
]

let default_actions =
      List.map
        (fun (x) -> x.name)
        (List.filter (fun (x) -> x.default) actions)

let all_actions = List.map (fun (x) -> x.name) actions

let usage =
	"install.ml: automatically fetches required sources and builds HaXe.\n"
      ^ "You can run install.ml alone - or run it within the HaXe source repository \n"
      ^ "\n"
      ^ " --actions names : only run actions\n"
      ^ "\n"
      ^ "default actions :" ^ (String.concat ", " default_actions) ^ "\n"
      ^ "available actions: " ^ (String.concat ", " all_actions) ^ "\n";;

(* -- parse arguments -- *)
Arg.parse arg_spec (fun arg -> argfiles := arg :: !argfiles) usage;;

let actions_to_run = match !actions_by_user with
	| [] -> default_actions
        | c -> c

let (zlib_headers, zlib_lib) = eval_option !zlib_option;;

let cfg = {
    zlib_headers  = zlib_headers;
    zlib_lib        = zlib_lib;
    obj_ext       = (match os_type with "Win32" -> ".obj" | _ -> ".o");
    ocamloptflags = (match os_type with "Unix" -> "-cclib -fno-stack-protector " | _ -> "");
    exe_ext       = (match os_type with "Win32" | "Cygwin" -> ".exe" | _ -> "")
};;

(* -- run actions (MAIN) -- *)
List.iter (fun (a) -> match List.filter (fun(action) -> String.compare action.name a == 0) actions with
    | [] -> failwith ("unkown action: " ^ a)
    | x::_ ->
        msg (">> running action : " ^ x.name ^ "\n");
        x.action(cfg);
  ) actions_to_run



(* vim: sw=8,noexpandtab
 * *)
