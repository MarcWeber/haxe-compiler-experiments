(*
 *  Haxe Compiler
 *  Copyright (c)2005-2008 Nicolas Cannasse
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
open Type

type package_rule =
	| Forbidden
	| Directory of string
	| Remap of string

type platform =
	| Cross
	| Flash8
	| Js
	| Neko
	| Flash
	| Php
	| Cpp
	| Cs
	| Java

type pos = Ast.pos

type basic_types = {
	mutable tvoid : t;
	mutable tint : t;
	mutable tfloat : t;
	mutable tbool : t;
	mutable tnull : t -> t;
	mutable tstring : t;
	mutable tarray : t -> t;
}

type stats = {
	s_files_parsed : int ref;
	s_classes_built : int ref;
	s_methods_typed : int ref;
	s_macros_called : int ref;
}

(**
	The capture policy tells which handling we make of captured locals
	(the locals which are referenced in local functions)

	See details/implementation in Codegen.captured_vars
*)
type capture_policy =
	(** do nothing, let the platform handle it *)
	| CPNone 
	(** wrap all captured variables into a single-element array to allow modifications *)
	| CPWrapRef
	(** similar to wrap ref, but will only apply to the locals that are declared in loops *)
	| CPLoopVars

type platform_config = {
	(** has a static type system, with not-nullable basic types (Int/Float/Bool) *)
	pf_static : bool;
	(** has access to the "sys" package *)
	pf_sys : bool;
	(** local variables are block-scoped *)
	pf_locals_scope : bool;
	(** captured local variables are scoped *)
	pf_captured_scope : bool;
	(** generated locals must be absolutely unique wrt the current function *)
	pf_unique_locals : bool;
	(** which expressions can be generated to initialize member variables (or will be moved into the constructor *)
	pf_can_init_member : tclass_field -> bool;
	(** captured variables handling (see before) *)
	pf_capture_policy : capture_policy;
	(** when calling a method with optional args, do we replace the missing args with "null" constants *)
	pf_pad_nulls : bool;
	(** add a final return to methods not having one already - prevent some compiler warnings *)
	pf_add_final_return : bool;
}

type context = {
	(* config *)
	version : int;
	args : string list;
	mutable display : bool;
	mutable debug : bool;
	mutable verbose : bool;
	mutable foptimize : bool;
	mutable platform : platform;
	mutable config : platform_config;
	mutable std_path : string list;
	mutable class_path : string list;
	mutable main_class : Type.path option;
	mutable defines : (string,unit) PMap.t;
	mutable package_rules : (string,package_rule) PMap.t;
	mutable error : string -> pos -> unit;
	mutable warning : string -> pos -> unit;
	mutable load_extern_type : (path -> pos -> (string * Ast.package) option) list; (* allow finding types which are not in sources *)
	mutable filters : (unit -> unit) list;
	mutable defines_signature : string option;
	mutable print : string -> unit;
	mutable get_macros : unit -> context option;
	(* output *)
	mutable file : string;
	mutable flash_version : float;
	mutable features : (string,bool) Hashtbl.t;
	mutable modules : Type.module_def list;
	mutable main : Type.texpr option;
	mutable types : Type.module_type list;
	mutable resources : (string,string) Hashtbl.t;
	mutable neko_libs : string list;
	mutable php_front : string option;
	mutable php_lib : string option;
	mutable php_prefix : string option;
	mutable swf_libs : (string * (unit -> Swf.swf) * (unit -> ((string list * string),As3hl.hl_class) Hashtbl.t)) list;
	mutable js_gen : (unit -> unit) option;
	(* typing *)
	mutable basic : basic_types;
}

exception Abort of string * Ast.pos

let display_default = ref false

let stats =
	{
		s_files_parsed = ref 0;
		s_classes_built = ref 0;
		s_methods_typed = ref 0;
		s_macros_called = ref 0;
	}

let default_config = 
	{
		pf_static = true;
		pf_sys = true;
		pf_locals_scope = true;
		pf_captured_scope = true;
		pf_unique_locals = false;
		pf_can_init_member = (fun _ -> true);
		pf_capture_policy = CPNone;
		pf_pad_nulls = false;
		pf_add_final_return = false;
	}

let get_config com =
	let defined f = PMap.mem f com.defines in
	match com.platform with
	| Cross ->
		default_config
	| Flash8 ->
		{
			pf_static = false;
			pf_sys = false;
			pf_locals_scope = com.flash_version > 6.;
			pf_captured_scope = false;
			pf_unique_locals = false;
			pf_can_init_member = (fun _ -> true);
			pf_capture_policy = CPLoopVars;
			pf_pad_nulls = false;
			pf_add_final_return = false;
		}
	| Js ->
		{
			pf_static = false;
			pf_sys = false;
			pf_locals_scope = false;		
			pf_captured_scope = false;
			pf_unique_locals = false;
			pf_can_init_member = (fun _ -> false);
			pf_capture_policy = CPLoopVars;
			pf_pad_nulls = false;
			pf_add_final_return = false;
		}
	| Neko ->
		{
			pf_static = false;
			pf_sys = true;
			pf_locals_scope = true;
			pf_captured_scope = true;
			pf_unique_locals = false;
			pf_can_init_member = (fun _ -> false);
			pf_capture_policy = CPNone;
			pf_pad_nulls = true;
			pf_add_final_return = false;
		}
	| Flash when defined "as3" ->
		{
			pf_static = true;
			pf_sys = false;
			pf_locals_scope = false;
			pf_captured_scope = true;
			pf_unique_locals = true;
			pf_can_init_member = (fun _ -> true);
			pf_capture_policy = CPLoopVars;
			pf_pad_nulls = false;
			pf_add_final_return = true;
		}
	| Flash ->
		{
			pf_static = true;
			pf_sys = false;
			pf_locals_scope = true;
			pf_captured_scope = true; (* handled by genSwf9 *)
			pf_unique_locals = false;
			pf_can_init_member = (fun _ -> false);
			pf_capture_policy = CPLoopVars;
			pf_pad_nulls = false;
			pf_add_final_return = false;
		}
	| Php ->
		{
			pf_static = false;
			pf_sys = true;
			pf_locals_scope = false; (* some duplicate work is done in genPhp *)
			pf_captured_scope = false;
			pf_unique_locals = false;
			pf_can_init_member = (fun cf ->
				match cf.cf_kind, cf.cf_expr with
				| Var { v_write = AccCall _ },  _ -> false
				| _, Some { eexpr = TTypeExpr _ } -> false
				| _ -> true 
			);
			pf_capture_policy = CPNone;
			pf_pad_nulls = true;
			pf_add_final_return = false;
		}
	| Cpp ->
		{
			pf_static = true;
			pf_sys = true;
			pf_locals_scope = true;
			pf_captured_scope = true;
			pf_unique_locals = false;
			pf_can_init_member = (fun _ -> false);
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_add_final_return = true;
		}
	| Cs ->
		{
			pf_static = true;
			pf_sys = true;
			pf_locals_scope = false;
			pf_captured_scope = true;
			pf_unique_locals = true;
			pf_can_init_member = (fun _ -> false);
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_add_final_return = false;
		}
	| Java ->
		{
			pf_static = true;
			pf_sys = true;
			pf_locals_scope = false;
			pf_captured_scope = true;
			pf_unique_locals = false;
			pf_can_init_member = (fun _ -> false);
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = true;
			pf_add_final_return = false;
		}

let create v args =
	let m = Type.mk_mono() in
	{
		version = v;
		args = args;
		debug = false;
		display = !display_default;
		verbose = false;
		foptimize = true;
		features = Hashtbl.create 0;
		platform = Cross;
		config = default_config;
		print = print_string;
		std_path = [];
		class_path = [];
		main_class = None;
		defines = PMap.add "true" () (if !display_default then PMap.add "display" () PMap.empty else PMap.empty);
		package_rules = PMap.empty;
		file = "";
		types = [];
		filters = [];
		modules = [];
		main = None;
		flash_version = 10.;
		resources = Hashtbl.create 0;
		php_front = None;
		php_lib = None;
		swf_libs = [];
		neko_libs = [];
		php_prefix = None;
		js_gen = None;
		load_extern_type = [];
		defines_signature = None;
		get_macros = (fun() -> None);
		warning = (fun _ _ -> assert false);
		error = (fun _ _ -> assert false);
		basic = {
			tvoid = m;
			tint = m;
			tfloat = m;
			tbool = m;
			tnull = (fun _ -> assert false);
			tstring = m;
			tarray = (fun _ -> assert false);
		};
	}

let log com str =
	if com.verbose then com.print (str ^ "\n")

let clone com =
	let t = com.basic in
	{ com with basic = { t with tvoid = t.tvoid }; main_class = None; }

let file_time file =
	try (Unix.stat file).Unix.st_mtime with _ -> 0.

let get_signature com =
	match com.defines_signature with
	| Some s -> s
	| None ->
		let str = String.concat "@" (PMap.foldi (fun k _ acc ->
			(* don't make much difference between these special compilation flags *)
			match k with
			| "display" | "use_rtti_doc" | "macrotimes" -> acc
			| _ -> k :: acc
		) com.defines []) in
		let s = Digest.string str in
		com.defines_signature <- Some s;
		s

let file_extension file =
	match List.rev (ExtString.String.nsplit file ".") with
	| e :: _ -> String.lowercase e
	| [] -> ""

let platforms = [
	Flash8;
	Js;
	Neko;
	Flash;
	Php;
	Cpp;
	Cs;
	Java;
]

let platform_name = function
	| Cross -> "cross"
	| Flash8 -> "flash8"
	| Js -> "js"
	| Neko -> "neko"
	| Flash -> "flash"
	| Php -> "php"
	| Cpp -> "cpp"
	| Cs -> "cs"
	| Java -> "java"

let flash_versions = List.map (fun v ->
	let maj = int_of_float v in
	let min = int_of_float (mod_float (v *. 10.) 10.) in
	v, string_of_int maj ^ (if min = 0 then "" else "_" ^ string_of_int min)
) [9.;10.;10.1;10.2;10.3;11.;11.1;11.2;11.3;11.4]

let defined ctx v = PMap.mem v ctx.defines

let define ctx v =
	ctx.defines <- PMap.add v () ctx.defines;
	let v = String.concat "_" (ExtString.String.nsplit v "-") in
	ctx.defines <- PMap.add v () ctx.defines;
	ctx.defines_signature <- None

let init_platform com pf =
	com.platform <- pf;
	let name = platform_name pf in
	let forbid acc p = if p = name || PMap.mem p acc then acc else PMap.add p Forbidden acc in
	com.package_rules <- List.fold_left forbid com.package_rules (List.map platform_name platforms);
	com.config <- get_config com;
(*	if com.config.pf_static then define com "static"; *)
	if com.config.pf_sys then define com "sys" else com.package_rules <- PMap.add "sys" Forbidden com.package_rules;
	define com name

let add_feature com f =
	Hashtbl.replace com.features f true

let rec has_feature com f =
	try
		Hashtbl.find com.features f
	with Not_found ->
		if com.types = [] then defined com "all_features" else
		match List.rev (ExtString.String.nsplit f ".") with
		| [] -> assert false
		| [cl] -> has_feature com (cl ^ ".*") 
		| meth :: cl :: pack ->
			let r = (try
				let path = List.rev pack, cl in
				(match List.find (fun t -> t_path t = path && not (has_meta ":realPath" (t_infos t).mt_meta)) com.types with
				| t when meth = "*" -> not (defined com "dce") || has_meta ":used" (t_infos t).mt_meta
				| TClassDecl c -> PMap.exists meth c.cl_statics || PMap.exists meth c.cl_fields
				| _ -> false)
			with Not_found ->
				false
			) in
			let r = r || defined com "all_features" in
			Hashtbl.add com.features f r;
			r

let error msg p = raise (Abort (msg,p))

let platform ctx p = ctx.platform = p

let add_filter ctx f =
	ctx.filters <- f :: ctx.filters

let find_file ctx f =
	let rec loop = function
		| [] -> raise Not_found
		| p :: l ->
			let file = p ^ f in
			if Sys.file_exists file then
				file
			else
				loop l
	in
	loop ctx.class_path

let get_full_path f = try Extc.get_full_path f with _ -> f

let unique_full_path = if Sys.os_type = "Win32" || Sys.os_type = "Cygwin" then (fun f -> String.lowercase (get_full_path f)) else get_full_path

(* ------------------------- TIMERS ----------------------------- *)

type timer_infos = {
	name : string;
	mutable start : float list;
	mutable total : float;
}

let get_time = Extc.time
let htimers = Hashtbl.create 0

let new_timer name =
	try
		let t = Hashtbl.find htimers name in
		t.start <- get_time() :: t.start;
		t
	with Not_found ->
		let t = { name = name; start = [get_time()]; total = 0.; } in
		Hashtbl.add htimers name t;
		t

let curtime = ref []

let close t =
	let start = (match t.start with
		| [] -> assert false
		| s :: l -> t.start <- l; s
	) in
	let now = get_time() in
	let dt = now -. start in
	t.total <- t.total +. dt;
	let rec loop() =
		match !curtime with
		| [] -> failwith ("Timer " ^ t.name ^ " closed while not active")
		| tt :: l -> curtime := l; if t != tt then loop()
	in
	loop();
	(* because of rounding errors while adding small times, we need to make sure that we don't have start > now *)
	List.iter (fun ct -> ct.start <- List.map (fun t -> let s = t +. dt in if s > now then now else s) ct.start) !curtime

let timer name =
	let t = new_timer name in
	curtime := t :: !curtime;
	(function() -> close t)

let rec close_times() =
	match !curtime with
	| [] -> ()
	| t :: _ -> close t; close_times()

