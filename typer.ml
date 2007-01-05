(*
 *  Haxe Compiler
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

open Ast
open Type

type error_msg =
	| Module_not_found of module_path
	| Unify of unify_error list
	| Custom of string
	| Protect of error_msg
	| Unknown_ident of string
	| Stack of error_msg * error_msg

type context = {
	(* shared *)
	types : (module_path, module_path) Hashtbl.t;
	modules : (module_path , module_def) Hashtbl.t;
	delays : (unit -> unit) list list ref;
	warn : string -> pos -> unit;
	error : error_msg -> pos -> unit;
	flash9 : bool;
	mutable std : module_def;
	mutable untyped : bool;
	mutable isproxy : bool;
	mutable super_call : bool;
	(* per-module *)
	current : module_def;
	mutable local_types : module_type list;
	(* per-class *)
	mutable curclass : tclass;
	mutable tthis : t;
	mutable type_params : (string * t) list;
	(* per-function *)
	mutable curmethod : string;
	mutable in_constructor : bool;
	mutable in_static : bool;
	mutable in_loop : bool;
	mutable ret : t;
	mutable locals : (string, t) PMap.t;
	mutable locals_map : (string, string) PMap.t;
	mutable locals_map_inv : (string, string) PMap.t;
	mutable opened : anon_status ref list;
}

(* ---------------------------------------------------------------------- *)
(* TOOLS *)

type access_kind =
	| AccNo of string
	| AccExpr of texpr
	| AccSet of texpr * string * t * string
	| AccSetField of texpr * string * t

type switch_mode =
	| CMatch of (string * (string option * t) list option)
	| CExpr of texpr

exception Error of error_msg * pos

let unify_error_msg ctx = function
	| Cannot_unify (t1,t2) ->
		s_type ctx t1 ^ " should be " ^ s_type ctx t2
	| Invalid_field_type s ->
		"Invalid type for field " ^ s ^ " :"
	| Has_no_field (t,n) ->
		s_type ctx t ^ " has no field " ^ n
	| Invalid_access (f,get) ->
		"Inconsistent " ^ (if get then "getter" else "setter") ^ " for field " ^ f
	| Invalid_visibility n ->
		"The field " ^ n ^ " is not public"
	| Not_matching_optional ->
		"Optional parameters can't be forced"

let rec error_msg = function
	| Module_not_found m -> "Class not found : " ^ s_type_path m
	| Unify l ->
		let ctx = print_context() in
		String.concat "\n" (List.map (unify_error_msg ctx) l)
	| Unknown_ident s -> "Unknown identifier : " ^ s
	| Custom s -> s
	| Stack (m1,m2) -> error_msg m1 ^ "\n" ^ error_msg m2
	| Protect m -> error_msg m

let forbidden_packages = ref []
let check_override = ref false

let error msg p = raise (Error (Custom msg,p))

let display_error ctx msg p = ctx.error (Custom msg) p

let load_ref : (context -> module_path -> pos -> module_def) ref = ref (fun _ _ _ -> assert false)
let type_expr_ref = ref (fun _ ?need_val _ -> assert false)
let type_module_ref = ref (fun _ _ _ _ -> assert false)
let generate_meta_data = ref (fun _ _ -> assert false)

let null p = mk (TConst TNull) (mk_mono()) p

let load ctx m p = (!load_ref) ctx m p

let context err warn =
	let empty =	{
		mpath = [] , "";
		mtypes = [];
		mimports = [];
	} in
	let ctx = {
		modules = Hashtbl.create 0;
		types = Hashtbl.create 0;
		delays = ref [];
		flash9 = Plugin.defined "flash9";
		in_constructor = false;
		in_static = false;
		in_loop = false;
		untyped = false;
		isproxy = false;
		super_call = false;
		ret = mk_mono();
		warn = warn;
		error = err;
		locals = PMap.empty;
		locals_map = PMap.empty;
		locals_map_inv = PMap.empty;
		local_types = [];
		type_params = [];
		curmethod = "";
		curclass = null_class;
		tthis = mk_mono();
		current = empty;
		std = empty;
		opened = [];
	} in
	ctx.std <- (try
		load ctx ([],"StdTypes") null_pos
	with
		Error (Module_not_found ([],"StdTypes"),_) ->
			error "Standard library not found" null_pos
	);
	ctx

let unify ctx t1 t2 p =
	try
		Type.unify t1 t2
	with
		Unify_error l ->
			if not ctx.untyped then ctx.error (Unify l) p

let unify_raise ctx t1 t2 p =
	try
		Type.unify t1 t2
	with
		Unify_error l ->
			if not ctx.untyped then raise (Error (Unify l,p))

let save_locals ctx =
	let locals = ctx.locals in
	let map = ctx.locals_map in
	let inv = ctx.locals_map_inv in
	(fun() ->
		ctx.locals <- locals;
		ctx.locals_map <- map;
		ctx.locals_map_inv <- inv;
	)

let add_local ctx v t =
	let rec loop n =
		let nv = (if n = 0 then v else v ^ string_of_int n) in
		if PMap.mem nv ctx.locals || PMap.mem nv ctx.locals_map_inv then
			loop (n+1)
		else begin
			ctx.locals <- PMap.add v t ctx.locals;
			if n <> 0 then begin
				ctx.locals_map <- PMap.add v nv ctx.locals_map;
				ctx.locals_map_inv <- PMap.add nv v ctx.locals_map_inv;
			end;
			nv
		end
	in
	loop 0

let gen_local ctx t =
	let rec loop n =
		let nv = (if n = 0 then "_g" else "_g" ^ string_of_int n) in
		if PMap.mem nv ctx.locals || PMap.mem nv ctx.locals_map_inv then
			loop (n+1)
		else
			nv
	in
	add_local ctx (loop 0) t

let exc_protect f =
	let rec r = ref (fun() ->
		try
			f r
		with
			| Error (Protect _,_) as e -> raise e
			| Error (m,p) -> raise (Error (Protect m,p))
	) in
	r

let mk_infos ctx p params =
	(EObjectDecl (
		("fileName" , (EConst (String (Filename.basename p.pfile)) , p)) ::
		("lineNumber" , (EConst (Int (string_of_int (Lexer.get_error_line p))),p)) ::
		("className" , (EConst (String (s_type_path ctx.curclass.cl_path)),p)) ::
		if ctx.curmethod = "" then
			params
		else
			("methodName", (EConst (String ctx.curmethod),p)) :: params
	) ,p)

let field_access ctx get f t e p =
	match if get then f.cf_get else f.cf_set with
	| NoAccess ->
		let normal = AccExpr (mk (TField (e,f.cf_name)) t p) in
		(match follow e.etype with
		| TInst (c,_) when is_parent c ctx.curclass -> normal
		| _ ->
			if ctx.untyped then normal else AccNo f.cf_name)
	| F9MethodAccess when not ctx.untyped ->
		error "Cannot redefine method with Flash9 : please use 'f9dynamic' before method declaration" p
	| NormalAccess | F9MethodAccess ->
		AccExpr (mk (TField (e,f.cf_name)) t p)
	| MethodAccess m ->
		if m = ctx.curmethod && e.eexpr = TConst TThis then
			AccExpr (mk (TField (e,f.cf_name)) t p)
		else if get then
			AccExpr (mk (TCall (mk (TField (e,m)) (mk_mono()) p,[])) t p)
		else
			AccSet (e,m,t,f.cf_name)

let acc_get g p =
	match g with
	| AccNo f -> error ("Field " ^ f ^ " cannot be accessed for reading") p
	| AccExpr e -> e
	| AccSet _ | AccSetField _ -> assert false

(** since load_type is used in PASS2 , it cannot access the structure of a type **)

let load_type_def ctx p tpath =
	let no_pack = fst tpath = [] in
	try
		List.find (fun t ->
			let tp = t_path t in
			tp = tpath || (no_pack && snd tp = snd tpath)
		) ctx.local_types
	with
		Not_found ->
			let tpath, m = (try
				if not no_pack || fst ctx.current.mpath = [] then raise Exit;
				let tpath2 = fst ctx.current.mpath , snd tpath in
				tpath2, load ctx tpath2 p
			with
				| Error (Module_not_found _,p2) when p == p2 -> tpath, load ctx tpath p
				| Exit -> tpath, load ctx tpath p
			) in
			try
				List.find (fun t -> not (t_private t) && t_path t = tpath) m.mtypes
			with
				Not_found -> error ("Module " ^ s_type_path tpath ^ " does not define type " ^ snd tpath) p

let rec load_normal_type ctx t p allow_no_params =
	try
		if t.tpackage <> [] then raise Not_found;
		let pt = List.assoc t.tname ctx.type_params in
		if t.tparams <> [] then error ("Class type parameter " ^ t.tname ^ " can't have parameters") p;
		pt
	with Not_found ->
		let types , path , f = match load_type_def ctx p (t.tpackage,t.tname) with
			| TClassDecl c -> c.cl_types , c.cl_path , (fun t -> TInst (c,t))
			| TEnumDecl e -> e.e_types , e.e_path , (fun t -> TEnum (e,t))
			| TTypeDecl t -> t.t_types , t.t_path , (fun tl -> TType(t,tl))
		in
		if allow_no_params && t.tparams = [] then
			f (List.map (fun (v,name,t) ->
				match follow t with
				| TEnum _ -> v, mk_mono()
				| _ -> error ("Type parameter " ^ name ^ " need constraint") p
			) types)
		else if path = ([],"Dynamic") then
			match t.tparams with
			| [] -> t_dynamic
			| [TPType (_,t)] -> TDynamic (load_type ctx p t)
			| _ -> error "Too many parameters for Dynamic" p
		else begin
			if List.length types <> List.length t.tparams then error ("Invalid number of type parameters for " ^ s_type_path path) p;			
			let tparams = List.map (fun t ->
				match t with
				| TPConst c ->
					let name = (match c with 
						| String s -> "S" ^ s
						| Int i -> "I" ^ i
						| Float f -> "F" ^ f
						| _ -> assert false
					) in
					VNo, TEnum ({ e_path = ([],name); e_pos = p; e_doc = None; e_private = false; e_extern = true; e_types = []; e_constrs = PMap.empty },[]), true
				| TPType (v1,t) -> v1, load_type ctx p t, false
			) t.tparams in
			let bparams = List.map (fun (v1,t,_) -> v1,t) tparams in
			let params = List.map2 (fun (v1,t,isconst) (v2,name,t2) ->
				if isconst <> (name = "Const") then error (if isconst then "Constant value unexpected here" else "Constant value excepted as type parameter") p;
				(match follow t2 with
				| TInst (c,[]) ->
					List.iter (fun (i,params) ->
						unify ctx t (apply_params types bparams (TInst (i,params))) p
					) c.cl_implements
				| TEnum (c,[]) -> ()
				| _ -> assert false);
				(match v1 with VNo -> v2 | _ -> v1) , t
			) tparams types in
			f params
		end

and load_type ctx p t =
	match t with
	| TPParent t -> load_type ctx p t
	| TPNormal t -> load_normal_type ctx t p false
	| TPExtend (t,l) ->
		(match load_type ctx p (TPAnonymous l) with
		| TAnon a ->
			let rec loop t =
				match follow t with
				| TInst (c,tl) ->
					let c2 = mk_class (fst c.cl_path,"+" ^ snd c.cl_path) p None false in
					PMap.iter (fun f _ ->
						try
							ignore(class_field c f);
							error ("Cannot redefine field " ^ f) p
						with
							Not_found -> ()
					) a.a_fields;
					c2.cl_super <- Some (c,tl);
					c2.cl_fields <- a.a_fields;
					TInst (c2,[])
				| TMono _ ->
					error "Please ensure correct initialization of cascading signatures" p
				| TAnon a2 ->
					PMap.iter (fun f _ ->
						if PMap.mem f a2.a_fields then error ("Cannot redefine field " ^ f) p
					) a.a_fields;
					mk_anon (PMap.foldi PMap.add a.a_fields a2.a_fields)
				| _ -> error "Cannot only extend classes and anonymous" p
			in
			loop (load_normal_type ctx t p false)
		| _ -> assert false)
	| TPAnonymous l ->
		let rec loop acc (n,pub,f,p) =
			if PMap.mem n acc then error ("Duplicate field declaration : " ^ n) p;
			let t , get, set = (match f with
				| AFVar t ->
					load_type ctx p t, NormalAccess, NormalAccess
				| AFFun (tl,t) ->
					let t = load_type ctx p t in
					let args = List.map (fun (name,o,t) -> name , o, load_type ctx p t) tl in
					TFun (args,t), NormalAccess, (if ctx.flash9 then F9MethodAccess else NormalAccess)
				| AFProp (t,i1,i2) ->
					let access m get =
						match m with
						| "null" -> NoAccess
						| "default" -> NormalAccess
						| "dynamic" -> MethodAccess ((if get then "get_"  else "set_") ^ n)
						| _ -> MethodAccess m
					in
					load_type ctx p t, access i1 true, access i2 false
			) in
			PMap.add n {
				cf_name = n;
				cf_type = t;
				cf_public = (match pub with None -> true | Some p -> p);
				cf_get = get;
				cf_set = set;
				cf_params = [];
				cf_expr = None;
				cf_doc = None;
			} acc
		in
		mk_anon (List.fold_left loop PMap.empty l)
	| TPFunction (args,r) ->
		match args with
		| [TPNormal { tpackage = []; tparams = []; tname = "Void" }] ->
			TFun ([],load_type ctx p r)
		| _ ->
			TFun (List.map (fun t -> "",false,load_type ctx p t) args,load_type ctx p r)

let load_type_opt ctx p t =
	match t with
	| None -> mk_mono()
	| Some t -> load_type ctx p t

let rec reverse_type t =
	match t with
	| TEnum (e,params) ->
		TPNormal { tpackage = fst e.e_path; tname = snd e.e_path; tparams = List.map reverse_param params }
	| TInst (c,params) ->
		TPNormal { tpackage = fst c.cl_path; tname = snd c.cl_path; tparams = List.map reverse_param params }
	| TType (t,params) ->
		TPNormal { tpackage = fst t.t_path; tname = snd t.t_path; tparams = List.map reverse_param params }
	| TFun (params,ret) ->
		TPFunction (List.map (fun (_,_,t) -> reverse_type t) params,reverse_type ret)
	| TAnon a ->
		TPAnonymous (PMap.fold (fun f acc ->
			(f.cf_name , Some f.cf_public, AFVar (reverse_type f.cf_type), null_pos) :: acc
		) a.a_fields [])
	| TDynamic t2 ->
		TPNormal { tpackage = []; tname = "Dynamic"; tparams = if t == t2 then [] else [TPType (VNo,reverse_type t2)] }
	| _ ->
		raise Exit

and reverse_param (v,t) =
	TPType (v , reverse_type t)

let extend_remoting ctx c t p async prot =
	if c.cl_super <> None then error "Cannot extend several classes" p;
	if ctx.isproxy then 
		() (* skip this proxy generation, we shouldn't need it anyway *)
	else
	let ctx2 = context ctx.error ctx.warn in
	let fb = !forbidden_packages in
	forbidden_packages := [];
	ctx2.isproxy <- true;
	let ct = (try load_normal_type ctx2 t p false with e -> forbidden_packages := fb; raise e) in
	forbidden_packages := fb;
	let tvoid = TPNormal { tpackage = []; tname = "Void"; tparams = [] } in
	let make_field name args ret =
		try
			let targs = List.map (fun (a,o,t) -> a, o, Some (reverse_type t)) args in
			let tret = reverse_type ret in
			let eargs = [EArrayDecl (List.map (fun (a,_,_) -> (EConst (Ident a),p)) args),p] in
			let targs , tret , eargs = if async then
				match tret with
				| TPNormal { tpackage = []; tname = "Void" } -> targs , tvoid , eargs @ [EConst (Ident "null"),p]
				| _ -> targs @ ["__callb",true,Some (TPFunction ([tret],tvoid))] , tvoid , eargs @ [EUntyped (EConst (Ident "__callb"),p),p]
			else
				targs, tret , eargs
			in
			let idname = EConst (String name) , p in
			(FFun (name,None,[APublic],[], {
				f_args = targs;
				f_type = Some tret;
				f_expr = (EBlock [
					(EReturn (Some (EUntyped (ECall (
						(EField (
							(ECall (
								(EField ((EConst (Ident "__cnx"),p),"__resolve"),p),
								[if prot then idname else ECall ((EConst (Ident "__unprotect__"),p),[idname]),p]
							),p)
						,"call"),p),eargs
					),p),p)),p)
				],p);
			}),p)
		with
			Exit -> error ("Field " ^ name ^ " type is not complete and cannot be used by RemotingProxy") p
	in
	let class_fields = (match ct with
		| TInst (c,params) ->
			(FVar ("__cnx",None,[],Some (TPNormal { tpackage = ["haxe";"remoting"]; tname = if async then "AsyncConnection" else "Connection"; tparams = [] }),None),p) ::
			(FFun ("new",None,[APublic],[],{ f_args = ["c",false,None]; f_type = None; f_expr = (EBinop (OpAssign,(EConst (Ident "__cnx"),p),(EConst (Ident "c"),p)),p) }),p) ::
			PMap.fold (fun f acc ->
				if not f.cf_public then
					acc
				else match follow f.cf_type with
				| TFun (args,ret) when f.cf_get = NormalAccess && (f.cf_set = NormalAccess || f.cf_set = F9MethodAccess) && f.cf_params = [] ->
					make_field f.cf_name args ret :: acc
				| _ -> acc
			) c.cl_fields []
		| _ ->
			error "Remoting type parameter should be a class" p
	) in
	let class_decl = (EClass {
		d_name = t.tname;
		d_doc = None;
		d_params = [];
		d_flags = [];
		d_data = class_fields;
	},p) in
	let m = (try Hashtbl.find ctx2.modules (t.tpackage,t.tname) with Not_found -> assert false) in
	let mdecl = (List.map (fun (m,t) -> (EImport (fst m.mpath, snd m.mpath, t),p)) m.mimports) @ [class_decl] in
	let m = (!type_module_ref) ctx ("Remoting" :: t.tpackage,t.tname) mdecl p in
	c.cl_super <- Some (match m.mtypes with
		| [TClassDecl c] -> (c,[])
		| _ -> assert false
	)

let extend_proxy ctx c t p =
	if c.cl_super <> None then error "Cannot extend several classes" p;
	let tclass = load_normal_type ctx t p false in
	let tdyn = TPNormal { tpackage = []; tname = "Dynamic"; tparams = []; } in
	let make_field f args =
		let args = List.map (fun (name,o,t) -> name , o, Some tdyn) args in
		let eargs = List.map (fun (name,_,_) -> EConst (Ident name) , p) args in
		f.cf_name , (FFun (f.cf_name,None,[if f.cf_public then APublic else APrivate],[], {
			f_args = args;
			f_type = None;
			f_expr = (EBlock [
				(EReturn (Some (ECall (
					(EConst (Ident "handleCall"),p),
					[(ECall ((EConst (Ident "__unprotect__"),p),[EConst (String f.cf_name),p]),p); (EArrayDecl eargs,p)]
				),p)),p)
			],p);
		}),p)
	in
	let class_fields = (match tclass with
		| TInst (c,params) ->
			let rec loop c =
				PMap.fold (fun f acc ->
					match follow f.cf_type with
					| TFun (args,ret) ->
						(try
							ignore(List.assoc f.cf_name acc);
							acc
						with
							Not_found -> make_field f args :: acc)
					| _ -> acc
				) c.cl_fields (match c.cl_super with None -> [] | Some (c,_) -> loop c)
			in
			List.map snd (loop c)
		| _ ->
			error "Proxy type parameter should be a class" p
	) in
	let tproxy = { tpackage = ["haxe"]; tname = "Proxy"; tparams = [TPType (VNo,TPNormal t)] } in
	let pname = "P" ^ t.tname in
	let class_decl = (EClass {
		d_name = pname;
		d_doc = None;
		d_params = List.map (fun (v,s,_) -> v,s,[]) c.cl_types;
		d_flags = [HExtends tproxy; HImplements t];
		d_data = class_fields;
	},p) in
	let m = (!type_module_ref) ctx ("Proxy" :: t.tpackage, pname) [class_decl] p in
	c.cl_super <- Some (match m.mtypes with
		| [TClassDecl c2] -> (c2,List.map (fun (v,_,t) -> v,t) c.cl_types)
		| _ -> assert false
	)

let set_heritance ctx c herits p =
	let rec loop = function
		| HPrivate | HExtern | HInterface ->
			()
		| HExtends { tpackage = ["haxe";"remoting"]; tname = "Proxy"; tparams = [TPType(_,TPNormal t)] } ->
			extend_remoting ctx c t p false true
		| HExtends { tpackage = ["haxe";"remoting"]; tname = "AsyncProxy"; tparams = [TPType(_,TPNormal t)] } ->
			extend_remoting ctx c t p true true
		| HExtends { tpackage = ["mt"]; tname = "AsyncProxy"; tparams = [TPType(_,TPNormal t)] } ->
			extend_remoting ctx c t p true false
		| HExtends { tpackage = ["haxe"]; tname = "Proxy"; tparams = [TPType(_,TPNormal t)] } when match c.cl_path with "Proxy" :: _ , _ -> false | _ -> true ->
			extend_proxy ctx c t p
		| HExtends t ->
			if c.cl_super <> None then error "Cannot extend several classes" p;
			let t = load_normal_type ctx t p false in
			(match follow t with
			| TInst (cl,params) ->
				if is_parent c cl then error "Recursive class" p;
				if c.cl_interface then error "Cannot extend an interface" p;
				if cl.cl_interface then error "Cannot extend by using an interface" p;
				c.cl_super <- Some (cl,params)
			| _ -> error "Should extend by using a class" p)
		| HImplements t ->
			let t = load_normal_type ctx t p false in
			(match follow t with
			| TInst (cl,params) ->
				if is_parent c cl then error "Recursive class" p;
				c.cl_implements <- (cl, params) :: c.cl_implements
			| TDynamic t ->
				if c.cl_dynamic <> None then error "Cannot have several dynamics" p;
				c.cl_dynamic <- Some t
			| _ -> error "Should implement by using an interface or a class" p)
	in
	List.iter loop herits

let type_type_params ctx path p (v,n,flags) =
	let t = (match flags with
	| [] ->
		(* build a phantom enum *)
		let e = {
			e_path = (fst path @ [snd path],n);
			e_pos = p;
			e_private = true;
			e_extern = true;
			e_types = [];
			e_constrs = PMap.empty;
			e_doc = None;
		} in
		TEnum (e,[])
	| l ->
		(* build a phantom class *)
		let c = mk_class (fst path @ [snd path],n) p None true in
		let t = TInst (c,[]) in
		let r = exc_protect (fun r ->
			r := (fun _ -> t);
			set_heritance ctx c (List.map (fun t -> HImplements t) l) p;
			t
		) in
		ctx.delays := [(fun () -> ignore(!r()))] :: !(ctx.delays);
		TLazy r
	) in
	v, n , t

let hide_types ctx =
	let old_locals = ctx.local_types in
	let old_type_params = ctx.type_params in
	ctx.local_types <- (try (Hashtbl.find ctx.modules ([],"StdTypes")).mtypes with Not_found -> assert false);
	ctx.type_params <- [];
	(fun() ->
		ctx.local_types <- old_locals;
		ctx.type_params <- old_type_params;
	)

let load_core_type ctx name =
	let show = hide_types ctx in
	let t = load_normal_type ctx { tpackage = []; tname = name; tparams = [] } null_pos false in
	show();
	t

let t_int ctx = load_core_type ctx "Int"
let t_float ctx = load_core_type ctx "Float"
let t_bool ctx = load_core_type ctx "Bool"
let t_void ctx = load_core_type ctx "Void"
let t_string ctx = load_core_type ctx "String"

let is_int t =
	match follow t with
	| TInst (c,[]) ->
		c.cl_path = ([],"Int")
	| _ ->
		false

let is_float t =
	match follow t with
	| TInst (c,[]) ->
		c.cl_path = ([],"Float")
	| _ ->
		false

let t_array ctx v =
	let show = hide_types ctx in
	match load_type_def ctx null_pos ([],"Array") with
	| TClassDecl c ->
		show();
		if List.length c.cl_types <> 1 then assert false;
		let pt = mk_mono() in
		TInst (c,[v,pt]) , pt
	| _ ->
		assert false

let t_array_access ctx v =
	let show = hide_types ctx in
	match load_type_def ctx null_pos ([],"ArrayAccess") with
	| TClassDecl c ->
		show();
		if List.length c.cl_types <> 1 then assert false;
		let pt = mk_mono() in
		TInst (c,[v,pt]) , pt
	| _ ->
		assert false

let t_iterator ctx =
	let show = hide_types ctx in
	match load_type_def ctx null_pos ([],"Iterator") with
	| TTypeDecl t ->
		show();
		if List.length t.t_types <> 1 then assert false;
		let pt = mk_mono() in
		apply_params t.t_types [VNo,pt] t.t_type, pt
	| _ ->
		assert false

let rec return_flow ctx e =
	let error() = display_error ctx "A return is missing here" e.epos; raise Exit in
	let return_flow = return_flow ctx in
	match e.eexpr with
	| TReturn _ | TThrow _ -> ()
	| TParenthesis e ->
		return_flow e
	| TBlock el ->
		let rec loop = function
			| [] -> error()
			| [e] -> return_flow e
			| { eexpr = TReturn _ } :: _ | { eexpr = TThrow _ } :: _ -> ()
			| _ :: l -> loop l
		in
		loop el
	| TIf (_,e1,Some e2) ->
		return_flow e1;
		return_flow e2;
	| TSwitch (_,cases,Some e) ->
		List.iter (fun (_,e) -> return_flow e) cases;
		return_flow e
	| TMatch (_,_,cases,def) ->
		List.iter (fun (_,_,e) -> return_flow e) cases;
		(match def with None -> () | Some e -> return_flow e)
	| TTry (e,cases) ->
		return_flow e;
		List.iter (fun (_,_,e) -> return_flow e) cases;
	| _ ->
		error()

(* ---------------------------------------------------------------------- *)
(* PASS 3 : type expression & check structure *)

let unify_call_params ctx name el args p =
	let error flag =
		let format_arg = (fun (name,opt,_) -> (if opt then "?" else "") ^ name) in
		let argstr = "Function " ^ (match name with None -> "" | Some n -> "'" ^ n ^ "' ") ^ "requires " ^ (if args = [] then "no arguments" else "arguments : " ^ String.concat ", " (List.map format_arg args)) in
		display_error ctx ((if flag then "Not enough" else "Too many") ^ " arguments\n" ^ argstr) p
	in
	let rec no_opt = function
		| [] -> []
		| ({ eexpr = TConst TNull },true) :: l -> no_opt l
		| l -> List.map fst l
	in
	let rec default_value t =
		let rec is_pos_infos = function
			| TMono r ->
				(match !r with
				| Some t -> is_pos_infos t
				| _ -> false)
			| TLazy f ->
				is_pos_infos (!f())
			| TType ({ t_path = ["haxe"] , "PosInfos" },[]) ->
				true
			| _ ->
				false
		in
		if is_pos_infos t then
			let infos = mk_infos ctx p [] in
			let e = (!type_expr_ref) ctx ~need_val:true infos in
			(e, true)
		else
			(null p, true)
	in
	let rec loop acc l l2 =
		match l , l2 with
		| [] , [] ->
			if Plugin.defined "flash" || Plugin.defined "js" then
				List.rev (no_opt acc)
			else
				List.rev (List.map fst acc)
		| [] , (_,false,_) :: _ ->
			error true;
			[]
		| [] , (_,true,t) :: l ->
			loop (default_value t :: acc) [] l
		| _ , [] ->
			error false;
			[]
		| e :: l, (name,opt,t) :: l2 ->
			try
				unify_raise ctx e.etype t e.epos;
				loop ((e,false) :: acc) l l2
			with
				Error (Unify ul,_) ->
					if opt then
						loop (default_value t :: acc) (e :: l) l2
					else
						raise (Error (Stack (Unify ul,Custom ("For function argument '" ^ name ^ "'")), p))
	in
	loop [] el args

let type_local ctx i p =
	(* local lookup *)
	let t = PMap.find i ctx.locals in
	let i = (try PMap.find i ctx.locals_map with Not_found -> i) in
	mk (TLocal i) t p

let type_type ctx tpath p =
	let rec loop t tparams =
	match t with
	| TClassDecl c ->
		let pub = is_parent c ctx.curclass in
		let types = (match tparams with
			| None ->
				List.map (fun (v,_,t) ->
					v, match follow t with
					| TEnum _ -> mk_mono()
					| _ -> t
				) c.cl_types
			| Some l ->
				l
		) in
		let t_tmp = {
			t_path = fst c.cl_path, "#" ^ snd c.cl_path;
			t_doc = None;
			t_pos = c.cl_pos;
			t_type = if pub then mk_anon (PMap.map (fun f -> { f with cf_public = true }) c.cl_statics) else TAnon { a_fields = c.cl_statics; a_status = static_status };
			t_private = true;
			t_static = Some c;
			t_types = c.cl_types;
		} in
		mk (TTypeExpr (TClassDecl c)) (TType (t_tmp,types)) p
	| TEnumDecl e ->
		let types = (match tparams with None -> List.map (fun (v,_,_) -> v,mk_mono()) e.e_types | Some l -> l) in
		let fl = PMap.fold (fun f acc ->
			PMap.add f.ef_name {
				cf_name = f.ef_name;
				cf_public = true;
				cf_type = f.ef_type;
				cf_get = NormalAccess;
				cf_set = NoAccess;
				cf_doc = None;
				cf_expr = None;
				cf_params = [];
			} acc
		) e.e_constrs PMap.empty in
		let t_tmp = {
			t_path = fst e.e_path, "#" ^ snd e.e_path;
			t_doc = None;
			t_pos = e.e_pos;
			t_type = mk_anon fl;
			t_private = true;
			t_static = None;
			t_types = e.e_types;
		} in
		mk (TTypeExpr (TEnumDecl e)) (TType (t_tmp,types)) p
	| TTypeDecl s ->
		match follow s.t_type with
		| TEnum (e,params) ->
			loop (TEnumDecl e) (Some params)
		| TInst (c,params) ->
			loop (TClassDecl c) (Some params)
		| _ ->
			error (s_type_path tpath ^ " is not a value") p
	in
	loop (load_type_def ctx p tpath) None

let type_ident ctx i is_type p get =
	match i with
	| "true" ->
		if get then
			AccExpr (mk (TConst (TBool true)) (t_bool ctx) p)
		else
			AccNo i
	| "false" ->
		if get then
			AccExpr (mk (TConst (TBool false)) (t_bool ctx) p)
		else
			AccNo i
	| "this" ->
		if not ctx.untyped && ctx.in_static then error "Cannot access this from a static function" p;
		if get then
			AccExpr (mk (TConst TThis) ctx.tthis p)
		else
			AccNo i
	| "super" ->
		if not ctx.super_call then
			AccNo i
		else
		let t = (match ctx.curclass.cl_super with
		| None -> error "Current class does not have a superclass" p
		| Some (c,params) -> TInst(c,params)
		) in
		if ctx.in_static then error "Cannot access super from a static function" p;
		ctx.super_call <- false;
		if get then
			AccExpr (mk (TConst TSuper) t p)
		else
			AccNo i
	| "null" ->
		if get then
			AccExpr (mk (TConst TNull) (mk_mono()) p)
		else
			AccNo i
	| "here" ->
		let infos = mk_infos ctx p [] in
		let e = (!type_expr_ref) ctx ~need_val:true infos in
		if get then
			AccExpr { e with etype = load_normal_type ctx { tpackage = ["haxe"]; tname = "PosInfos"; tparams = [] } p false }
		else
			AccNo i
	| _ ->
	try
		let e = type_local ctx i p in
		AccExpr e
	with Not_found -> try
		(* member variable lookup *)
		if ctx.in_static then raise Not_found;
		let t , f = class_field ctx.curclass i in
		field_access ctx get f t (mk (TConst TThis) ctx.tthis p) p
	with Not_found -> try
		(* static variable lookup *)
		let f = PMap.find i ctx.curclass.cl_statics in
		(* expr type is not accurate but needed for protect *)
		let tt = mk (TTypeExpr (TClassDecl ctx.curclass)) (TInst (ctx.curclass,[])) p in
		field_access ctx get f (field_type f) tt p
	with Not_found -> try
		(* lookup imported *)
		let rec loop l =
			match l with
			| [] -> raise Not_found
			| t :: l ->
				match t with
				| TClassDecl _ | TTypeDecl _ ->
					loop l
				| TEnumDecl e ->
					try
						let ef = PMap.find i e.e_constrs in
						mk (TEnumField (e,i)) (monomorphs e.e_types ef.ef_type) p
					with
						Not_found -> loop l
		in
		let e = loop ctx.local_types in
		if get then
			AccExpr e
		else
			AccNo i
	with Not_found -> try
		(* lookup type *)
		if not is_type then raise Not_found;
		let e = (try type_type ctx ([],i) p with Error (Module_not_found ([],name),_) when name = i -> raise Not_found) in
		AccExpr e
	with Not_found ->
		if ctx.untyped then
			AccExpr (mk (TLocal i) (mk_mono()) p)
		else begin
			if ctx.in_static && PMap.mem i ctx.curclass.cl_fields then error ("Cannot access " ^ i ^ " in static function") p;
			raise (Error (Unknown_ident i,p))
		end

let type_constant ctx c p =
	match c with
	| Int s ->
		(try
			mk (TConst (TInt (Int32.of_string s))) (t_int ctx) p
		with
			_ -> mk (TConst (TFloat s)) (t_float ctx) p)
	| Float f -> mk (TConst (TFloat f)) (t_float ctx) p
	| String s -> mk (TConst (TString s)) (t_string ctx) p
	| Regexp (r,opt) ->
		let str = mk (TConst (TString r)) (t_string ctx) p in
		let opt = mk (TConst (TString opt)) (t_string ctx) p in
		let t = load_core_type ctx "EReg" in
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> assert false),[],[str;opt])) t p
	| Ident _
	| Type _ -> assert false

let check_assign ctx e =
	match e.eexpr with
	| TLocal _ | TArray _ | TField _ ->
		()
	| TTypeExpr _ when ctx.untyped ->
		()
	| _ ->
		error "Invalid assign" e.epos

let type_matching ctx (enum,params) (e,p) ecases =
	let invalid() = error "Invalid enum matching" p in
	let needs n = error ("This constructor needs " ^ string_of_int n ^ " parameters") p in
	let constr name =
		if PMap.mem name (!ecases) then error "This constructor has already been used" p;
		ecases := PMap.add name () (!ecases);
		try
			PMap.find name enum.e_constrs
		with
			Not_found -> error ("This constructor is not part of the enum " ^ s_type_path enum.e_path) p
	in
	match e with
	| EConst (Ident name) | EConst (Type name) ->
		let c = constr name in
		(match c.ef_type with
			| TFun (l,_) -> needs (List.length l)
			| TEnum _ -> ()
			| _ -> assert false
		);
		(name,None)
	| ECall ((EConst (Ident name),_),el)
	| ECall ((EConst (Type name),_),el) ->
		let c = constr name in
		let args = (match c.ef_type with
			| TFun (l,_) ->
				if List.length l <> List.length el then needs (List.length l);
				List.map (fun (_,_,t) -> apply_params enum.e_types params t) l
			| TEnum _ -> error "This constructor does not take any paramter" p
			| _ -> assert false
		) in
		let idents = List.map2 (fun (e,_) t ->
			match e with
			| EConst (Ident "_") ->
				None , t
			| EConst (Ident name) | EConst (Type name) ->
				let name = add_local ctx name t in
				Some name , t
			| _ -> invalid()
		) el args in
		(name,Some idents)
	| _ ->
		invalid()

let type_field ctx e i p get =
	let no_field() =
		if not ctx.untyped then display_error ctx (s_type (print_context()) e.etype ^ " has no field " ^ i) p;
		AccExpr (mk (TField (e,i)) (mk_mono()) p)
	in
	match follow e.etype with
	| TInst (c,params) ->
		let rec loop_dyn c params =
			match c.cl_dynamic with
			| Some t ->
				let t = apply_params c.cl_types params t in
				if get && PMap.mem "__resolve" c.cl_fields then
					AccExpr (mk (TCall (mk (TField (e,"__resolve")) (mk_mono()) p,[type_constant ctx (String i) p])) t p)
				else if not get && PMap.mem "__setfield" c.cl_fields then
					AccSetField (e,i,t)
				else
					AccExpr (mk (TField (e,i)) t p)
			| None ->
				match c.cl_super with
				| None -> raise Not_found
				| Some (c,params) -> loop_dyn c params
		in
		(try
			let t , f = class_field c i in
			if ctx.flash9 && e.eexpr = TConst TSuper && f.cf_set = NormalAccess then error "Cannot access superclass variable for calling : needs to be a proper method" p;
			if not f.cf_public && not (is_parent c ctx.curclass) && not ctx.untyped then display_error ctx ("Cannot access to private field " ^ i) p;
			field_access ctx get f (apply_params c.cl_types params t) e p
		with Not_found -> try
			loop_dyn c params
		with Not_found ->
			if PMap.mem i c.cl_statics then error ("Cannot access static field " ^ i ^ " from a class instance") p;
			no_field())
	| TDynamic t ->
		AccExpr (mk (TField (e,i)) t p)
	| TAnon a ->
		(try
			let f = PMap.find i a.a_fields in
			if !(a.a_status) <> Closed && not f.cf_public && not ctx.untyped then display_error ctx ("Cannot access to private field " ^ i) p;
			field_access ctx get f (field_type f) e p
		with Not_found ->
			if is_closed a then
				no_field()
			else
			let f = mk_field i (mk_mono()) in
			a.a_fields <- PMap.add i f a.a_fields;
			field_access ctx get f (field_type f) e p
		)
	| TMono r ->
		if ctx.untyped && Plugin.defined "swf-mark" && Plugin.defined "flash" then ctx.warn "Mark" p;
		let f = mk_field i (mk_mono()) in
		let x = ref Opened in
		let t = TAnon { a_fields = PMap.add i f PMap.empty; a_status = x } in
		ctx.opened <- x :: ctx.opened;
		r := Some t;
		field_access ctx get f (field_type f) e p
	| t ->
		no_field()

type type_class =
	| KInt
	| KFloat
	| KString
	| KUnk
	| KDyn
	| KOther

let classify t =
	match follow t with
	| TInst ({ cl_path = ([],"Int") },[]) -> KInt
	| TInst ({ cl_path = ([],"Float") },[]) -> KFloat
	| TInst ({ cl_path = ([],"String") },[]) -> KString
	| TMono r when !r = None -> KUnk
	| TDynamic _ -> KDyn
	| _ -> KOther

let rec type_binop ctx op e1 e2 p =
	match op with
	| OpAssign ->
		let e1 = type_access ctx (fst e1) (snd e1) false in
		let e2 = type_expr ctx e2 in
		(match e1 with
		| AccNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AccExpr e1 ->
			unify ctx e2.etype e1.etype p;
			check_assign ctx e1;
			(match e1.eexpr , e2.eexpr with
			| TLocal i1 , TLocal i2
			| TField ({ eexpr = TConst TThis },i1) , TField ({ eexpr = TConst TThis },i2) when i1 = i2 ->
				error "Assigning a value to itself" p
			| _ , _ -> ());
			mk (TBinop (op,e1,e2)) e1.etype p
		| AccSetField (e,f,t) ->
			unify ctx e2.etype t p;
			mk (TCall (mk (TField (e,"__setfield")) (mk_mono()) p,[mk (TConst (TString f)) (mk_mono()) p; e2])) t p
		| AccSet (e,m,t,_) ->
			unify ctx e2.etype t p;
			mk (TCall (mk (TField (e,m)) (mk_mono()) p,[e2])) t p)
	| OpAssignOp op ->
		(match type_access ctx (fst e1) (snd e1) false with
		| AccNo s -> error ("Cannot access field or identifier " ^ s ^ " for writing") p
		| AccExpr e ->
			let eop = type_binop ctx op e1 e2 p in
			(match eop.eexpr with
			| TBinop (_,_,e2) ->
				unify ctx e2.etype e.etype p;
				check_assign ctx e;
				mk (TBinop (OpAssignOp op,e,e2)) e.etype p;
			| _ ->
				assert false)
		| AccSetField _ ->
			error "This kind of operation is not supported" p
		| AccSet (e,m,t,f) ->
			let l = save_locals ctx in
			let v = gen_local ctx e.etype in
			let ev = mk (TLocal v) e.etype p in
			let get = type_binop ctx op (EField ((EConst (Ident v),p),f),p) e2 p in
			unify ctx get.etype t p;
			l();
			mk (TBlock [
				mk (TVars [v,e.etype,Some e]) (t_void ctx) p;
				mk (TCall (mk (TField (ev,m)) (mk_mono()) p,[get])) t p
			]) t p)
	| _ ->
	let e1 = type_expr ctx e1 in
	let e2 = type_expr ctx e2 in
	let mk_op t = mk (TBinop (op,e1,e2)) t p in
	match op with
	| OpAdd ->
		mk_op (match classify e1.etype, classify e2.etype with
		| KInt , KInt ->
			t_int ctx
		| KFloat , KInt
		| KInt, KFloat
		| KFloat, KFloat ->
			t_float ctx
		| KUnk , KInt
		| KUnk , KFloat
		| KUnk , KString  ->
			unify ctx e1.etype e2.etype e1.epos;
			e1.etype
		| KInt , KUnk
		| KFloat , KUnk
		| KString , KUnk ->
			unify ctx e2.etype e1.etype e2.epos;
			e2.etype
		| _ , KString
		| _ , KDyn ->
			e2.etype
		| KString , _
		| KDyn , _ ->
			e1.etype
		| KUnk , KUnk ->
			let t = t_int ctx in
			unify ctx e1.etype t e1.epos;
			unify ctx e2.etype t e2.epos;
			t
		| KOther, _
		| _ , KOther ->
			let pr = print_context() in
			error ("Cannot add " ^ s_type pr e1.etype ^ " and " ^ s_type pr e2.etype) p
		)
	| OpAnd
	| OpOr
	| OpXor
	| OpShl
	| OpShr
	| OpUShr ->
		let i = t_int ctx in
		unify ctx e1.etype i e1.epos;
		unify ctx e2.etype i e2.epos;
		mk_op i
	| OpMod
	| OpMult
	| OpDiv
	| OpSub ->
		let i = t_int ctx in
		let f1 = is_float e1.etype in
		let f2 = is_float e2.etype in
		if not f1 then unify ctx e1.etype i e1.epos;
		if not f2 then unify ctx e2.etype i e2.epos;
		if op <> OpDiv && not f1 && not f2 then
			mk_op i
		else
			mk_op (t_float ctx)
	| OpEq
	| OpPhysEq
	| OpPhysNotEq
	| OpNotEq ->
		(try
			unify_raise ctx e1.etype e2.etype p
		with
			Error (Unify _,_) -> unify ctx e2.etype e1.etype p);
		mk_op (t_bool ctx)
	| OpGt
	| OpGte
	| OpLt
	| OpLte ->
		(match classify e1.etype, classify e2.etype with
		| KInt , KInt | KInt , KFloat | KFloat , KInt | KFloat , KFloat | KString , KString -> ()
		| KInt , KUnk | KFloat , KUnk | KString , KUnk -> unify ctx e2.etype e1.etype e2.epos
		| KUnk , KInt | KUnk , KFloat | KUnk , KString -> unify ctx e1.etype e2.etype e1.epos
		| KUnk , KUnk ->
			let t = t_int ctx in
			unify ctx e1.etype t e1.epos;
			unify ctx e2.etype t e2.epos;
		| KDyn , KInt | KDyn , KFloat | KDyn , KString -> ()
		| KInt , KDyn | KFloat , KDyn | KString , KDyn -> ()
		| KDyn , KDyn -> ()
		| KDyn , KUnk
		| KUnk , KDyn
		| KString , KInt
		| KString , KFloat
		| KInt , KString
		| KFloat , KString
		| KOther , _
		| _ , KOther ->
			let pr = print_context() in
			error ("Cannot compare " ^ s_type pr e1.etype ^ " and " ^ s_type pr e2.etype) p
		);
		mk_op (t_bool ctx)
	| OpBoolAnd
	| OpBoolOr ->
		let b = t_bool ctx in
		unify ctx e1.etype b p;
		unify ctx e2.etype b p;
		mk_op b
	| OpInterval ->
		let i = t_int ctx in
		let t = load_core_type ctx "IntIter" in
		unify ctx e1.etype i e1.epos;
		unify ctx e2.etype i e2.epos;
		mk (TNew ((match t with TInst (c,[]) -> c | _ -> assert false),[],[e1;e2])) t p
	| OpAssign
	| OpAssignOp _ ->
		assert false

and type_unop ctx op flag e p =
	let set = (op = Increment || op = Decrement) in
	let acc = type_access ctx (fst e) (snd e) (not set) in
	match acc with
	| AccExpr e ->
		let t = (match op with
		| Not ->
			let b = t_bool ctx in
			unify ctx e.etype b e.epos;
			b
		| Increment
		| Decrement
		| Neg
		| NegBits ->
			if set then check_assign ctx e;
			if is_float e.etype then
				t_float ctx
			else begin
				unify ctx e.etype (t_int ctx) e.epos;
				t_int ctx
			end
		) in
		(match op, e.eexpr with
		| Neg , TConst (TInt i) -> mk (TConst (TInt (Int32.neg i))) t p
		| _ -> mk (TUnop (op,flag,e)) t p)
	| AccNo s ->
		error ("The field or identifier " ^ s ^ " is not accessible for " ^ (if set then "writing" else "reading")) p
	| AccSetField _ ->
		error "This kind of operation is not supported" p
	| AccSet (e,m,t,f) ->
		let l = save_locals ctx in
		let v = gen_local ctx e.etype in
		let ev = mk (TLocal v) e.etype p in
		let op = (match op with Increment -> OpAdd | Decrement -> OpSub | _ -> assert false) in
		let one = (EConst (Int "1"),p) in
		let eget = (EField ((EConst (Ident v),p),f),p) in
		match flag with
		| Prefix ->
			let get = type_binop ctx op eget one p in
			unify ctx get.etype t p;
			l();
			mk (TBlock [
				mk (TVars [v,e.etype,Some e]) (t_void ctx) p;
				mk (TCall (mk (TField (ev,m)) (mk_mono()) p,[get])) t p
			]) t p
		| Postfix ->
			let v2 = gen_local ctx t in
			let ev2 = mk (TLocal v2) t p in
			let get = type_expr ctx eget in
			let plusone = type_binop ctx op (EConst (Ident v2),p) one p in
			unify ctx get.etype t p;
			l();
			mk (TBlock [
				mk (TVars [v,e.etype,Some e; v2,t,Some get]) (t_void ctx) p;
				mk (TCall (mk (TField (ev,m)) (mk_mono()) p,[plusone])) t p;
				ev2
			]) t p

and type_switch ctx e cases def need_val p =
	let e = type_expr ctx e in
	let t = (if need_val then mk_mono() else t_void ctx) in
	let rec lookup_enum l =
		match l with
		| [] -> None
		| (ECall ((EConst (Type name),p),_),_) :: l
		| (ECall ((EConst (Ident name),p),_),_) :: l
		| (EConst (Ident name),p) :: l
		| (EConst (Type name),p) :: l ->
			(try
				let e = acc_get (type_ident ctx name false p true) p in
				(match e.eexpr with
				| TEnumField (e,_) -> Some (e, List.map (fun (v,_,_) -> v,mk_mono()) e.e_types)
				| _ -> None)
			with
				Error (Custom _,_) -> lookup_enum l)
		| _ ->
			None
	in
	let enum = ref (match follow e.etype with
		| TEnum (e,params) -> Some (e,params)
		| TMono _ -> lookup_enum (List.map fst cases)
		| _ -> None
	) in
	let first = ref true in
	let ecases = ref PMap.empty in
	let type_case e e1 =
		let e1 = type_expr ctx e1 in
		(* this inversion is needed *)
		unify ctx e.etype e1.etype e1.epos;
		CExpr e1
	in
	let cases = List.map (fun (e1,e2) ->
		let locals = save_locals ctx in
		let e1 = (match !enum with
		| Some en -> 
			(try 
				CMatch (type_matching ctx en e1 ecases)
			with
				Error _ when !first ->
					enum := None;
					type_case e e1)
		| None ->
			type_case e e1
		) in
		first := false;
		let e2 = type_expr ctx ~need_val e2 in
		locals();
		if need_val then unify ctx e2.etype t e2.epos;
		(e1,e2)
	) cases in
	let def = (match def with
		| None ->
		(match !enum with
			| None -> ()
			| Some (e,_) ->
				let l = PMap.fold (fun c acc ->
					if PMap.mem c.ef_name (!ecases) then acc else c.ef_name :: acc
				) e.e_constrs [] in
				match l with
				| [] -> ()
				| _ -> display_error ctx ("Some constructors are not matched : " ^ String.concat "," l) p
			);
			if need_val then Some (null p) else None
		| Some e ->
			let e = type_expr ctx ~need_val e in
			if need_val then unify ctx e.etype t e.epos;
			Some e
	) in
	match !enum with
	| None ->
		let exprs (c,e) =
			match c with
			| CExpr c -> c , e
			| _ -> assert false
		in
		mk (TSwitch (e,List.map exprs cases,def)) t p
	| Some (en,enparams) ->
		let has_params = ref false in
		let matchs (c,e) =
			match c with
			| CMatch (c,p) ->
				if p <> None then has_params := true;
				(c,p,e)
			| _ -> assert false
		in
		let constructs (c,_,e) =
			let c = mk (TField (mk (TTypeExpr (TEnumDecl en)) t_dynamic p , c)) (TEnum (en,enparams)) p in
			(c,e)
		in
		let cases = List.map matchs cases in
		match !has_params with
		| true -> mk (TMatch (e,(en,enparams),cases,def)) t p
		| false -> mk (TSwitch (e,List.map constructs cases,def)) t p

and type_access ctx e p get =
	match e with
	| EConst (Ident s) ->
		type_ident ctx s false p get
	| EConst (Type s) ->
		type_ident ctx s true p get
	| EField _
	| EType _ ->
		let fields path e =
			List.fold_left (fun e (f,_,p) ->
				let e = acc_get (e true) p in
				type_field ctx e f p
			) e path
		in
		let type_path path =
			let rec loop acc path =
				match path with
				| [] ->
					(match List.rev acc with
					| [] -> assert false
					| (name,flag,p) :: path ->
						try
							fields path (type_access ctx (EConst (if flag then Type name else Ident name)) p)
						with
							Error (Unknown_ident _,p2) as e when p = p2 ->
								try
									let path = ref [] in
									let name , _ , _ = List.find (fun (name,flag,p) ->
										if flag then
											true
										else begin
											path := name :: !path;
											false
										end
									) (List.rev acc) in
									raise (Error (Module_not_found (List.rev !path,name),p))
								with
									Not_found -> raise e)
				| (_,false,_) as x :: path ->
					loop (x :: acc) path
				| (name,true,p) as x :: path ->
					let pack = List.rev_map (fun (x,_,_) -> x) acc in
					try
						let e = type_type ctx (pack,name) p in
						fields path (fun _ -> AccExpr e)
					with
						Error (Module_not_found m,_) when m = (pack,name) ->
							loop ((List.rev path) @ x :: acc) []
			in
			match path with
			| [] -> assert false
			| (name,_,p) :: pnext ->
				try
					fields pnext (fun _ -> AccExpr (type_local ctx name p))
				with
					Not_found -> loop [] path
		in
		let rec loop acc e =
			match fst e with
			| EField (e,s) ->
				loop ((s,false,p) :: acc) e
			| EType (e,s) ->
				loop ((s,true,p) :: acc) e
			| EConst (Ident i) ->
				type_path ((i,false,p) :: acc)
			| EConst (Type i) ->
				type_path ((i,true,p) :: acc)
			| _ ->
				fields acc (type_access ctx (fst e) (snd e))
		in
		loop [] (e,p) get
	| EArray (e1,e2) ->
		let e1 = type_expr ctx e1 in
		let e2 = type_expr ctx e2 in
		unify ctx e2.etype (t_int ctx) e2.epos;
		let pt = (try
			let t , pt = t_array ctx VNo in
			unify_raise ctx e1.etype t e1.epos;
			pt
		with Error (Unify _,_) -> try
			let t , pt = t_array ctx (if get then VCo else VContra) in
			unify_raise ctx e1.etype t e1.epos;
			pt
		with Error (Unify _,_) ->
			let t, pt = t_array_access ctx (if get then VCo else VContra) in
			unify ctx e1.etype t e1.epos;
			pt
		) in
		AccExpr (mk (TArray (e1,e2)) pt p)
	| _ ->
		AccExpr (type_expr ctx (e,p))

and type_expr ctx ?(need_val=true) (e,p) =
	match e with
	| EField _
	| EType _
	| EArray _
	| EConst (Ident _)
	| EConst (Type _) ->
		acc_get (type_access ctx e p true) p
	| EConst c ->
		type_constant ctx c p
    | EBinop (op,e1,e2) ->
		type_binop ctx op e1 e2 p
	| EBlock l ->
		let locals = save_locals ctx in
		let rec loop = function
			| [] -> []
			| [e] ->
				(try
					[type_expr ctx ~need_val e]
				with
					Error (e,p) -> ctx.error e p; [])
			| e :: l ->
				try
					let e = type_expr ctx ~need_val:false e in
					e :: loop l
				with
					Error (e,p) -> ctx.error e p; loop l
		in
		let l = loop l in
		locals();
		let rec loop = function
			| [] -> t_void ctx
			| [e] -> e.etype
			| _ :: l -> loop l
		in
		mk (TBlock l) (loop l) p
	| EParenthesis e ->
		let e = type_expr ctx ~need_val e in
		mk (TParenthesis e) e.etype p
	| EObjectDecl fl ->
		let rec loop (l,acc) (f,e) =
			if PMap.mem f acc then error ("Duplicate field in object declaration : " ^ f) p;
			let e = type_expr ctx e in
			let cf = mk_field f e.etype in
			((f,e) :: l, PMap.add f cf acc)
		in
		let fields , types = List.fold_left loop ([],PMap.empty) fl in
		mk (TObjectDecl fields) (mk_anon types) p
	| EArrayDecl el ->
		let t , pt = t_array ctx VNo in
		let dyn = ref ctx.untyped in
		let el = List.map (fun e ->
			let e = type_expr ctx e in
			if not (!dyn) then (try
				unify_raise ctx e.etype pt e.epos;
			with
				Error (Unify _,_) -> dyn := true);
			e
		) el in
		let t = if !dyn then begin
			let t , pt = t_array ctx VNo in
			(match pt with
			| TMono r -> r := Some t_dynamic;
			| _ -> assert false);
			t
		end else t in
		mk (TArrayDecl el) t p
	| EVars vl ->
		let vl = List.map (fun (v,t,e) ->
			try
				let t = load_type_opt ctx p t in
				let e = (match e with
					| None -> None
					| Some e ->
						let e = type_expr ctx e in
						unify ctx e.etype t p;
						Some e
				) in
				let v = add_local ctx v t in
				v , t , e
			with
				Error (e,p) ->
					ctx.error e p;
					let t = t_dynamic in
					let v = add_local ctx v t in
					v , t, None
		) vl in
		mk (TVars vl) (t_void ctx) p
	| EFor (i,e1,e2) ->
		let e1 = type_expr ctx e1 in
		let t, pt = t_iterator ctx in
		let e1 = (match follow e1.etype with
		| TAnon _
		| TInst _ ->
			(try
				unify_raise ctx e1.etype t e1.epos;
				e1
			with Error (Unify _,_) ->
				let acc = acc_get (type_field ctx e1 "iterator" e1.epos true) e1.epos in
				match follow acc.etype with
				| TFun ([],it) ->
					unify ctx it t e1.epos;
					mk (TCall (acc,[])) t e1.epos
				| _ ->
					error "The field iterator is not a method" e1.epos
			)
		| _ ->
			unify ctx e1.etype t e1.epos;
			e1
		) in
		let old_loop = ctx.in_loop in
		let old_locals = save_locals ctx in
		let i = add_local ctx i pt in
		ctx.in_loop <- true;
		let e = (match e1.eexpr with
		| TNew ({ cl_path = ([],"IntIter") },[],[i1;i2]) ->
			(match i1.eexpr , i2.eexpr with
			| TConst (TInt a), TConst (TInt b) when Int32.compare b a <= 0 ->
				error "Range operate can't iterate backwards" p
			| _ -> ());
			let max = gen_local ctx i2.etype in
			let n = gen_local ctx i1.etype in
			let e2 = type_expr ~need_val:false ctx e2 in
			let block = [
				mk (TVars [i,i1.etype,Some (mk (TLocal n) i1.etype p)]) (t_void ctx) p;
				mk (TUnop (Increment,Prefix,mk (TLocal n) i1.etype p)) i1.etype p;
				e2
			] in
			let ident = mk (TLocal n) i1.etype p in
			mk (TBlock [
				mk (TVars [n,i1.etype,Some i1;max,i2.etype,Some i2]) (t_void ctx) p;
				mk (TWhile (
					mk (TBinop (OpLt, ident, mk (TLocal max) i2.etype p)) (t_bool ctx) p,
					mk (TBlock block) (t_void ctx) p,
					NormalWhile
				)) (t_void ctx) p;
			]) (t_void ctx) p
		| _ ->
			let e2 = type_expr ~need_val:false ctx e2 in
			mk (TFor (i,e1,e2)) (t_void ctx) p
		) in
		ctx.in_loop <- old_loop;
		old_locals();
		e
	| EIf (e,e1,e2) ->
		let e = type_expr ctx e in
		unify ctx e.etype (t_bool ctx) e.epos;
		let e1 = type_expr ctx ~need_val e1 in
		(match e2 with
		| None -> 
			mk (TIf (e,e1,if need_val then Some (null p) else None)) (t_void ctx) p
		| Some e2 ->
			let e2 = type_expr ctx ~need_val e2 in
			let t = if not need_val then t_void ctx else (try
				unify_raise ctx e1.etype e2.etype p;
				e2.etype
			with
				Error (Unify _,_) ->
					unify ctx e2.etype e1.etype p;
					e1.etype
			) in
			mk (TIf (e,e1,Some e2)) t p)
	| EWhile (cond,e,NormalWhile) ->
		let old_loop = ctx.in_loop in
		let cond = type_expr ctx cond in
		unify ctx cond.etype (t_bool ctx) cond.epos;
		ctx.in_loop <- true;
		let e = type_expr ~need_val:false ctx e in
		ctx.in_loop <- old_loop;
		mk (TWhile (cond,e,NormalWhile)) (t_void ctx) p
	| EWhile (cond,e,DoWhile) ->
		let old_loop = ctx.in_loop in
		ctx.in_loop <- true;
		let e = type_expr ~need_val:false ctx e in
		ctx.in_loop <- old_loop;
		let cond = type_expr ctx cond in
		unify ctx cond.etype (t_bool ctx) cond.epos;
		mk (TWhile (cond,e,DoWhile)) (t_void ctx) p
	| ESwitch (e,cases,def) ->
		type_switch ctx e cases def need_val p
	| EReturn e ->
		let e , t = (match e with
			| None ->
				let v = t_void ctx in
				unify ctx v ctx.ret p;
				None , v
			| Some e ->
				let e = type_expr ctx e in
				unify ctx e.etype ctx.ret e.epos;
				Some e , e.etype
		) in
		mk (TReturn e) (mk_mono()) p
	| EBreak ->
		if not ctx.in_loop then error "Break outside loop" p;
		mk TBreak (mk_mono()) p
	| EContinue ->
		if not ctx.in_loop then error "Continue outside loop" p;
		mk TContinue (mk_mono()) p
	| ETry (e1,catches) ->
		let e1 = type_expr ctx ~need_val e1 in
		let catches = List.map (fun (v,t,e) ->
			let t = load_type ctx (pos e) t in
			(match follow t with
			| TInst (_,params) | TEnum (_,params) ->
				List.iter (fun (_,pt) ->
					if pt != t_dynamic then error "Catch class parameter must be Dynamic" p;
				) params;
			| TDynamic _ -> ()
			| _ -> error "Catch type must be a class" p);
			let locals = save_locals ctx in
			let v = add_local ctx v t in
			let e = type_expr ctx ~need_val e in
			locals();
			if need_val then unify ctx e.etype e1.etype e.epos;
			v , t , e
		) catches in
		mk (TTry (e1,catches)) (if not need_val then t_void ctx else e1.etype) p
	| EThrow e ->
		let e = type_expr ctx e in
		mk (TThrow e) (mk_mono()) p
	| ECall ((EConst (Ident "trace"),p),e :: el) ->
		if Plugin.defined "no_traces" then
			mk (TConst TNull) (t_void ctx) p
		else
		let params = (match el with [] -> [] | _ -> ["customParams",(EArrayDecl el , p)]) in
		let infos = mk_infos ctx p params in
		type_expr ctx (ECall ((EField ((EType ((EConst (Ident "haxe"),p),"Log"),p),"trace"),p),[e;EUntyped infos,p]),p)
	| ECall ((EConst (Ident "callback"),p),e :: params) ->
		let e = type_expr ctx e in
		let eparams = List.map (type_expr ctx) params in
		(match follow e.etype with
		| TFun (args,ret) ->
			let rec loop args params eargs =
				match args, params with
				| _ , [] ->
					let k = ref 0 in
					let fun_arg = ("f",false,e.etype) in
					let first_args = List.map (fun t -> incr k; "a" ^ string_of_int !k, false, t) (List.rev eargs) in
					let missing_args = List.map (fun (_,opt,t) -> incr k; "a" ^ string_of_int !k, opt, t) args in
					let vexpr (v,_,t) = mk (TLocal v) t p in
					let func = mk (TFunction {
						tf_args = missing_args;
						tf_type = ret;
						tf_expr = mk (TReturn (Some (
							mk (TCall (vexpr fun_arg,List.map vexpr (first_args @ missing_args))) ret p
						))) ret p;
					}) (TFun (missing_args,ret)) p in
					let func = mk (TFunction {
						tf_args = fun_arg :: first_args;
						tf_type = func.etype;
						tf_expr = mk (TReturn (Some func)) e.etype p;
					}) (TFun (first_args,func.etype)) p in
					mk (TCall (func,e :: eparams)) (TFun (missing_args,ret)) p
				| [], _ -> error "Too many callback arguments" p
				| (_,_,t) :: args , e :: params ->
					unify ctx e.etype t p;
					loop args params (t :: eargs)
			in
			loop args eparams []
		| _ -> error "First parameter of callback is not a function" p);
	| ECall ((EConst (Ident "type"),_),[e]) ->
		let e = type_expr ctx e in
		ctx.warn (s_type (print_context()) e.etype) e.epos;
		e
	| ECall ((EConst (Ident "__unprotect__"),_),[(EConst (String _),_) as e]) ->
		let e = type_expr ctx e in
		if Plugin.defined "flash" then
			mk (TCall (mk (TLocal "__unprotect__") (mk_mono()) p,[e])) e.etype e.epos
		else
			e
	| ECall ((EConst (Ident "super"),sp),el) ->
		let el = List.map (type_expr ctx) el in
		if ctx.in_static || not ctx.in_constructor then error "Cannot call superconstructor outside class constructor" p;
		let el, t = (match ctx.curclass.cl_super with
		| None -> error "Current class does not have a super" p
		| Some (c,params) ->
			let f = (match c.cl_constructor with Some f -> f | None -> error (s_type_path c.cl_path ^ " does not have a constructor") p) in
			let el = (match follow (apply_params c.cl_types params (field_type f)) with
			| TFun (args,_) ->
				unify_call_params ctx (Some "new") el args p;
			| _ ->
				error "Constructor is not a function" p
			) in
			el , TInst (c,params)
		) in
		mk (TCall (mk (TConst TSuper) t sp,el)) (t_void ctx) p
	| ECall (e,el) ->
		(match e with EField ((EConst (Ident "super"),_),_) , _ -> ctx.super_call <- true | _ -> ());
		let e = type_expr ctx e in
		let el = List.map (type_expr ctx) el in
		let el , t = (match follow e.etype with
		| TFun (args,r) ->
			let el = unify_call_params ctx (match e.eexpr with TField (_,f) -> Some f | _ -> None) el args p in
			el , r
		| TMono _ ->
			let t = mk_mono() in
			unify ctx (TFun (List.map (fun e -> "",false,e.etype) el,t)) e.etype e.epos;
			el, t
		| t ->
			el, if t == t_dynamic then
				t_dynamic
			else if ctx.untyped then
				mk_mono()
			else
				error (s_type (print_context()) t ^ " cannot be called") e.epos
		) in
		mk (TCall (e,el)) t p
	| ENew (t,el) ->
		let name = (match t.tpackage with [] -> t.tname | x :: _ -> x) in
		if PMap.mem name ctx.locals then error ("Local variable " ^ name ^ " is preventing usage of this class here") p;
		let t = load_normal_type ctx t p true in
		let el = List.map (type_expr ctx) el in
		let el, c , params , t = (match follow t with
		| TInst (c,params) ->
			let f = (match c.cl_constructor with Some f -> f | None -> error (s_type_path c.cl_path ^ " does not have a constructor") p) in
			if not f.cf_public && not (is_parent c ctx.curclass) && not ctx.untyped then error "Cannot access private constructor" p;
			let el = (match follow (apply_params c.cl_types params (field_type f)) with
			| TFun (args,r) ->
				unify_call_params ctx (Some "new") el args p
			| _ ->
				error "Constructor is not a function" p
			) in
			el , c , params , t
		| _ ->
			error (s_type (print_context()) t ^ " cannot be constructed") p
		) in
		mk (TNew (c,params,el)) t p
	| EUnop (op,flag,e) ->
		type_unop ctx op flag e p
	| EFunction f ->
		let rt = load_type_opt ctx p f.f_type in
		let args = List.map (fun (s,opt,t) -> s , opt, load_type_opt ctx p t) f.f_args in
		let ft = TFun (args,rt) in
		let e , fargs = type_function ctx ft true false f p in
		let f = {
			tf_args = fargs;
			tf_type = rt;
			tf_expr = e;
		} in
		mk (TFunction f) ft p
	| EUntyped e ->
		let old = ctx.untyped in
		ctx.untyped <- true;
		let e = type_expr ctx e in
		ctx.untyped <- old;
		{
			eexpr = e.eexpr;
			etype = mk_mono();
			epos = e.epos;
		}
	| ECast (e,None) ->
		let e = type_expr ctx e in
		{ e with etype = mk_mono() }
	| ECast (e, Some t) ->
		(* // if( Std.is(tmp,T) ) tmp else throw "Class cast error" *)
		let etmp = (EConst (Ident "tmp"),p) in
		let t = load_type ctx (pos e) t in
		let tname = (match follow t with
		| TInst (_,params) | TEnum (_,params) ->
			List.iter (fun (_,pt) ->
				if pt != t_dynamic then error "Cast class parameter must be Dynamic" p;
			) params;
			(match follow t with
			| TInst (c,_) -> c.cl_path
			| TEnum (e,_) -> e.e_path
			| _ -> assert false);
		| _ -> 
			error "Cast type must be a class" p
		) in
		let make_type (path,name) =
			match path with
			| [] -> (EConst (Type name),p)
			| x :: path -> (EType (List.fold_left (fun acc x -> (EField (acc,x),p)) (EConst (Ident x),p) path,name),p)
		in
		let cond = (ECall ((EField ((EConst (Type "Std"),p),"is"),p),[etmp;make_type tname]),p) in
		let e = type_expr ctx (EBlock [
			(EVars [("tmp",None,Some e)],p);
			(EIf (cond,etmp,Some (EThrow (EConst (String "Class cast error"),p),p)),p);
		],p) in
		{ e with etype = t }

and type_function ctx t static constr f p =
	let locals = save_locals ctx in
	let fargs , r = (match t with
		| TFun (args,r) -> List.map (fun (n,opt,t) -> add_local ctx n t, opt, t) args, r
		| _ -> assert false
	) in
	let old_ret = ctx.ret in
	let old_static = ctx.in_static in
	let old_constr = ctx.in_constructor in
	let old_opened = ctx.opened in
	ctx.in_static <- static;
	ctx.in_constructor <- constr;
	ctx.ret <- r;
	ctx.opened <- [];
	let e = type_expr ~need_val:false ctx f.f_expr in
	let rec loop e =
		match e.eexpr with
		| TReturn (Some _) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	let have_ret = (try loop e; false with Exit -> true) in
	if have_ret then
		(try return_flow ctx e with Exit -> ())
	else
		unify ctx r (t_void ctx) p;
	let rec loop e =
		match e.eexpr with
		| TCall ({ eexpr = TConst TSuper },_) -> raise Exit
		| TFunction _ -> ()
		| _ -> Type.iter loop e
	in
	if constr && (match ctx.curclass.cl_super with None -> false | Some (cl,_) -> cl.cl_constructor <> None) then
		(try
			loop e;
			error "Missing super constructor call" p
		with
			Exit -> ());
	locals();
	List.iter (fun r -> r := Closed) ctx.opened;
	ctx.ret <- old_ret;
	ctx.in_static <- old_static;
	ctx.in_constructor <- old_constr;
	ctx.opened <- old_opened;
	e , fargs

let type_static_var ctx t e p =
	ctx.in_static <- true;
	let e = type_expr ctx e in
	unify ctx e.etype t p;
	e

let valid_redefinition ctx f t =
	let ft = field_type f in
	match follow ft , t with
	| TFun (args,r) , TFun (targs,tr) when f.cf_expr <> None && List.length args = List.length targs ->
		List.for_all2 (fun (_,o1,a1) (_,o2,a2) -> o1 = o2 && type_eq false a1 a2) args targs && 
		(try unify_raise ctx r tr null_pos; true with Error (Unify _,_) -> false)
	| _ , _ ->
		type_eq false ft t

let check_overriding ctx c p () =
	match c.cl_super with
	| None -> ()
	| Some (csup,params) ->
		PMap.iter (fun i f ->
			try
				let t , f2 = class_field csup i in
				let t = apply_params csup.cl_types params t in
				ignore(follow f.cf_type); (* force evaluation *)
				let p = (match f.cf_expr with None -> p | Some e -> e.epos) in
				if !check_override && not (List.mem i c.cl_overrides) then
					display_error ctx ("Field " ^ i ^ " should be declared with 'override' since it is inherited from superclass") p
				else if f.cf_public <> f2.cf_public then
					display_error ctx ("Field " ^ i ^ " has different visibility (public/private) than superclass one") p
				else if f2.cf_get <> f.cf_get || f2.cf_set <> f.cf_set then
					display_error ctx ("Field " ^ i ^ " has different property access than in superclass") p
				else if not (valid_redefinition ctx f t) then
					display_error ctx ("Field " ^ i ^ " overload parent class with different or incomplete type") p
			with
				Not_found ->
					if List.mem i c.cl_overrides then display_error ctx ("Field " ^ i ^ " is declared 'override' but doesn't override any field") p
		) c.cl_fields

let class_field_no_interf c i =
	try
		let f = PMap.find i c.cl_fields in
		field_type f , f
	with Not_found ->
		match c.cl_super with
		| None ->
			raise Not_found
		| Some (c,tl) ->
			(* rec over class_field *)
			let t , f = class_field c i in
			apply_params c.cl_types tl t , f

let rec check_interface ctx c p intf params =
	PMap.iter (fun i f ->
		try
			let t , f2 = class_field_no_interf c i in
			ignore(follow f.cf_type); (* force evaluation *)
			let p = (match f.cf_expr with None -> p | Some e -> e.epos) in
			if f.cf_public && not f2.cf_public then
				display_error ctx ("Field " ^ i ^ " should be public as requested by " ^ s_type_path intf.cl_path) p
			else if not(unify_access f2.cf_get f.cf_get) then
				display_error ctx ("Field " ^ i ^ " has different property access than in " ^ s_type_path intf.cl_path) p
			else
				let t1 = apply_params intf.cl_types params (field_type f) in				
				if not (valid_redefinition ctx f2 t1) then
					display_error ctx ("Field " ^ i ^ " has different type than in " ^ s_type_path intf.cl_path) p;
		with
			Not_found ->
				if not c.cl_interface then display_error ctx ("Field " ^ i ^ " needed by " ^ s_type_path intf.cl_path ^ " is missing") p
	) intf.cl_fields;
	List.iter (fun (i2,p2) ->
		check_interface ctx c p i2 (List.map (fun (v,t) -> v, apply_params intf.cl_types params t) p2)
	) intf.cl_implements

let check_interfaces ctx c p () =
	match c.cl_path with
	| "Proxy" :: _ , _ -> ()
	| _ ->
	List.iter (fun (intf,params) -> check_interface ctx c p intf params) c.cl_implements

(* ---------------------------------------------------------------------- *)
(* PASS 1 & 2 : Module and Class Structure *)

let init_class ctx c p herits fields =
	ctx.type_params <- List.map (fun (_,n,t) -> n,t) c.cl_types;
	c.cl_extern <- List.mem HExtern herits;
	c.cl_interface <- List.mem HInterface herits;
	set_heritance ctx c herits p;
	let tthis = TInst (c,List.map (fun (v,_,t) -> v,t) c.cl_types) in
	let is_public access =
		if c.cl_extern || c.cl_interface then not (List.mem APrivate access) else List.mem APublic access
	in
	let type_opt ctx p t =
		match t with
		| None when c.cl_extern || c.cl_interface ->
			display_error ctx "Type required for extern classes and interfaces" p;
			t_dynamic
		| _ ->
			load_type_opt ctx p t
	in
	let rec has_field f = function
		| None -> false
		| Some (c,_) ->
			PMap.exists f c.cl_fields || has_field f c.cl_super || List.exists (fun i -> has_field f (Some i)) c.cl_implements
	in
	let loop_cf f p =
		match f with
		| FVar (name,doc,access,t,e) ->
			let stat = List.mem AStatic access in
			if not stat && has_field name c.cl_super then error ("Redefinition of variable " ^ name ^ " in subclass is not allowed") p;
			let t = (match t with
				| None ->
					if not stat then error ("Type required for member variable " ^ name) p;
					mk_mono()
				| Some t ->
					let old = ctx.type_params in
					if stat then ctx.type_params <- [];
					let t = load_type ctx p t in
					if stat then ctx.type_params <- old;
					t
			) in
			let cf = {
				cf_name = name;
				cf_doc = doc;
				cf_type = t;
				cf_get = NormalAccess;
				cf_set = NormalAccess;
				cf_expr = None;
				cf_public = is_public access;
				cf_params = [];
			} in
			let delay = (match e with
				| None -> (fun() -> ())
				| Some e ->
					let ctx = { ctx with curclass = c; tthis = tthis } in
					let r = exc_protect (fun r ->
						r := (fun() -> t);
						if !Plugin.verbose then print_endline ("Typing " ^ s_type_path c.cl_path ^ "." ^ name);
						cf.cf_expr <- Some (type_static_var ctx t e p);
						t
					) in
					cf.cf_type <- TLazy r;
					(fun () -> ignore(!r()))
			) in
			access, false, cf, delay
		| FFun (name,doc,access,params,f) ->
			let params = List.map (fun (v,n,flags) ->
				match flags with
				| [] ->
					let _, n, t = type_type_params ctx c.cl_path p (v,n,[]) in
					n, t
				| _ -> error "This notation is not allowed because it can't be checked" p
			) params in
			let ctx = { ctx with
				curclass = c;
				curmethod = name;
				tthis = tthis;
				type_params = params @ ctx.type_params;
			} in
			let ret = type_opt ctx p f.f_type in
			let args = List.map (fun (name,opt,t) -> name , opt, type_opt ctx p t) f.f_args in
			let t = TFun (args,ret) in
			let stat = List.mem AStatic access in
			let constr = (name = "new") in
			let cf = {
				cf_name = name;
				cf_doc = doc;
				cf_type = t;
				cf_get = NormalAccess;
				cf_set = (if ctx.flash9 && not (List.mem AF9Dynamic access) then F9MethodAccess else NormalAccess);
				cf_expr = None;
				cf_public = is_public access;
				cf_params = params;
			} in
			let r = exc_protect (fun r ->
				r := (fun() -> t);
				if !Plugin.verbose then print_endline ("Typing " ^ s_type_path c.cl_path ^ "." ^ name);
				let e , fargs = type_function ctx t stat constr f p in
				let f = {
					tf_args = fargs;
					tf_type = ret;
					tf_expr = e;
				} in
				if stat && name = "__init__" then c.cl_init <- Some e;
				cf.cf_expr <- Some (mk (TFunction f) t p);
				t
			) in
			let delay = (
				if (c.cl_extern || c.cl_interface || ctx.isproxy) && cf.cf_name <> "__init__" then
					(fun() -> ())
				else begin
					cf.cf_type <- TLazy r;
					(fun() -> ignore((!r)()))
				end
			) in
			access, constr, cf, delay
		| FProp (name,doc,access,get,set,t) ->
			let ret = load_type ctx p t in
			let check_get = ref (fun() -> ()) in
			let check_set = ref (fun() -> ()) in
			let check_method m t () =
				try
					let t2 = (if List.mem AStatic access then (PMap.find m c.cl_statics).cf_type else fst (class_field c m)) in
					unify_raise ctx t2 t p;
				with
					| Error (Unify l,_) -> raise (Error (Stack (Custom ("In method " ^ m ^ " required by property " ^ name),Unify l),p))
					| Not_found -> if not c.cl_interface then error ("Method " ^ m ^ " required by property " ^ name ^ " is missing") p
			in
			let get = (match get with
				| "null" -> NoAccess
				| "dynamic" -> MethodAccess ("get_" ^ name)
				| "default" -> NormalAccess
				| _ ->
					check_get := check_method get (TFun ([],ret));
					MethodAccess get
			) in
			let set = (match set with
				| "null" -> NoAccess
				| "dynamic" -> MethodAccess ("set_" ^ name)
				| "default" -> NormalAccess
				| _ ->
					check_set := check_method set (TFun (["",false,ret],ret));
					MethodAccess set
			) in
			if set = NormalAccess && (match get with MethodAccess _ -> true | _ -> false) then error "Unsupported property combination" p;
			let cf = {
				cf_name = name;
				cf_doc = doc;
				cf_get = get;
				cf_set = set;
				cf_expr = None;
				cf_type = ret;
				cf_public = is_public access;
				cf_params = [];
			} in
			access, false, cf, (fun() -> (!check_get)(); (!check_set)())
	in
	let fl = List.map (fun (f,p) ->
		let access , constr, f , delayed = loop_cf f p in
		let is_static = List.mem AStatic access in
		if is_static && f.cf_name = "name" && Plugin.defined "js" then error "This identifier cannot be used in Javascript for statics" p;
		if constr then begin
			if c.cl_constructor <> None then error "Duplicate constructor" p;
			c.cl_constructor <- Some f;
		end else if not is_static || f.cf_name <> "__init__" then begin
			if PMap.mem f.cf_name (if is_static then c.cl_statics else c.cl_fields) then error ("Duplicate class field declaration : " ^ f.cf_name) p;
			if is_static then begin
				c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
				c.cl_ordered_statics <- f :: c.cl_ordered_statics;
			end else begin
				c.cl_fields <- PMap.add f.cf_name f c.cl_fields;
				if List.mem AOverride access then c.cl_overrides <- f.cf_name :: c.cl_overrides;
			end;
		end;
		delayed
	) fields in
	c.cl_ordered_statics <- List.rev c.cl_ordered_statics;
	(* define an default inherited constructor *)
	(match c.cl_constructor, c.cl_super with
	| None , Some ({ cl_constructor = Some f; cl_types = tl } as csuper, cparams) ->
		let t = apply_params tl cparams (field_type f) in
		(match follow t with
		| TFun (args,r) ->
			let n = ref 0 in
			let args = List.map (fun (_,b,t) -> incr n; "p" ^ string_of_int (!n) , b, t) args in
			let eargs = List.map (fun (n,_,t) -> mk (TLocal n) t p) args in
			let func = {
				tf_args = args;
				tf_type = t;
				tf_expr = mk (TCall (mk (TConst TSuper) (TInst (csuper,cparams)) p,eargs)) r p;
			} in
			c.cl_constructor <- Some {
				cf_name = "new";
				cf_type = t;
				cf_get = NormalAccess;
				cf_set = NoAccess;
				cf_doc = None;
				cf_expr = Some (mk (TFunction func) t p);
				cf_public = f.cf_public;
				cf_params = f.cf_params;
			}
		| _ -> assert false)
	| _ , _ ->
		());
	fl

let type_module ctx m tdecls loadp =
	(* PASS 1 : build module structure - does not load any module or type - should be atomic ! *)
	let decls = ref [] in
	let decl_with_name name p priv =
		let tpath = if priv then (fst m @ ["_" ^ snd m], name) else (fst m, name) in
		if priv then begin
			if List.exists (fun t -> tpath = t_path t) (!decls) then error ("Type name " ^ name ^ " is alreday defined in this module") p;
			tpath
		end else try
			let m2 = Hashtbl.find ctx.types tpath in
			if String.lowercase (s_type_path m2) = String.lowercase (s_type_path m) then error ("Module " ^ s_type_path m2 ^ " is loaded with a different case than " ^ s_type_path m) loadp;
			error ("Type name " ^ s_type_path tpath ^ " is redefined from module " ^ s_type_path m2) p
		with
			Not_found ->
				Hashtbl.add ctx.types (fst m,name) m;
				tpath
	in
	List.iter (fun (d,p) ->
		match d with
		| EImport _ -> ()
		| EClass d ->
			let priv = List.mem HPrivate d.d_flags in
			let path = decl_with_name d.d_name p priv in
			let c = mk_class path p d.d_doc priv in
			decls := TClassDecl c :: !decls
		| EEnum d ->
			let priv = List.mem EPrivate d.d_flags in
			let path = decl_with_name d.d_name p priv in
			let e = {
				e_path = path;
				e_pos = p;
				e_doc = d.d_doc;
				e_types = [];
				e_private = priv;
				e_extern = List.mem EExtern d.d_flags || d.d_data = [];
				e_constrs = PMap.empty;
			} in
			decls := TEnumDecl e :: !decls
		| ETypedef d ->
			let priv = List.mem EPrivate d.d_flags in
			let path = decl_with_name d.d_name p priv in
			let t = {
				t_path = path;
				t_pos = p;
				t_doc = d.d_doc;
				t_private = priv;
				t_types = [];
				t_static = None;
				t_type = mk_mono();
			} in
			decls := TTypeDecl t :: !decls
	) tdecls;
	let m = {
		mpath = m;
		mtypes = List.rev !decls;
		mimports = [];
	} in
	Hashtbl.add ctx.modules m.mpath m;
	(* PASS 2 : build types structure - does not type any expression ! *)
	let ctx = {
		modules = ctx.modules;
		delays = ctx.delays;
		types = ctx.types;
		warn = ctx.warn;
		error = ctx.error;
		curclass = ctx.curclass;
		tthis = ctx.tthis;
		std = ctx.std;
		ret = ctx.ret;
		isproxy = ctx.isproxy;
		flash9 = ctx.flash9;
		current = m;
		locals = PMap.empty;
		locals_map = PMap.empty;
		locals_map_inv = PMap.empty;
		local_types = ctx.std.mtypes @ m.mtypes;
		type_params = [];
		curmethod = "";
		super_call = false;
		in_constructor = false;
		in_static = false;
		in_loop = false;
		untyped = false;
		opened = [];
	} in
	let delays = ref [] in
	let get_class name =
		let c = List.find (fun d -> match d with TClassDecl { cl_path = _ , n } -> n = name | _ -> false) m.mtypes in
		match c with TClassDecl c -> c | _ -> assert false
	in
	let get_enum name =
		let e = List.find (fun d -> match d with TEnumDecl { e_path = _ , n } -> n = name | _ -> false) m.mtypes in
		match e with TEnumDecl e -> e | _ -> assert false
	in
	let get_tdef name =
		let s = List.find (fun d -> match d with TTypeDecl { t_path = _ , n } -> n = name | _ -> false) m.mtypes in
		match s with TTypeDecl s -> s | _ -> assert false
	in
	(* here is an additional PASS 1 phase, which handle the type parameters declaration, with lazy contraints *)
	List.iter (fun (d,p) ->
		match d with
		| EImport _ -> ()
		| EClass d ->
			let c = get_class d.d_name in
			c.cl_types <- List.map (type_type_params ctx c.cl_path p) d.d_params;
		| EEnum d ->
			let e = get_enum d.d_name in
			e.e_types <- List.map (type_type_params ctx e.e_path p) d.d_params;
		| ETypedef d ->
			let t = get_tdef d.d_name in
			t.t_types <- List.map (type_type_params ctx t.t_path p) d.d_params;
	) tdecls;
	(* back to PASS2 *)
	List.iter (fun (d,p) ->
		match d with
		| EImport (pack,name,topt) ->
			let md = load ctx (pack,name) p in
			let types = List.filter (fun t -> not (t_private t)) md.mtypes in
			(match topt with
			| None -> ctx.local_types <- ctx.local_types @ types
			| Some t ->
				try
					let t = List.find (fun tdecl -> snd (t_path tdecl) = t) types in
					ctx.local_types <- ctx.local_types @ [t]
				with
					Not_found -> error ("Module " ^ s_type_path (pack,name) ^ " does not define type " ^ name) p
			);
			m.mimports <- (md,topt) :: m.mimports;
		| EClass d ->
			let c = get_class d.d_name in
			delays := !delays @ check_overriding ctx c p :: check_interfaces ctx c p :: init_class ctx c p d.d_flags d.d_data
		| EEnum d ->
			let e = get_enum d.d_name in
			ctx.type_params <- List.map (fun (_,n,t) -> n, t) e.e_types;
			let et = TEnum (e,List.map (fun (v,_,t) -> v ,t) e.e_types) in
			List.iter (fun (c,doc,t,p) ->
				if c = "name" && Plugin.defined "js" then error "This identifier cannot be used in Javascript" p;
				let t = (match t with
					| [] -> et
					| l -> TFun (List.map (fun (s,b,t) -> s, b, load_type ctx p t) l, et)
				) in
				e.e_constrs <- PMap.add c { ef_name = c; ef_type = t; ef_pos = p; ef_doc = doc } e.e_constrs
			) d.d_data
		| ETypedef d ->
			let t = get_tdef d.d_name in
			ctx.type_params <- List.map (fun (_,n,t) -> n, t) t.t_types;
			let tt = load_type ctx p d.d_data in
			(match t.t_type with
			| TMono r ->
				(match !r with
				| None -> r := Some tt;
				| Some _ -> assert false);
			| _ -> assert false);			
	) tdecls;
	(* PASS 3 : type checking, delayed until all modules and types are built *)
	ctx.delays := !delays :: !(ctx.delays);
	m.mimports <- List.rev m.mimports;
	m

let rec f9path p = {
	tpackage = (match p.tpackage with "flash" :: l -> "flash9" :: l | l -> l);
	tname = p.tname;
	tparams = List.map (fun c -> 
		match c with
		| TPConst _ -> c
		| TPType (v,t) -> TPType (v,f9t t)
	) p.tparams;
}

and f9t = function
	| TPNormal t -> TPNormal (f9path t)
	| TPFunction (tl,t) -> TPFunction (List.map f9t tl,f9t t)
	| TPAnonymous fields -> TPAnonymous (List.map f9a fields)
	| TPParent t -> TPParent (f9t t)
	| TPExtend (t,fields) -> TPExtend (f9path t,List.map f9a fields)

and f9a (name,pub,f,p) =
	name , pub, (match f with
		| AFVar t -> AFVar (f9t t)
		| AFProp (t,g,s) -> AFProp (f9t t,g,s)
		| AFFun (pl,t) -> AFFun (List.map (fun (name,p,t) -> name, p, f9t t) pl,f9t t)
	) , p

let f9to = function
	| None -> None
	| Some t -> Some (f9t t)

let f9decl (d,p) =
	(match d with
	| EClass d ->
		EClass {
			d with
			d_flags = List.map (function
				| HInterface
				| HExtern
				| HPrivate as f -> f
				| HExtends p -> HExtends (f9path p)
				| HImplements p -> HImplements (f9path p)
			) d.d_flags;
			d_data = List.map (fun (f,p) ->
				(match f with
				| FVar (name,doc,acc,t,e) ->
					FVar (name,doc,acc,f9to t,e)
				| FFun (name,doc,acc,params,f) ->
					FFun (name,doc,acc,params,{
						f_args = List.map (fun (n,o,t) -> n , o, f9to t) f.f_args;
						f_type = f9to f.f_type;
						f_expr = f.f_expr;
					})
				| FProp (name,doc,acc,get,set,t) ->
					FProp (name,doc,acc,get,set,f9t t)
				) , p
			) d.d_data
		}
	| EEnum d ->
		EEnum {
			d with
			d_data = List.map (fun (name,doc,args,p) ->
				name, doc, List.map (fun (name,p,t) -> name, p, f9t t) args, p
			) d.d_data
		}
	| ETypedef d ->
		ETypedef { d with d_data = f9t d.d_data }
	| EImport ("flash" :: l,x,o) ->
		EImport ("flash9" :: l,x,o)
	| EImport _ ->
		d
	) , p

let load ctx m p =
	try
		Hashtbl.find ctx.modules m
	with
		Not_found ->
			let file = (match m with
				| [] , name -> name
				| x :: l , name ->
					if List.mem x (!forbidden_packages) then error ("You can't access the " ^ x ^ " package with current compilation flags") p;
					let x = (match x with "flash" when ctx.flash9 -> "flash9" | _ -> x) in
					String.concat "/" (x :: l) ^ "/" ^ name
			) ^ ".hx" in
			let file = (try Plugin.find_file file with Not_found -> raise (Error (Module_not_found m,p))) in
			let ch = (try open_in file with _ -> error ("Could not open " ^ file) p) in
			let pack , decls = (try Parser.parse (Lexing.from_channel ch) file with e -> close_in ch; raise e) in
			let pack , decls = (match pack , fst m with "flash" :: l , "flash9" :: l2 when l = l2 && Plugin.defined "flash9doc" -> fst m, List.map f9decl decls | _ -> pack , decls) in
			close_in ch;
			if !Plugin.verbose then print_endline ("Parsed " ^ file);
			if pack <> fst m then begin
				let spack m = if m = [] then "<empty>" else String.concat "." m in
				if p == Ast.null_pos then
					error ("Invalid commandline class : " ^ s_type_path m ^ " should be " ^ s_type_path (pack,snd m)) p
				else
					error ("Invalid package : " ^ spack (fst m) ^ " should be " ^ spack pack) p
			end;
			type_module ctx m decls p

let rec finalize ctx =
	let delays = List.concat !(ctx.delays) in
	ctx.delays := [];
	match delays with
	| [] -> () (* at last done *)
	| l ->
		List.iter (fun f -> f()) l;
		finalize ctx

let module_of_type ctx t =
	let mfound = ref ctx.current in
	try
		Hashtbl.iter (fun _ m ->
			if List.mem t m.mtypes then begin
				mfound := m;
				raise Exit;
			end;
		) ctx.modules;
		(* @Main, other generated classes ? *)
		{
			mtypes = [t];
			mpath = t_path t;
			mimports = [];
		}
	with
		Exit -> !mfound

type state =
	| Generating
	| Done
	| NotYet

let rec has_rtti c =
	List.exists (function (t,pl) -> 
		match t, pl with 
		| { cl_path = ["haxe";"rtti"],"Infos" },[] -> true
		| _ -> false
	) c.cl_implements || (match c.cl_super with None -> false | Some (c,_) -> has_rtti c)

let types ctx main excludes =
	let types = ref [] in
	let states = Hashtbl.create 0 in
	let state p = try Hashtbl.find states p with Not_found -> NotYet in
	let statics = ref PMap.empty in

	let rec loop t =
		let p = t_path t in
		match state p with
		| Done -> ()
		| Generating ->
			prerr_endline ("Warning : maybe loop in static generation of " ^ s_type_path p);
		| NotYet ->
			Hashtbl.add states p Generating;
			let t = (match t with
			| TClassDecl c ->
				walk_class p c;
				if List.mem c.cl_path excludes then begin
					c.cl_extern <- true;
					c.cl_init <- None;
				end;				
				if has_rtti c then begin
					let f = mk_field "__rtti" (t_string ctx) in
					let str = (!generate_meta_data) ctx t in
					f.cf_expr <- Some (mk (TConst (TString str)) f.cf_type c.cl_pos);
					c.cl_ordered_statics <- f :: c.cl_ordered_statics;
					c.cl_statics <- PMap.add f.cf_name f c.cl_statics;
				end;
				t
			| TEnumDecl _ | TTypeDecl _ ->
				t
			) in
			Hashtbl.replace states p Done;
			types := t :: !types

    and loop_class p c =
		if c.cl_path <> p then loop (TClassDecl c)

	and loop_enum p e =
		if e.e_path <> p then loop (TEnumDecl e)

	and walk_static_call p c name =
		try
			let f = PMap.find name c.cl_statics in
			match f.cf_expr with
			| None -> ()
			| Some e ->
				if PMap.mem (c.cl_path,name) (!statics) then
					()
				else begin
					statics := PMap.add (c.cl_path,name) () (!statics);
					walk_expr p e;
				end
		with
			Not_found -> ()

	and walk_expr p e =
		match e.eexpr with
		| TTypeExpr t ->
			(match t with
			| TClassDecl c -> loop_class p c
			| TEnumDecl e -> loop_enum p e
			| TTypeDecl _ -> assert false)
		| TEnumField (e,_) ->
			loop_enum p e
		| TNew (c,_,_) ->
			iter (walk_expr p) e;
			loop_class p c
		| TMatch (_,(enum,_),_,_) ->
			loop_enum p enum;
			iter (walk_expr p) e
		| TCall (f,_) ->
			iter (walk_expr p) e;
			(* static call for initializing a variable *)
			let rec loop f =
				match f.eexpr with
				| TField ({ eexpr = TTypeExpr t },name) ->
					(match t with
					| TEnumDecl _ -> ()
					| TTypeDecl _ -> assert false
					| TClassDecl c -> walk_static_call p c name)
				| _ -> ()
			in
			loop f
		| _ ->
			iter (walk_expr p) e

    and walk_class p c =
		(match c.cl_super with None -> () | Some (c,_) -> loop_class p c);
		List.iter (fun (c,_) -> loop_class p c) c.cl_implements;
		(match c.cl_init with
		| None -> ()
		| Some e -> walk_expr p e);
		PMap.iter (fun _ f ->
			match f.cf_expr with
			| None -> ()
			| Some e ->
				match e.eexpr with
				| TFunction _ -> ()
				| _ -> walk_expr p e
		) c.cl_statics

	in
	Hashtbl.iter (fun _ m -> List.iter loop m.mtypes) ctx.modules;
	(match main with
	| None -> ()
	| Some cl ->
		let t = load_type_def ctx null_pos cl in
		let cmain = (match t with
		| TEnumDecl _ | TTypeDecl _ ->
			error ("Invalid -main : " ^ s_type_path cl ^ " is not a class") null_pos
		| TClassDecl c ->
			try
				let f = PMap.find "main" c.cl_statics in
				(match follow (field_type f) with
				| TFun ([],_) -> ()
				| _ -> error ("Invalid -main : " ^ s_type_path cl ^ " has invalid main function") null_pos);
				c
			with
				Not_found -> error ("Invalid -main : " ^ s_type_path cl ^ " does not have static function main") null_pos
		) in
		let path = ([],"@Main") in
		let tmain = TInst (cmain,List.map (fun (v,_,t) -> v,t) cmain.cl_types) in
		let c = mk_class path null_pos None false in
		let f = {
			cf_name = "init";
			cf_type = mk_mono();
			cf_public = false;
			cf_get = NormalAccess;
			cf_set = NormalAccess;
			cf_doc = None;
			cf_params = [];
			cf_expr = Some (mk (TCall (mk (TField (mk (TTypeExpr t) tmain null_pos,"main")) (mk_mono()) null_pos,[])) (mk_mono()) null_pos);
		} in
		c.cl_statics <- PMap.add "init" f c.cl_statics;
		c.cl_ordered_statics <- f :: c.cl_ordered_statics;
		types := TClassDecl c :: !types
	);
	List.rev !types

;;
load_ref := load;
type_expr_ref := type_expr;
type_module_ref := type_module;

