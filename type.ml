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
 
type module_path = string list * string

type field_access =
	| NormalAccess
	| NoAccess
	| MethodAccess of string

type t = 
	| TMono of t option ref
	| TEnum of tenum * t list
	| TInst of tclass * t list
	| TFun of (string * t) list * t
	| TAnon of (string, tclass_field) PMap.t * string option
	| TDynamic of t
	| TLazy of (unit -> t) ref

and tconstant =
	| TInt of string
	| TFloat of string
	| TString of string
	| TBool of bool
	| TNull
	| TThis
	| TSuper

and tfunc = {
	tf_args : (string * t) list;
	tf_type : t;
	tf_expr : texpr;
}

and texpr_expr =
	| TConst of tconstant
	| TLocal of string
	| TEnumField of tenum * string
	| TArray of texpr * texpr
	| TBinop of Ast.binop * texpr * texpr
	| TField of texpr * string
	| TType of module_type
	| TParenthesis of texpr
	| TObjectDecl of (string * texpr) list
	| TArrayDecl of texpr list
	| TCall of texpr * texpr list
	| TNew of tclass * t list * texpr list
	| TUnop of Ast.unop * Ast.unop_flag * texpr
	| TFunction of tfunc
	| TVars of (string * t * texpr option) list
	| TBlock of texpr list
	| TFor of string * texpr * texpr
	| TIf of texpr * texpr * texpr option
	| TWhile of texpr * texpr * Ast.while_flag
	| TSwitch of texpr * (texpr * texpr) list * texpr option
	| TMatch of texpr * (tenum * t list) * (string * (string * t) list option * texpr) list * texpr option
	| TTry of texpr * (string * t * texpr) list
	| TReturn of texpr option
	| TBreak
	| TContinue
	| TThrow of texpr

and texpr = {
	eexpr : texpr_expr;
	etype : t;
	epos : Ast.pos;
}

and tclass_field = {
	cf_name : string;
	mutable cf_type : t;
	cf_public : bool;
	cf_doc : Ast.documentation;
	cf_get : field_access;
	cf_set : field_access;
	mutable cf_expr : texpr option;
}

and tclass = {
	cl_path : module_path;
	cl_pos : Ast.pos;
	cl_doc : Ast.documentation;
	cl_private : bool;
	mutable cl_extern : bool;
	mutable cl_interface : bool;
	mutable cl_locked : bool;
	mutable cl_types : (string * t) list;
	mutable cl_super : (tclass * t list) option;
	mutable cl_implements : (tclass * t list) list;
	mutable cl_fields : (string , tclass_field) PMap.t;
	mutable cl_statics : (string, tclass_field) PMap.t;
	mutable cl_ordered_statics : tclass_field list;
	mutable cl_dynamic : t option;
	mutable cl_constructor : tclass_field option;
}

and tenum_field = {
	ef_name : string;
	ef_type : t;
	ef_pos : Ast.pos;
	ef_doc : Ast.documentation;
}

and tenum = {
	e_path : module_path;
	e_pos : Ast.pos;
	e_doc : Ast.documentation;
	e_private : bool;
	mutable e_types : (string * t) list;
	mutable e_constrs : (string , tenum_field) PMap.t;
}

and module_type = 
	| TClassDecl of tclass
	| TEnumDecl of tenum

type module_def = {
	mpath : module_path;		
	mtypes : module_type list;
}

let mk e t p = { eexpr = e; etype = t; epos = p }

let mk_mono() = TMono (ref None)

let rec t_dynamic = TDynamic t_dynamic

let mk_class path pos doc priv =
	{
		cl_path = path;
		cl_pos = pos;
		cl_doc = doc;
		cl_private = priv;
		cl_extern = false;
		cl_interface = false;
		cl_locked = false;
		cl_types = [];
		cl_super = None;
		cl_implements = [];
		cl_fields = PMap.empty;
		cl_ordered_statics = [];
		cl_statics = PMap.empty;
		cl_dynamic = None;
		cl_constructor = None;
	}

let null_class = mk_class ([],"") Ast.null_pos None true

let t_private = function
	| TClassDecl c -> c.cl_private
	| TEnumDecl  e -> e.e_private

let t_path = function
	| TClassDecl c -> c.cl_path
	| TEnumDecl  e -> e.e_path

let print_context() = ref []

let rec s_type ctx t = 
	match t with
	| TMono r ->
		(match !r with
		| None -> Printf.sprintf "Unknown<%d>" (try List.assq t (!ctx) with Not_found -> let n = List.length !ctx in ctx := (t,n) :: !ctx; n)
		| Some t -> s_type ctx t)
	| TEnum (e,tl) ->
		Ast.s_type_path e.e_path ^ s_type_params ctx tl
	| TInst (c,tl) ->
		Ast.s_type_path c.cl_path ^ s_type_params ctx tl
	| TFun ([],t) ->
		"Void -> " ^ s_type ctx t
	| TFun (l,t) ->
		String.concat " -> " (List.map (fun (s,t) -> (if s = "" then "" else s ^ " : ") ^ match t with TFun _ -> "(" ^ s_type ctx t ^ ")" | _ -> s_type ctx t) l) ^ " -> " ^ s_type ctx t
	| TAnon (fl,name) ->
		(match name with
		| Some s -> s
		| None ->
			let fl = PMap.fold (fun f acc -> (" " ^ f.cf_name ^ " : " ^ s_type ctx f.cf_type) :: acc) fl [] in
			"{" ^ String.concat "," fl ^ " }");
	| TDynamic t2 ->
		"Dynamic" ^ s_type_params ctx (if t == t2 then [] else [t2])
	| TLazy f ->		
		s_type ctx (!f())

and s_type_params ctx = function
	| [] -> ""
	| l -> "<" ^ String.concat ", " (List.map (s_type ctx) l) ^ ">"

let type_path t =
	match t with 
	| TClassDecl c -> c.cl_path
	| TEnumDecl e -> e.e_path

let rec follow t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow t
		| _ -> t)
	| TLazy f ->		
		follow (!f())
	| _ -> t

let rec is_parent csup c =
	if c == csup then
		true
	else match c.cl_super with
		| None -> false
		| Some (c,_) -> is_parent csup c

let rec link e a b =
	let rec loop t =
		if t == a then
			true
		else match t with
		| TMono t -> (match !t with None -> false | Some t -> loop t)
		| TEnum (_,tl) -> List.exists loop tl
		| TInst (_,tl) -> List.exists loop tl
		| TFun (tl,t) -> List.exists (fun (_,t) -> loop t) tl || loop t
		| TDynamic t2 ->
			if t == t2 then
				false
			else
				loop t2
		| TLazy f ->
			loop (!f())
		| TAnon (fl,_) ->
			try
				PMap.iter (fun _ f -> if loop f.cf_type then raise Exit) fl;
				false
			with
				Exit -> true
	in
	if loop b then
		false
	else begin
		e := Some b;
		true
	end

(* substitute parameters with other types *)
let apply_params cparams params t =
	let rec loop l1 l2 =
		match l1, l2 with
		| [] , [] -> []
		| (_,t1) :: l1 , t2 :: l2 -> (t1,t2) :: loop l1 l2
		| _ -> assert false
	in
	let subst = loop cparams params in
	let rec loop t =
		try
			List.assq t subst
		with Not_found ->
		match t with
		| TMono r ->
			(match !r with
			| None -> t
			| Some t -> loop t)
		| TEnum (e,tl) ->
			TEnum (e,List.map loop tl)
		| TInst (c,tl) ->
			(match tl with
			| [TMono r] ->
				(match !r with
				| Some tt when t == tt -> 
					(* for dynamic *)
					let pt = mk_mono() in
					let t = TInst (c,[pt]) in
					(match pt with TMono r -> r := Some t | _ -> assert false);
					t
				| _ -> TInst (c,List.map loop tl))
			| _ ->
				TInst (c,List.map loop tl))
		| TFun (tl,r) ->
			TFun (List.map (fun (s,t) -> s, loop t) tl,loop r)
		| TAnon (fl,name) ->
			TAnon (PMap.map (fun f -> { f with cf_type = loop f.cf_type }) fl,name)
		| TLazy f ->
			loop (!f())
		| TDynamic t2 ->
			if t == t2 then
				t
			else
				TDynamic (loop t2)
	in
	loop t

let monomorphs eparams t =
	apply_params eparams (List.map (fun _ -> mk_mono()) eparams) t

let rec type_eq param a b =
	if a == b || (param && b == t_dynamic) then
		true
	else match a , b with
	| TLazy f , _ -> type_eq param (!f()) b
	| _ , TLazy f -> type_eq param a (!f())
	| TMono t , _ -> (match !t with None -> link t a b | Some t -> type_eq param t b)
	| _ , TMono t -> (match !t with None -> link t b a | Some t -> type_eq param a t)
	| TEnum (a,tl1) , TEnum (b,tl2) -> a == b && List.for_all2 (type_eq param) tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) -> 
		c1 == c2 && List.for_all2 (type_eq param) tl1 tl2
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		type_eq param r1 r2 && List.for_all2 (fun (_,t1) (_,t2) -> type_eq param t1 t2) l1 l2
	| TDynamic a , TDynamic b ->
		type_eq param a b
	| TAnon (fl1,_), TAnon (fl2,_) ->
		let keys1 = PMap.fold (fun f acc -> f :: acc) fl1 [] in
		let keys2 = PMap.fold (fun f acc -> f :: acc) fl2 [] in
		(try
			List.iter2 (fun f1 f2 ->
				if f1.cf_name <> f2.cf_name || not (type_eq param f1.cf_type f2.cf_type) then raise Not_found
			) keys1 keys2;
			true
		with
			_ -> false)
	| _ , _ ->
		false

(* perform unification with subtyping.
   the first type is always the most down in the class hierarchy
   it's also the one that is pointed by the position.
   It's actually a typecheck of  A :> B where some mutations can happen *)

type unify_error =
	| Cannot_unify of t * t
	| Invalid_field_type of string
	| Has_no_field of t * string

exception Unify_error of unify_error list

let cannot_unify a b = Cannot_unify (a,b)
let invalid_field n = Invalid_field_type n
let has_no_field t n = Has_no_field (t,n)
let error l = raise (Unify_error l)

let unify_types a b tl1 tl2 =
	List.iter2 (fun ta tb ->
		if not (type_eq true ta tb) then error [cannot_unify a b; cannot_unify ta tb]
	) tl1 tl2

let rec unify a b =
	if a == b then
		()
	else match a, b with
	| TLazy f , _ -> unify (!f()) b
	| _ , TLazy f -> unify a (!f())
	| TMono t , _ -> 
		(match !t with 
		| None -> if not (link t a b) then error [cannot_unify a b] 
		| Some t -> unify t b)
	| _ , TMono t -> 
		(match !t with
		| None -> if not (link t b a) then error [cannot_unify a b]
		| Some t -> unify a t)
	| TEnum (ea,tl1) , TEnum (eb,tl2) -> 
		if ea != eb then error [cannot_unify a b];
		unify_types a b tl1 tl2
	| TInst (c1,tl1) , TInst (c2,tl2) ->
		let rec loop c tl =
			if c == c2 then begin
				unify_types a b tl tl2;
				true
			end else (match c.cl_super with
				| None -> false
				| Some (cs,tls) ->
					loop cs (List.map (apply_params c.cl_types tl) tls)
			) || List.exists (fun (cs,tls) ->
				loop cs (List.map (apply_params c.cl_types tl) tls)
			) c.cl_implements
		in
		if not (loop c1 tl1) then error [cannot_unify a b]
	| TFun (l1,r1) , TFun (l2,r2) when List.length l1 = List.length l2 ->
		(try
			unify r1 r2;
			List.iter2 (fun (_,t1) (_,t2) -> unify t1 t2) l2 l1 (* contravariance *)
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TInst (c,tl) , TAnon (fl,_) ->
		(try
			PMap.iter (fun n f2 ->
				let f1 = (try PMap.find n c.cl_fields with Not_found -> error [has_no_field a n]) in
				try 
					unify (apply_params c.cl_types tl f1.cf_type) f2.cf_type
				with
					Unify_error l -> error (invalid_field n :: l)
			) fl
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TAnon (fl,_) , TInst (c,tl) ->
		let rec loop c tl =
			PMap.iter (fun n f2 ->
				let f1 = (try PMap.find n fl with Not_found -> error [has_no_field a n]) in
				try
					unify f1.cf_type (apply_params c.cl_types tl f2.cf_type)
				with
					Unify_error l -> error (invalid_field n :: l)
			) c.cl_fields;
			List.iter (fun (c,t) -> loop c t) c.cl_implements;
			match c.cl_super with
			| None -> ()
			| Some (c,tl) -> loop c tl
		in
		if c.cl_locked then error [cannot_unify a b];
		(try
			loop c tl;
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TAnon (fl1,_) , TAnon (fl2,_) ->
		(try
			PMap.iter (fun n f2 ->
				let f1 = (try PMap.find n fl1 with Not_found -> error [has_no_field a n]) in
				try
					unify f1.cf_type f2.cf_type
				with
					Unify_error l -> error (invalid_field n :: l)
			) fl2;			
		with
			Unify_error l -> error (cannot_unify a b :: l))
	| TDynamic t , _ ->
		if t == a then
			()
		else (match b with 
		| TDynamic t2 ->
			if t2 != b && not (type_eq true t t2) then error [cannot_unify a b; cannot_unify t t2];
		| _ ->
			error [cannot_unify a b])
	| _ , TDynamic t ->
		if t == b then
			()
		else (match a with 
		| TDynamic t2 -> 
			if t2 != a && not (type_eq true t t2) then error [cannot_unify a b; cannot_unify t t2]
		| _ -> 
			error [cannot_unify a b])
	| _ , _ ->
		error [cannot_unify a b]

let rec iter f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TEnumField _
	| TBreak
	| TContinue
	| TType _ ->
		()
	| TArray (e1,e2)
	| TBinop (_,e1,e2)
	| TFor (_,e1,e2)
	| TWhile (e1,e2,_) ->
		f e1;
		f e2;
	| TThrow e
	| TField (e,_)
	| TParenthesis e
	| TUnop (_,_,e) ->
		f e
	| TArrayDecl el
	| TNew (_,_,el)
	| TBlock el ->
		List.iter f el
	| TObjectDecl fl ->
		List.iter (fun (_,e) -> f e) fl
	| TCall (e,el) ->
		f e;
		List.iter f el
	| TVars vl ->
		List.iter (fun (_,_,e) -> match e with None -> () | Some e -> f e) vl
	| TFunction fu ->
		f fu.tf_expr
	| TIf (e,e1,e2) ->
		f e;
		f e1;
		(match e2 with None -> () | Some e -> f e)
	| TSwitch (e,cases,def) ->
		f e;
		List.iter (fun (e1,e2) -> f e1; f e2) cases;
		(match def with None -> () | Some e -> f e)
	| TMatch (e,_,cases,def) ->
		f e;
		List.iter (fun (_,_,e) -> f e) cases;
		(match def with None -> () | Some e -> f e)
	| TTry (e,catches) ->
		f e;
		List.iter (fun (_,_,e) -> f e) catches
	| TReturn eo ->
		(match eo with None -> () | Some e -> f e)
