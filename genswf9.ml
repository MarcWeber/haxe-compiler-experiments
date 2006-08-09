(*
 *  Haxe Compiler
 *  Copyright (c)2006 Nicolas Cannasse
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
open As3

type ('a,'b) gen_lookup = {
	h : ('a,'b) Hashtbl.t;
	a : 'a DynArray.t;
	c : int -> 'b;
}

type 'a lookup = ('a,'a index) gen_lookup
type 'a lookup_nz = ('a,'a index_nz) gen_lookup

type access =
	| VReg of reg
	| VId of type_index
	| VArray

type code_infos = {
	mutable iregs : int;
	mutable imaxregs : int;
	mutable ipos : int;
	mutable istack : int;
	mutable imax : int;
	mutable iscopes : int;
	mutable imaxscopes : int;
}

type context = {
	(* globals *)
	strings : string lookup;
	ints : int32 lookup;
	floats : float lookup;
	brights : as3_base_right lookup;
	rights : as3_rights lookup;
	types : as3_type lookup;
	mtypes : as3_method_type lookup_nz;
	mutable classes : as3_class list;
	mutable statics : as3_static list;
	functions : as3_function lookup;
	rpublic : as3_base_right index;
	gpublic : as3_rights index;

	(* per-function *)
	mutable locals : (string,int) PMap.t;
	mutable code : as3_opcode DynArray.t;
	mutable infos : code_infos;	
	mutable trys : (int * int * int) list;
}

let error p = Typer.error "Invalid expression" p
let stack_error p = Typer.error "Stack error" p

let stack_delta = function
	| A3Throw -> -1
	| A3GetSuper _ -> 1
	| A3SetSuper _ -> -1
	| A3RegReset _ -> 0
	| A3Nop -> 0
	| A3Jump (cond,_) -> if cond = J3Always then 0 else -1
	| A3Switch _ -> -1
	| A3PopScope -> 0
	| A3XmlOp3 -> assert false
	| A3ForIn | A3ForEach -> assert false
	| A3Null
	| A3Undefined
	| A3SmallInt _
	| A3Int _
	| A3True
	| A3False
	| A3String _
	| A3IntRef _
	| A3Function _
	| A3Float _
	| A3NaN -> 1
	| A3Pop -> -1
	| A3Dup -> 1
	| A3CatchDone -> assert false
	| A3Scope -> -1
	| A3Next _ -> 0
	| A3StackCall n -> -(n + 1)
	| A3StackNew n -> -(n + 1)
	| A3SuperCall (_,n) -> -n
	| A3Call (_,n) -> -n
	| A3RetVoid -> 0
	| A3Ret -> -1
	| A3SuperConstr n -> -(n + 1)
	| A3New (_,n) -> -n
	| A3SuperCallUnknown (_,n) -> -(n + 1)
	| A3CallUnknown (_,n) -> -(n + 1)
	| A3Object n -> -(n * 2) + 1
	| A3Array n -> -n + 1
	| A3NewBlock -> 1
	| A3ClassDef _ -> 0
	| A3XmlOp1 _ -> assert false
	| A3Catch _ -> assert false
	| A3GetInf _ -> 1
	| A3SetInf _ -> 1
	| A3GetProp _ -> 1
	| A3SetProp _ -> -1
	| A3Reg _ -> 1
	| A3SetReg _ -> -1
	| A3GetScope _ -> 1	
	| A3Get _ -> 0
	| A3Set _ -> -2
	| A3Delete _ -> -1
	| A3GetSlot _ -> 0
	| A3SetSlot _ -> -2
	| A3ToInt
	| A3ToUInt
	| A3ToNumber
	| A3ToObject
	| A3ToString
	| A3ToBool -> 0
	| A3XmlOp2 -> assert false
	| A3Cast _ -> 0
	| A3Typeof
	| A3InstanceOf -> -1
	| A3IncrReg _ -> 0
	| A3This -> 1
	| A3DebugReg _ 
	| A3DebugLine _
	| A3DebugFile _ -> 0
	| A3Op op ->
		(match op with
		| A3ONeg | A3OIncr | A3ODecr | A3ONot | A3OBitNot | A3OIIncr | A3OIDecr -> 0
		| _ -> -1)
	| A3Unk _ -> assert false

let index_int (x : int) : 'a index = Obj.magic (x + 1)
let index_nz_int (x : int) : 'a index_nz = Obj.magic x
let tid (x : 'a index) : int = Obj.magic x

let new_lookup() = { h = Hashtbl.create 0; a = DynArray.create(); c = index_int }
let new_lookup_nz() = { h = Hashtbl.create 0; a = DynArray.create(); c = index_nz_int }

let jsize = As3code.length (A3Jump (J3Always,0))

let lookup i w =
	try
		Hashtbl.find w.h i
	with
		Not_found ->
			let id = w.c (DynArray.length w.a) in
			Hashtbl.add w.h i id;
			DynArray.add w.a i;
			id

let add i w =
	let id = w.c (DynArray.length w.a) in
	DynArray.add w.a i;
	id

let lookup_array w = DynArray.to_array w.a

let string ctx i = lookup i ctx.strings

let write ctx op =
	DynArray.add ctx.code op;
	ctx.infos.ipos <- As3code.length op + ctx.infos.ipos;
	let s = ctx.infos.istack + stack_delta op in	
	ctx.infos.istack <- s;
	if s > ctx.infos.imax then ctx.infos.imax <- s;
	match op with
	| A3Scope ->
		let n = ctx.infos.iscopes + 1 in
		ctx.infos.iscopes <- n;
		if n > ctx.infos.imaxscopes then ctx.infos.imaxscopes <- n
	| A3PopScope ->
		ctx.infos.iscopes <- ctx.infos.iscopes - 1
	| _ ->
		()

let debug ctx ?file line =
	(match file with None -> () | Some f -> write ctx (A3DebugFile (string ctx f)));
	write ctx (A3DebugLine line)

let acc_ident ctx i =
	write ctx (A3Reg (PMap.find i ctx.locals))

let jump ctx cond =
	let op = DynArray.length ctx.code in
	write ctx (A3Jump (cond,-4));
	let p = ctx.infos.ipos in
	(fun () ->
		let delta = ctx.infos.ipos - p in
		DynArray.set ctx.code op (A3Jump (cond,delta))
	)

let jump_back ctx =
	write ctx (A3Jump (J3Always,1));
	let p = ctx.infos.ipos in
	write ctx A3Nop;
	(fun cond ->
		let delta = p + -(ctx.infos.ipos + jsize) in
		write ctx (A3Jump (cond,delta))
	)

let type_path ctx ?(getclass=false) (pack,name) =
	let pid = string ctx (String.concat "." pack) in
	let nameid = string ctx name in
	let pid = lookup (A3RPublic (Some pid)) ctx.brights in	
	let tid = lookup (if getclass then A3TClassInterface (Some nameid,lookup [pid] ctx.rights) else A3TMethodVar (nameid,pid)) ctx.types in
	tid

let ident ctx i = type_path ctx ([],i)

let default_infos() =
	{ ipos = 0; istack = 0; imax = 0; iregs = 0; imaxregs = 0; iscopes = 0; imaxscopes = 0 }

let alloc_reg ctx =
	let r = ctx.infos.iregs + 1 in
	ctx.infos.iregs <- r;
	if ctx.infos.imaxregs < r then ctx.infos.imaxregs <- r;
	r

let free_reg ctx r =
	if ctx.infos.iregs <> r then assert false;
	ctx.infos.iregs <- r - 1

let open_block ctx =
	let old_stack = ctx.infos.istack in
	let old_regs = ctx.infos.iregs in
	(fun() ->
		if ctx.infos.istack <> old_stack then assert false;
		ctx.infos.iregs <- old_regs
	)

let begin_fun ctx args =
	let mt = {
		mt3_ret = None;
		mt3_args = List.map (fun _ -> None) args;
		mt3_native = false;
		mt3_var_args = false;
		mt3_debug_name = None;
		mt3_dparams = None;
		mt3_pnames = None;
		mt3_unk_flags = (false,false,false,false);
	} in
	let old_locals = ctx.locals in
	let old_code = ctx.code in
	let old_infos = ctx.infos in
	let old_trys = ctx.trys in
	ctx.infos <- default_infos();
	ctx.code <- DynArray.create();
	ctx.trys <- [];
	ctx.locals <- List.fold_left (fun acc name -> PMap.add name (alloc_reg ctx) acc) PMap.empty args;
	(fun () ->
		let f = {
			fun3_id = add mt ctx.mtypes;
			fun3_stack_size = ctx.infos.imax;
			fun3_nregs = ctx.infos.imaxregs + 1;
			fun3_unk3 = 1;
			fun3_max_scope = ctx.infos.imaxscopes + 1;
			fun3_code = DynArray.to_list ctx.code;
			fun3_trys = Array.of_list (List.map (fun (p,size,cp) ->
				{
					tc3_start = p;
					tc3_end = size;
					tc3_handle = cp;
					tc3_type = None;
					tc3_name = None;
				}
			) (List.rev ctx.trys));
			fun3_locals = [||];
		} in
		ignore(add f ctx.functions);
		ctx.locals <- old_locals;
		ctx.code <- old_code;
		ctx.infos <- old_infos;
		ctx.trys <- old_trys;
		f.fun3_id
	)

let gen_constant ctx c =
	match c with
	| TInt i ->
		if Int32.compare i (-128l) > 0 && Int32.compare i 128l < 0 then
			write ctx (A3SmallInt (Int32.to_int i))
		else
			write ctx (A3IntRef (lookup i ctx.ints));
		write ctx A3ToNumber
	| TFloat f ->
		let f = float_of_string f in
		write ctx (A3Float (lookup f ctx.floats))
	| TString s ->
		write ctx (A3String (lookup s ctx.strings))
	| TBool b ->
		write ctx (if b then A3True else A3False)
	| TNull ->
		write ctx A3Null
	| TThis ->
		write ctx A3This
	| TSuper ->
		assert false

let setvar ctx acc retval =
	match acc with
	| VReg r ->
		if retval then write ctx A3Dup;
		write ctx (A3SetReg r);
	| VId id ->
		if retval then begin				
			let r = alloc_reg ctx in
			write ctx A3Dup;
			write ctx (A3SetReg r);
			write ctx (A3Set id);
			write ctx (A3Reg r);
			free_reg ctx r
		end else
			write ctx (A3Set id)
	| VArray ->
		let id_aset = lookup (A3TArrayAccess ctx.gpublic) ctx.types in
		if retval then begin
			let r = alloc_reg ctx in
			write ctx A3Dup;
			write ctx (A3SetReg r);
			write ctx (A3Set id_aset);
			write ctx (A3Reg r);
			free_reg ctx r
		end else
			write ctx (A3Set id_aset);
		ctx.infos.istack <- ctx.infos.istack - 1

let getvar ctx acc =
	match acc with
	| VReg r ->		
		write ctx (A3Reg r);
	| VId id ->
		write ctx (A3Get id)
	| VArray ->
		let id_aget = lookup (A3TArrayAccess ctx.gpublic) ctx.types in
		write ctx (A3Get id_aget);
		ctx.infos.istack <- ctx.infos.istack - 1

let no_value ctx retval =
	(* does not push a null but still increment the stack like if
	   a real value was pushed *)
	if retval then ctx.infos.istack <- ctx.infos.istack + 1

let rec gen_expr_content ctx retval e =
	match e.eexpr with
	| TConst c ->
		gen_constant ctx c
	| TThrow e ->
		gen_expr ctx true e;
		write ctx A3Throw;
		no_value ctx retval;
	| TTypeExpr t ->
		write ctx (A3GetScope (0,true));
		write ctx (A3Get (type_path ctx (t_path t)));
	| TParenthesis e ->
		gen_expr ctx retval e
	| TEnumField (e,s) ->
		write ctx (A3GetScope (0,true));
		write ctx (A3Get (type_path ctx e.e_path));
		write ctx (A3Get (ident ctx s));
	| TObjectDecl fl ->
		List.iter (fun (name,e) ->
			write ctx (A3String (lookup name ctx.strings));
			gen_expr ctx true e
		) fl;
		write ctx (A3Object (List.length fl))
	| TArrayDecl el ->
		List.iter (gen_expr ctx true) el;
		write ctx (A3Array (List.length el))	
	| TBlock el ->		
		let rec loop = function
			| [] -> if retval then write ctx A3Null
			| [e] -> gen_expr ctx retval e
			| e :: l ->
				gen_expr ctx false e;
				loop l
		in
		let b = open_block ctx in
		loop el;
		b();
	| TVars vl ->
		List.iter (fun (v,_,e) ->
			let r = alloc_reg ctx in
			ctx.locals <- PMap.add v r ctx.locals;
			match e with
			| None -> ()
			| Some e ->
				gen_expr ctx true e;
				write ctx (A3SetReg r)
		) vl
	| TReturn None ->
		write ctx A3RetVoid;
		no_value ctx retval
	| TReturn (Some e) ->
		gen_expr ctx true e;
		write ctx A3Ret;
		no_value ctx retval
	| TField _
	| TLocal _
	| TArray _ ->
		getvar ctx (gen_access ctx e)
	| TBinop (op,e1,e2) ->
		gen_binop ctx retval op e1 e2
	| TCall (e,el) ->
		gen_call ctx e el
	| TNew (c,_,pl) ->
		let id = type_path ctx c.cl_path in
		write ctx (A3GetInf id);
		List.iter (gen_expr ctx true) pl;
		write ctx (A3New (id,List.length pl))
	| TFunction f ->
		write ctx (A3Function (generate_function ctx f true))
	| TIf (e,e1,e2) ->
		gen_expr ctx true e;
		let j = jump ctx J3False in
		gen_expr ctx retval e1;
		(match e2 with
		| None -> j()
		| Some e ->
			let jend = jump ctx J3Always in
			j();
			gen_expr ctx retval e;
			jend())
	| TWhile (econd,e,NormalWhile) ->
		let loop = jump_back ctx in
		gen_expr ctx true econd;
		let jend = jump ctx J3False in
		gen_expr ctx false e;
		loop J3Always;
		jend();
		if retval then write ctx A3Null
	| TWhile (econd,e,DoWhile) ->	
		let loop = jump_back ctx in
		gen_expr ctx false e;
		gen_expr ctx true econd;
		loop J3True;
		if retval then write ctx A3Null
	| TUnop (op,flag,e) ->
		gen_unop ctx retval op flag e
	| TTry (e,cases) ->
		let p = ctx.infos.ipos in
		gen_expr ctx retval e;
		let pend = ctx.infos.ipos in
		let jend = jump ctx J3Always in		
		let rec loop ncases = function
			| [] -> []
			| (ename,t,e) :: l ->
				let old_locals = ctx.locals in
				let r = alloc_reg ctx in
				ctx.trys <- (p,pend,ctx.infos.ipos) :: ctx.trys;
				ctx.infos.istack <- ctx.infos.istack + 1;
				if ctx.infos.imax < ctx.infos.istack then ctx.infos.imax <- ctx.infos.istack;
				write ctx A3This;
				write ctx A3Scope;
				write ctx (A3SetReg r);
				ctx.locals <- PMap.add ename r ctx.locals;
				gen_expr ctx retval e;
				ctx.locals <- old_locals;
				free_reg ctx r;				
				match l with
				| [] -> []
				| _ -> 
					let j = jump ctx J3Always in
					j :: loop (ncases + 1) l
		in
		let loops = loop (List.length ctx.trys) cases in
		List.iter (fun j -> j()) loops;
		jend()
	| TFor (v,it,e) ->
		gen_expr ctx true it;
		let r = alloc_reg ctx in
		write ctx (A3SetReg r);
		let start = jump_back ctx in
		write ctx (A3Reg r);
		write ctx (A3Call (ident ctx "hasNext",0));
		let jend = jump ctx J3False in

		let r2 = alloc_reg ctx in
		let old_locals = ctx.locals in
		write ctx (A3Reg r);
		write ctx (A3Call (ident ctx "next",0));
		write ctx (A3SetReg r2);
		ctx.locals <- PMap.add v r2 ctx.locals;
		gen_expr ctx false e;
		ctx.locals <- old_locals;
		free_reg ctx r2;

		start J3Always;
		jend();
		free_reg ctx r;
		if retval then write ctx (A3Reg r2)

(*
	| TSwitch of texpr * (texpr * texpr) list * texpr option
	| TMatch of texpr * (tenum * t list) * (string * (string option * t) list option * texpr) list * texpr option
	| TBreak
	| TContinue
*)
	| _ ->
		assert false

and gen_call ctx e el =
	match e.eexpr with
	| TConst TSuper ->
		write ctx A3This;
		List.iter (gen_expr ctx true) el;
		write ctx (A3SuperConstr (List.length el));
	| TField ({ eexpr = TConst TSuper },f) ->
		let id = ident ctx f in
		write ctx (A3GetInf id);
		List.iter (gen_expr ctx true) el;
		write ctx (A3SuperCall (id,List.length el));
	| TField ({ eexpr = TConst TThis },f) ->
		let id = ident ctx f in
		write ctx (A3GetInf id);
		List.iter (gen_expr ctx true) el;
		write ctx (A3Call (id,List.length el));
	| TField (e,f) ->
		gen_expr ctx true e;
		List.iter (gen_expr ctx true) el;
		write ctx (A3Call (ident ctx f,List.length el));
	| _ ->
		gen_expr ctx true e;
		write ctx (A3GetScope (0,true));
		List.iter (gen_expr ctx true) el;
		write ctx (A3StackCall (List.length el))

and gen_access ctx e =
	match e.eexpr with
	| TLocal i ->
		VReg (try PMap.find i ctx.locals with Not_found -> error e.epos)
	| TField (e,f) ->
		let id = ident ctx f in
		(match e.eexpr with 
		| TConst TThis -> write ctx (A3GetInf id)
		| _ -> gen_expr ctx true e);
		VId id
	| TArray (e,eindex) ->
		gen_expr ctx true e;
		gen_expr ctx true eindex;
		VArray
	| _ ->
		error e.epos

and gen_unop ctx retval op flag e =
	match op with
	| Not ->
		gen_expr ctx true e;
		write ctx (A3Op A3ONot);
	| Neg ->
		gen_expr ctx true e;
		write ctx (A3Op A3ONeg);
	| NegBits ->
		gen_expr ctx true e;
		write ctx (A3Op A3OBitNot);
	| Increment
	| Decrement ->
		let incr = (op = Increment) in
		let acc = gen_access ctx e in (* for set *)
		getvar ctx (gen_access ctx e);
		match flag with
		| Postfix when retval ->
			let r = alloc_reg ctx in
			write ctx A3Dup;
			write ctx (A3SetReg r);
			write ctx (A3Op (if incr then A3OIncr else A3ODecr));
			setvar ctx acc false;
			write ctx (A3Reg r);
			free_reg ctx r
		| Postfix | Prefix ->
			write ctx (A3Op (if incr then A3OIncr else A3ODecr));
			setvar ctx acc retval

and gen_binop ctx retval op e1 e2 =
	let gen_op o =
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write ctx (A3Op o)
	in
	match op with
	| OpAssign ->
		let acc = gen_access ctx e1 in
		gen_expr ctx true e2;
		setvar ctx acc retval
	| OpBoolAnd ->
		gen_expr ctx true e1;
		write ctx A3Dup;
		let j = jump ctx J3False in
		write ctx A3Pop;
		gen_expr ctx true e2;
		j();
	| OpBoolOr ->
		gen_expr ctx true e1;
		write ctx A3Dup;
		let j = jump ctx J3True in
		write ctx A3Pop;
		gen_expr ctx true e2;
		j();
	| OpAssignOp op ->
		let acc = gen_access ctx e1 in
		gen_binop ctx true op e1 e2;
		setvar ctx acc retval
	| OpAdd ->
		gen_op A3OAdd
	| OpMult ->
		gen_op A3OMul
	| OpDiv ->
		gen_op A3ODiv
	| OpSub ->
		gen_op A3OSub
	| OpEq ->
		gen_op A3OEq
	| OpPhysEq ->
		gen_op A3OPhysEq
	| OpNotEq ->
		gen_op A3OEq;
		write ctx (A3Op A3ONot)
	| OpPhysNotEq ->
		gen_op A3OPhysEq;
		write ctx (A3Op A3ONot)
	| OpGt ->
		gen_op A3OGt
	| OpGte ->
		gen_op A3OGte
	| OpLt ->
		gen_op A3OLt
	| OpLte ->
		gen_op A3OLte
	| OpAnd ->
		gen_op A3OAnd
	| OpOr ->
		gen_op A3OOr
	| OpXor ->
		gen_op A3OXor
	| OpShl ->
		gen_op A3OShl
	| OpShr ->
		gen_op A3OShr
	| OpUShr ->
		gen_op A3OUShr
	| OpMod ->
		gen_op A3OMod
	| OpInterval ->
		assert false

and gen_expr ctx retval e =
	let old = ctx.infos.istack in
	gen_expr_content ctx retval e;
	if old <> ctx.infos.istack then begin
		if old + 1 <> ctx.infos.istack then stack_error e.epos;
		if not retval then write ctx A3Pop;
	end else if retval then stack_error e.epos

and generate_function ctx fdata stat =
	let f = begin_fun ctx (List.map (fun (name,_,_) -> name) fdata.tf_args) in
	if not stat then begin
		write ctx A3This;
		write ctx A3Scope;
	end;
	gen_expr ctx false fdata.tf_expr;
	write ctx A3RetVoid;
	f()

let generate_construct ctx args =
	let f = begin_fun ctx args in
	write ctx A3This;
	write ctx A3Scope;
	write ctx A3This;
	(try List.iter (acc_ident ctx) args with Not_found -> assert false);
	write ctx (A3SuperConstr (List.length args));
	write ctx A3RetVoid;
	f()

let generate_class_init ctx c slot =
	write ctx (A3GetScope (0,true));
	let path = (match c.cl_super with None -> ([],"Object") | Some (sup,_) -> sup.cl_path) in
	write ctx (A3GetProp (type_path ctx path));
	write ctx A3Scope;
	write ctx (A3GetProp (type_path ~getclass:true ctx path));
	write ctx (A3ClassDef slot);
	write ctx A3PopScope;
	let r = alloc_reg ctx in
	write ctx A3Dup;
	write ctx (A3SetReg r);	
	write ctx (A3Set (type_path ctx c.cl_path));
	let nslot = ref 0 in
	List.iter (fun f ->
		incr nslot;
		match f.cf_expr with
		| Some { eexpr = TFunction _ } | None -> ()
		| Some e ->
			write ctx (A3Reg r);
			gen_expr ctx true e;
			write ctx (A3SetSlot !nslot);
	) c.cl_ordered_statics

let generate_class_static ctx c =
	let f = begin_fun ctx [] in
	write ctx A3RetVoid;
	f()

let generate_field_kind ctx f c stat =
	match f.cf_expr with
	| Some { eexpr = TFunction fdata } ->
		let rec loop c =
			match c.cl_super with
			| None -> false
			| Some (c,_) ->
				PMap.exists f.cf_name c.cl_fields || loop c
		in
		A3FMethod {
			m3_type = generate_function ctx fdata stat;
			m3_final = false;
			m3_override = not stat && loop c;
			m3_kind = MK3Normal;
		}
	| _ ->
		A3FVar {
			v3_type = None;
			v3_value = A3VNone;
			v3_const = false;
		}

let generate_class ctx c =
	let name_id = type_path ctx c.cl_path in
	let st_id = generate_class_static ctx c in
	let cid = (match c.cl_constructor with
		| None ->
			let rec loop c =
				match c.cl_super with
				| None ->
					generate_construct ctx []
				| Some (csup,_) ->
					match csup.cl_constructor with
					| None -> loop csup
					| Some co -> 
						let args = (match follow co.cf_type with 
							| TFun (l,_) -> List.map (fun (name,_,_) -> name) l
							| _ -> assert false
						) in
						generate_construct ctx args
			in
			loop c
		| Some f ->
			match f.cf_expr with
			| Some { eexpr = TFunction f } -> generate_function ctx f false
			| _ -> assert false
	) in
	let fields = Array.of_list (PMap.fold (fun f acc ->
		{
			f3_name = ident ctx f.cf_name;
			f3_slot = 0;
			f3_kind = generate_field_kind ctx f c false;
			f3_metas = None;
		} :: acc
	) c.cl_fields []) in
	let sc = {
		cl3_name = name_id;
		cl3_super = Some (type_path ctx (match c.cl_super with None -> [],"Object" | Some (c,_) -> c.cl_path));
		cl3_sealed = true;
		cl3_final = false;
		cl3_interface = false;
		cl3_rights = None;
		cl3_implements = [||];
		cl3_construct = cid;
		cl3_fields = fields;
	} in
	let st_count = ref 0 in
	let st = {
		st3_method = st_id;
		st3_fields = Array.of_list (List.map (fun f ->
			incr st_count;
			{
				f3_name = ident ctx f.cf_name;
				f3_slot = !st_count;
				f3_kind = generate_field_kind ctx f c true;
				f3_metas = None;
			}
		) c.cl_ordered_statics)
	} in
	ctx.classes <- sc :: ctx.classes;
	ctx.statics <- st :: ctx.statics;
	()

let generate_type ctx t =
	match t with
	| TClassDecl c -> if not c.cl_extern then generate_class ctx c
	| TTypeDecl _ -> ()
	| TEnumDecl e ->
		match e.e_path with
		| [] , "Void" | [] , "Bool" | [] , "Dynamic" -> ()
		| _ ->
			failwith (Ast.s_type_path e.e_path)

let generate_inits ctx types =
	let f = begin_fun ctx [] in
	write ctx A3This;
	write ctx A3Scope;
	let slot = ref 0 in
	let classes = List.fold_left (fun acc t ->
		match t with
		| TClassDecl c when not c.cl_extern ->
			incr slot;
			generate_class_init ctx c (!slot - 1);
			{ 
				f3_name = type_path ctx c.cl_path;
				f3_slot = !slot;
				f3_kind = A3FClass (index_nz_int (!slot - 1));
				f3_metas = None;
			} :: acc
		| _ -> acc
	) [] types in
	write ctx A3RetVoid;	
	{
		st3_method = f();
		st3_fields = Array.of_list (List.rev classes);
	}
	
let generate types hres =
	let brights = new_lookup() in
	let strings = new_lookup() in
	let rights = new_lookup() in
	let empty_id = lookup "" strings in
	let rpublic = lookup (A3RPublic (Some empty_id)) brights in
	let ctx = {
		strings = strings;
		ints = new_lookup();
		floats = new_lookup();
		brights = brights;
		rights = rights;
		types = new_lookup();
		mtypes = new_lookup_nz();
		rpublic = rpublic;
		gpublic = lookup [rpublic] rights;
		classes = [];
		statics = [];
		functions = new_lookup();

		code = DynArray.create();
		locals = PMap.empty;
		infos = default_infos();
		trys = [];
	} in	
	List.iter (generate_type ctx) types;
	Hashtbl.iter (fun _ _ -> assert false) hres;
	let init = generate_inits ctx types in
	let a = {
		as3_ints = lookup_array ctx.ints;
		as3_floats = lookup_array ctx.floats;
		as3_idents = lookup_array ctx.strings;
		as3_base_rights = lookup_array ctx.brights;
		as3_rights = lookup_array ctx.rights;
		as3_types = lookup_array ctx.types;
		as3_method_types = lookup_array ctx.mtypes;
		as3_metadatas = [||];
		as3_classes = Array.of_list (List.rev ctx.classes);
		as3_statics = Array.of_list (List.rev ctx.statics);
		as3_inits = [|init|];
		as3_functions = lookup_array ctx.functions;
		as3_unknown = "";
	} in
	[Swf.TActionScript3 (None,a)]
