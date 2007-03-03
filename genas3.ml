(*
 *  Haxe Compiler
 *  Copyright (c)2005-2007 Nicolas Cannasse
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

type context = {
	ch : out_channel;
	buf : Buffer.t;
	path : module_path;
	mutable get_sets : (string * bool,string) Hashtbl.t;
	mutable curclass : tclass;
	mutable tabs : string;
	mutable in_value : string option;
	mutable in_static : bool;
	mutable handle_break : bool;
	mutable imports : (string,string list) Hashtbl.t;
	mutable locals : (string,string) PMap.t;
	mutable inv_locals : (string,string) PMap.t;
	mutable local_types : t list;
	mutable inits : texpr list;
	mutable constructor_block : bool;
}

let s_path ctx path p =
	match path with
	| ([],name) ->
		(match name with
		| "Int" -> "int"		
		| "Float" -> "Number"
		| "Dynamic" -> "Object"
		| "Bool" -> "Boolean"
		| _ -> name)
	| (["flash"],"FlashXml__") ->
		"Xml"
	| (pack,name) ->		
		(try
			let pack2 = Hashtbl.find ctx.imports name in
			if pack2 <> pack then Typer.error ("The classes " ^ Ast.s_type_path (pack,name) ^ " and " ^ Ast.s_type_path (pack2,name) ^ " conflicts") p;
		with Not_found ->
			Hashtbl.add ctx.imports name pack);
		name

let s_ident n =
	match n with
	| "is" -> "_is"
	| "int" -> "_int"
	| "getTimer" -> "_getTimer"
	| "typeof" -> "_typeof"
	| "parseInt" -> "_parseInt"
	| "parseFloat" -> "_parseFloat"
	| _ -> n

let init dir path =
	let rec create acc = function
		| [] -> ()
		| d :: l ->
			let dir = String.concat "/" (List.rev (d :: acc)) in
			if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
			create (d :: acc) l
	in
	let dir = dir :: fst path in
	create [] dir;
	let ch = open_out (String.concat "/" dir ^ "/" ^ snd path ^ ".as") in
	{
		tabs = "";
		ch = ch;
		path = path;
		buf = Buffer.create (1 lsl 14);
		in_value = None;
		in_static = false;
		handle_break = false;
		imports = Hashtbl.create 0;
		curclass = null_class;
		locals = PMap.empty;
		inv_locals = PMap.empty;
		local_types = [];
		inits = [];
		get_sets = Hashtbl.create 0;
		constructor_block = false;
	}

let close ctx =
	output_string ctx.ch (Printf.sprintf "package %s {\n" (String.concat "." (fst ctx.path)));
	Hashtbl.iter (fun name pack ->
		if ctx.path <> (pack,name) then output_string ctx.ch ("\timport " ^ Ast.s_type_path (pack,name) ^ ";\n");
	) ctx.imports;
	output_string ctx.ch (Buffer.contents ctx.buf);
	close_out ctx.ch

let save_locals ctx =
	let old = ctx.locals in
	(fun() -> ctx.locals <- old)

let define_local ctx l =
	let rec loop n =
		let name = (if n = 1 then s_ident l else l ^ string_of_int n) in
		if PMap.mem name ctx.inv_locals then
			loop (n+1)
		else begin
			ctx.locals <- PMap.add l name ctx.locals;
			ctx.inv_locals <- PMap.add name l ctx.inv_locals;
			name
		end
	in
	loop 1

let spr ctx s = Buffer.add_string ctx.buf s
let print ctx = Printf.kprintf (fun s -> Buffer.add_string ctx.buf s)

let unsupported = Typer.error "This expression cannot be generated to AS3"

let rec follow_not_stat t =
	match t with
	| TMono r ->
		(match !r with
		| Some t -> follow_not_stat t
		| _ -> t)
	| TLazy f ->
		follow_not_stat (!f())
	| TType (t,tl) when t.t_static = None ->
		follow_not_stat (apply_params t.t_types tl t.t_type)
	| _ -> t

let newline ctx =
	match Buffer.nth ctx.buf (Buffer.length ctx.buf - 1) with
	| '}' | '{' | ':' -> print ctx "\n%s" ctx.tabs
	| _ -> print ctx ";\n%s" ctx.tabs

let rec concat ctx s f = function
	| [] -> ()
	| [x] -> f x
	| x :: l ->
		f x;
		spr ctx s;
		concat ctx s f l

let open_block ctx =
	let oldt = ctx.tabs in
	ctx.tabs <- "\t" ^ ctx.tabs;
	(fun() -> ctx.tabs <- oldt)

let block = Transform.block

let parent e =
	match e.eexpr with
	| TParenthesis _ -> e
	| _ -> mk (TParenthesis e) e.etype e.epos

let rec type_str ctx t p =	
	match t with
	| TEnum _ | TInst _ when List.memq t ctx.local_types ->
		"*"
	| TEnum (e,_) ->
		if e.e_extern then (match e.e_path with
			| [], "Void" -> "void"
			| [], "Bool" -> "Boolean"
			| _ -> "Object"
		) else
			s_path ctx e.e_path p
	| TInst (c,_) -> 
		if (snd c.cl_path).[0] = '+' then "*" else s_path ctx c.cl_path p
	| TFun _ ->
		"Function"
	| TMono r ->
		(match !r with None -> "*" | Some t -> type_str ctx t p)
	| TAnon _ | TDynamic _ ->
		"*"
	| TType (t,args) ->
		(match t.t_path with
		| [], "UInt" -> "uint"
		| [] , "Null" ->
			(match args with
			| [_,t] ->
				(match follow t with
				| TInst ({ cl_path = [],"Int" },_)
				| TInst ({ cl_path = [],"Float" },_)
				| TEnum ({ e_path = [],"Bool" },_) -> "*"
				| _ -> type_str ctx t p)
			| _ -> assert false);
		| _ -> type_str ctx (apply_params t.t_types args t.t_type) p)
	| TLazy f ->
		type_str ctx ((!f)()) p

let rec iter_switch_break in_switch e =
	match e.eexpr with
	| TFunction _ | TWhile _ | TFor _ -> ()
	| TSwitch _ | TMatch _ when not in_switch -> iter_switch_break true e
	| TBreak when in_switch -> raise Exit
	| _ -> iter (iter_switch_break in_switch) e

let handle_break ctx e =
	let old_handle = ctx.handle_break in
	try
		iter_switch_break false e;
		ctx.handle_break <- false;
		(fun() -> ctx.handle_break <- old_handle)
	with
		Exit ->
			spr ctx "try {";
			let b = open_block ctx in
			newline ctx;
			ctx.handle_break <- true;
			(fun() ->
				b();
				ctx.handle_break <- old_handle;
				newline ctx;
				spr ctx "} catch( e : * ) { if( e != \"__break__\" ) throw e; }";
			)

let this ctx = if ctx.in_value <> None then "$this" else "this"

let gen_function_header ctx name f params p =
	let old = ctx.in_value in
	let old_l = ctx.locals in
	let old_li = ctx.inv_locals in
	let old_t = ctx.local_types in
	ctx.in_value <- None;
	ctx.local_types <- List.map snd params @ ctx.local_types;
	print ctx "function%s(" (match name with None -> "" | Some n -> " " ^ n);
	concat ctx "," (fun (arg,o,t) ->
		let arg = define_local ctx arg in
		print ctx "%s : %s" arg (type_str ctx t p);
		if o then spr ctx " = null";
	) f.tf_args;
	print ctx ") : %s " (type_str ctx f.tf_type p);
	(fun () ->
		ctx.in_value <- old;
		ctx.locals <- old_l;
		ctx.inv_locals <- old_li;
		ctx.local_types <- old_t;
	)

let escape_bin s =
	let b = Buffer.create 0 in
	for i = 0 to String.length s - 1 do
		match Char.code (String.unsafe_get s i) with
		| c when c < 32 -> Buffer.add_string b (Printf.sprintf "\\x%.2X" c)
		| c -> Buffer.add_char b (Char.chr c)
	done;
	Buffer.contents b

let gen_constant ctx p = function
	| TInt i -> print ctx "%ld" i
	| TFloat s -> spr ctx s
	| TString s -> print ctx "\"%s\"" (escape_bin (Ast.s_escape s))
	| TBool b -> spr ctx (if b then "true" else "false")
	| TNull -> spr ctx "null"
	| TThis -> spr ctx (this ctx)
	| TSuper -> spr ctx "super"

let rec gen_call ctx e el =
	match e.eexpr , el with
	| TCall (x,_) , el ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")";
	| TLocal "__is__" , [e1;e2] ->
		gen_value ctx e1;
		spr ctx " is ";
		gen_value ctx e2;
	| TLocal "__as__" , [e1;e2] ->
		gen_value ctx e1;
		spr ctx " as ";
		gen_value ctx e2;
	| TLocal "__typeof__", [e] ->
		spr ctx "typeof ";
		gen_value ctx e;
	| TLocal "__keys__", [e] ->
		let ret = (match ctx.in_value with None -> assert false | Some r -> r) in
		print ctx "%s = new Array()" ret;
		newline ctx;
		let b = save_locals ctx in
		let tmp = define_local ctx "$k" in
		print ctx "for(var %s : String in " tmp;
		gen_value ctx e;
		print ctx ") %s.push(%s)" ret tmp;
		b();
	| TLocal "__new__", e :: args ->
		spr ctx "new ";
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) args;
		spr ctx ")";
	| TLocal "__delete__", [e;f] ->
		spr ctx "delete(";
		gen_value ctx e;
		spr ctx "[";
		gen_value ctx f;
		spr ctx "]";
		spr ctx ")";
	| TLocal "__unprotect__", [e] ->
		gen_value ctx e
	| _ ->
		gen_value ctx e;
		spr ctx "(";
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"

and gen_value_op ctx e =
	match e.eexpr with
	| TBinop (op,_,_) when op = Ast.OpAnd || op = Ast.OpOr || op = Ast.OpXor ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
	| _ ->
		gen_value ctx e

and gen_field_access ctx t s =
	match follow_not_stat t with
	| TType ({ t_static = Some c },_) | TInst (c,_) ->
		(match fst c.cl_path, snd c.cl_path, s with
		| [], "Math", "NaN"
		| [], "Math", "NEGATIVE_INFINITY"
		| [], "Math", "POSITIVE_INFINITY"
		| [], "Math", "isFinite"
		| [], "Math", "isNaN"
		| [], "Date", "now"
		| [], "Date", "fromTime"
		| [], "Date", "fromString"
		| [], "Date", "toString"
		| [], "String", "charCodeAt"
		-> 
			print ctx "[\"%s\"]" s
		| _ -> 
			print ctx ".%s" (s_ident s));
	| _ ->
		print ctx ".%s" (s_ident s)

and gen_expr ctx e =
	match e.eexpr with
	| TConst c ->
		gen_constant ctx e.epos c
	| TLocal s ->
		spr ctx (try PMap.find s ctx.locals with Not_found -> Typer.error ("Unknown local " ^ s) e.epos)
	| TEnumField (en,s) ->
		print ctx "%s.%s" (s_path ctx en.e_path e.epos) (s_ident s)
	| TArray ({ eexpr = TLocal "__global__" },{ eexpr = TConst (TString s) }) ->
		let path = (match List.rev (ExtString.String.nsplit s ".") with
			| [] -> assert false
			| x :: l -> List.rev l , x
		) in
		spr ctx (s_path ctx path e.epos)
	| TArray (e1,e2) ->
		gen_value ctx e1;
		spr ctx "[";
		gen_value ctx e2;
		spr ctx "]";
	| TBinop (op,{ eexpr = TField (e1,s) },e2) ->
		gen_value_op ctx e1;
		gen_field_access ctx e1.etype s;
		print ctx " %s " (Ast.s_binop op);
		gen_value_op ctx e2;
	| TBinop (op,e1,e2) ->
		gen_value_op ctx e1;
		print ctx " %s " (Ast.s_binop op);
		gen_value_op ctx e2;
	| TField ({ eexpr = TTypeExpr t },s) when t_path t = ctx.curclass.cl_path && not (PMap.mem s ctx.locals) ->
		print ctx "%s" (s_ident s)
	| TField (e,s) ->
   		gen_value ctx e;
		gen_field_access ctx e.etype s
	| TTypeExpr t ->
		spr ctx (s_path ctx (t_path t) e.epos)
	| TParenthesis e ->
		spr ctx "(";
		gen_value ctx e;
		spr ctx ")";
	| TReturn eo ->
		if ctx.in_value <> None then unsupported e.epos;
		(match eo with
		| None ->
			spr ctx "return"
		| Some e ->
			spr ctx "return ";
			gen_value ctx e);
	| TBreak ->
		if ctx.in_value <> None then unsupported e.epos;
		if ctx.handle_break then spr ctx "throw \"__break__\"" else spr ctx "break"
	| TContinue ->
		if ctx.in_value <> None then unsupported e.epos;
		spr ctx "continue"
	| TBlock [] ->
		spr ctx "null"
	| TBlock el ->
		let b = save_locals ctx in
		print ctx "{";
		let bend = open_block ctx in
		let cb = (if not ctx.constructor_block then
			(fun () -> ())
		else begin
			ctx.constructor_block <- false;
			print ctx " if( !%s.skip_constructor ) {" (s_path ctx (["flash"],"Boot") e.epos);
            (fun() -> print ctx "}")
		end) in
		List.iter (fun e -> newline ctx; gen_expr ctx e) el;
		bend();
		newline ctx;
		cb();
		print ctx "}";
		b();
	| TFunction f ->
		let h = gen_function_header ctx None f [] e.epos in
		gen_expr ctx (block f.tf_expr);		
		h();
	| TCall (e,el) ->
		gen_call ctx e el
	| TArrayDecl el ->
		spr ctx "[";
		concat ctx "," (gen_value ctx) el;
		spr ctx "]"
	| TThrow e ->
		spr ctx "throw ";
		gen_value ctx e;
	| TVars [] ->
		()
	| TVars vl ->
		spr ctx "var ";
		concat ctx ", " (fun (n,t,v) ->
			let n = define_local ctx n in
			print ctx "%s : %s" n (type_str ctx t e.epos);
			match v with
			| None -> ()
			| Some e ->
				spr ctx " = ";
				gen_value ctx e
		) vl;
	| TNew (c,_,el) ->
		print ctx "new %s(" (s_path ctx c.cl_path e.epos);
		concat ctx "," (gen_value ctx) el;
		spr ctx ")"
	| TIf (cond,e,eelse) ->
		spr ctx "if";
		gen_value ctx (parent cond);
		spr ctx " ";
		gen_expr ctx e;
		(match eelse with
		| None -> ()
		| Some e ->
			newline ctx;
			spr ctx "else ";
			gen_expr ctx e);
	| TUnop (op,Ast.Prefix,e) ->
		spr ctx (Ast.s_unop op);
		gen_value ctx e
	| TUnop (op,Ast.Postfix,e) ->
		gen_value ctx e;
		spr ctx (Ast.s_unop op)
	| TWhile (cond,e,Ast.NormalWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "while";
		gen_value ctx (parent cond);
		spr ctx " ";
		gen_expr ctx e;
		handle_break();
	| TWhile (cond,e,Ast.DoWhile) ->
		let handle_break = handle_break ctx e in
		spr ctx "do ";
		gen_expr ctx e;
		spr ctx " while";
		gen_value ctx (parent cond);
		handle_break();
	| TObjectDecl fields ->
		spr ctx "{ ";
		concat ctx ", " (fun (f,e) -> print ctx "%s : " f; gen_value ctx e) fields;
		spr ctx "}"
	| TFor (v,t,it,e) ->
		let handle_break = handle_break ctx e in
		let b = save_locals ctx in
		let tmp = define_local ctx "$it" in		
		print ctx "{ var %s : * = " tmp;
		gen_value ctx it;
		newline ctx;
		let v = define_local ctx v in
		print ctx "while( %s.hasNext() ) { var %s : %s = %s.next()" tmp v (type_str ctx t e.epos) tmp;
		newline ctx;
		gen_expr ctx e;
		newline ctx;
		spr ctx "}}";
		b();
		handle_break();
	| TTry (e,catchs) ->
		spr ctx "try ";
		gen_expr ctx (block e);
		List.iter (fun (v,t,e) ->
			newline ctx;
			let b = save_locals ctx in
			let v = define_local ctx v in
			print ctx "catch( %s : %s )" v (type_str ctx t e.epos);
			gen_expr ctx (block e);
			b();
		) catchs;
	| TMatch (e,_,cases,def) ->
		let b = save_locals ctx in
		let tmp = define_local ctx "$e" in
		print ctx "var %s : enum = " tmp;
		gen_value ctx e;
		newline ctx;
		print ctx "switch( %s.tag ) {" tmp;
		newline ctx;
		List.iter (fun (cl,params,e) ->
			List.iter (fun c ->
				print ctx "case \"%s\":" c;
				newline ctx;
			) cl;
			let b = save_locals ctx in
			(match params with
			| None | Some [] -> ()
			| Some l ->
				let n = ref (-1) in
				let l = List.fold_left (fun acc (v,t) -> incr n; match v with None -> acc | Some v -> (v,t,!n) :: acc) [] l in
				match l with
				| [] -> ()
				| l ->
					spr ctx "var ";
					concat ctx ", " (fun (v,t,n) ->
						let v = define_local ctx v in
						print ctx "%s : %s = %s.params[%d]" v (type_str ctx t e.epos) tmp n;
					) l;
					newline ctx);
			gen_expr ctx (block e);
			print ctx "break";
			newline ctx;
			b()
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			spr ctx "default:";
			gen_expr ctx (block e);
			print ctx "break";
			newline ctx;
		);
		spr ctx "}";
		b()
	| TSwitch (e,cases,def) ->
		spr ctx "switch";
		gen_value ctx (parent e);
		spr ctx " {";
		newline ctx;
		List.iter (fun (el,e2) ->
			List.iter (fun e ->
				spr ctx "case ";
				gen_value ctx e;
				spr ctx ":";
			) el;
			gen_expr ctx (block e2);
			print ctx "break";
			newline ctx;
		) cases;
		(match def with
		| None -> ()
		| Some e ->
			spr ctx "default:";
			gen_expr ctx (block e);
			print ctx "break";
			newline ctx;
		);
		spr ctx "}"

and gen_value ctx e =
	let assign e =
		mk (TBinop (Ast.OpAssign,
			mk (TLocal (match ctx.in_value with None -> assert false | Some v -> "$r")) t_dynamic e.epos,
			e
		)) e.etype e.epos
	in
	let value block =
		let old = ctx.in_value in
		let t = type_str ctx e.etype e.epos in
		let locs = save_locals ctx in
		let tmp = define_local ctx "$r" in
		ctx.in_value <- Some tmp;
		if ctx.in_static then
			print ctx "function() : %s " t
		else
			print ctx "function($this:%s) : %s " (snd ctx.path) t;
		let b = if block then begin
			spr ctx "{";
			let b = open_block ctx in
			newline ctx;
			print ctx "var %s : %s" tmp t;
			newline ctx;
			b
		end else
			(fun() -> ())
		in
		(fun() ->
			if block then begin
				newline ctx;
				print ctx "return %s" tmp;
				b();
				newline ctx;
				spr ctx "}";
			end;
			ctx.in_value <- old;
			locs();
			if ctx.in_static then
				print ctx "()"
			else
				print ctx "(%s)" (this ctx)
		)
	in
	match e.eexpr with
	| TCall ({ eexpr = TLocal "__keys__" },_) ->
		let v = value true in
		gen_expr ctx e;
		v()
	| TConst _
	| TLocal _
	| TEnumField _
	| TArray _
	| TBinop _
	| TField _
	| TTypeExpr _
	| TParenthesis _
	| TObjectDecl _
	| TArrayDecl _
	| TCall _
	| TNew _
	| TUnop _
	| TFunction _ ->
		gen_expr ctx e
	| TReturn _
	| TBreak
	| TContinue ->
		unsupported e.epos
	| TVars _
	| TFor _
	| TWhile _
	| TThrow _ ->
		(* value is discarded anyway *)
		let v = value true in
		gen_expr ctx e;
		v()
	| TBlock el ->		
		let v = value true in
		let rec loop = function
			| [] ->
				spr ctx "return null";
			| [e] ->
				gen_expr ctx (assign e);
			| e :: l ->
				gen_expr ctx e;
				newline ctx;
				loop l
		in
		loop el;
		v();
	| TIf (cond,e,eo) ->
		spr ctx "(";
		gen_value ctx cond;
		spr ctx "?";
		gen_value ctx e;
		spr ctx ":";
		(match eo with
		| None -> spr ctx "null"
		| Some e -> gen_value ctx e);
		spr ctx ")"
	| TSwitch (cond,cases,def) ->
		let v = value true in
		gen_expr ctx (mk (TSwitch (cond,
			List.map (fun (e1,e2) -> (e1,assign e2)) cases,
			match def with None -> None | Some e -> Some (assign e)
		)) e.etype e.epos);
		v()
	| TMatch (cond,enum,cases,def) ->
		let v = value true in
		gen_expr ctx (mk (TMatch (cond,enum,
			List.map (fun (constr,params,e) -> (constr,params,assign e)) cases,
			match def with None -> None | Some e -> Some (assign e)
		)) e.etype e.epos);
		v()
	| TTry (b,catchs) ->
		let v = value true in
		gen_expr ctx (mk (TTry (assign b,
			List.map (fun (v,t,e) -> v, t , assign e) catchs
		)) e.etype e.epos);
		v()

let generate_boot_init ctx =	
	print ctx "private static function init() : void {";
	List.iter (gen_expr ctx) ctx.inits;
	print ctx "}"

let generate_field ctx static f =
	newline ctx;
	ctx.in_static <- static;
	ctx.locals <- PMap.empty;
	ctx.inv_locals <- PMap.empty;
	let public = f.cf_public || Hashtbl.mem ctx.get_sets (f.cf_name,static) || (f.cf_name = "main" && static) in
	let rights = (if static then "static " else "") ^ (if public then "public" else "protected") in
	let p = ctx.curclass.cl_pos in
	match f.cf_expr with
	| Some { eexpr = TFunction fd } when f.cf_set = F9MethodAccess ->
		print ctx "%s " rights;
		let h = gen_function_header ctx (Some (s_ident f.cf_name)) fd f.cf_params p in
		gen_expr ctx (block fd.tf_expr);
		h()
	| _ ->
		if ctx.curclass.cl_path = (["flash"],"Boot") && f.cf_name = "init" then 
			generate_boot_init ctx
		else if not ctx.curclass.cl_interface then begin
			print ctx "%s var %s : %s" rights (s_ident f.cf_name) (type_str ctx f.cf_type p);
			match f.cf_expr with
			| None -> ()
			| Some e ->
				print ctx " = ";
				gen_value ctx e
		end

let define_getset ctx stat f =
	let def name =
		Hashtbl.add ctx.get_sets (name,stat) f.cf_name
	in
	(match f.cf_get with MethodAccess m -> def m | _ -> ());
	(match f.cf_set with MethodAccess m -> def m | _ -> ())

let generate_class ctx c =
	ctx.curclass <- c;
	List.iter (define_getset ctx false) c.cl_ordered_fields;
	List.iter (define_getset ctx true) c.cl_ordered_statics;
	ctx.local_types <- List.map (fun (_,_,t) -> t) c.cl_types;
	let pack = open_block ctx in
	print ctx "\tpublic %s%s %s " (match c.cl_dynamic with None -> "" | Some _ -> "dynamic ") (if c.cl_interface then "interface" else "class") (snd c.cl_path);
	(match c.cl_super with
	| None -> ()
	| Some (csup,_) -> print ctx "extends %s " (s_path ctx csup.cl_path c.cl_pos));
	(match c.cl_implements with
	| [] -> ()
	| l ->
		spr ctx "implements ";
		concat ctx ", " (fun (i,_) -> print ctx "%s" (s_path ctx i.cl_path c.cl_pos)) l);
	spr ctx "{";
	let cl = open_block ctx in
	(match c.cl_constructor with
	| None -> ()
	| Some f ->
		let f = { f with
			cf_name = snd c.cl_path;
			cf_public = true;
			cf_set = F9MethodAccess;
		} in
		let fd = (match f.cf_expr with Some { eexpr = TFunction f } -> f | _ -> assert false) in
		ctx.constructor_block <- true;
		generate_field ctx false f;
		newline ctx;
		print ctx "public static function __construct__(args:Array) : %s {" (snd c.cl_path);
		newline ctx;
		print ctx "\treturn new %s(" (snd c.cl_path);
		let n = ref 0 in
		concat ctx "," (fun _ -> print ctx "args[%d]" !n; incr n) fd.tf_args;
		spr ctx ")";
		newline ctx;
		spr ctx "}";
	);
	List.iter (generate_field ctx false) c.cl_ordered_fields;
	List.iter (generate_field ctx true) c.cl_ordered_statics;
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate_main ctx c =
	ctx.curclass <- c;	
	let pack = open_block ctx in
	print ctx "\tpublic class __main__ extends %s {" (s_path ctx (["flash";"display"],"MovieClip") c.cl_pos);
	let cl = open_block ctx in
	newline ctx;
	(match c.cl_ordered_statics with
	| [{ cf_expr = Some e }] ->
		spr ctx "public function __main__() {";
		let f = open_block ctx in
		newline ctx;
		print ctx "new %s(this)" (s_path ctx (["flash"],"Boot") c.cl_pos);
		newline ctx;
		gen_value ctx e;
		f();
		newline ctx;
		spr ctx "}";
	| _ -> assert false);
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate_enum ctx e =	
	ctx.local_types <- List.map (fun (_,_,t) -> t) e.e_types;
	let pack = open_block ctx in
	let ename = snd e.e_path in
	print ctx "\tpublic class %s extends enum {" ename;
	let cl = open_block ctx in
	newline ctx;
	print ctx "public static const __isenum : Boolean = true";
	newline ctx;
	print ctx "public function %s( t : String, p : Array = null ) : void { this.tag = t; this.params = p; }" ename;	
	PMap.iter (fun _ c ->
		newline ctx;
		match c.ef_type with
		| TFun (args,_) ->
			print ctx "public static function %s(" c.ef_name;
			concat ctx ", " (fun (a,o,t) ->
				print ctx "%s : %s" a (type_str ctx t c.ef_pos);
				if o then spr ctx " = null";
			) args;
			print ctx ") : %s {" ename;
			print ctx " return new %s(\"%s\",[" ename c.ef_name;
			concat ctx "," (fun (a,_,_) -> spr ctx a) args;
			print ctx "]); }";
		| _ ->
			print ctx "public static var %s : %s = new %s(\"%s\")" c.ef_name ename ename c.ef_name;
	) e.e_constrs;
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate_base_enum ctx =
	let pack = open_block ctx in	
	spr ctx "\tpublic class enum {";
	let cl = open_block ctx in
	newline ctx;
	spr ctx "public var tag : String";
	newline ctx;
	spr ctx "public var params : Array";
	cl();
	newline ctx;
	print ctx "}";
	pack();
	newline ctx;
	print ctx "}";
	newline ctx

let generate dir types =
	let ctx = init dir ([],"enum") in
	generate_base_enum ctx;
	close ctx;
	let boot = ref None in
	let inits = ref [] in
	List.iter (fun t ->
		match t with
		| TClassDecl c ->
			let c = (match c.cl_path with
				| ["flash"],"FlashXml__" -> { c with cl_path = [],"Xml" }
				| _ -> c
			) in
			(match c.cl_init with
			| None -> ()
			| Some e -> inits := e :: !inits);			
			if c.cl_extern then
				()
			else (match c.cl_path with
			| [], "@Main" ->
				let ctx = init dir ([],"__main__") in
				generate_main ctx c;
				close ctx;
			| ["flash"], "Boot" ->
				boot := Some c;
			| _ ->				
				let ctx = init dir c.cl_path in
				generate_class ctx c;
				close ctx)
		| TEnumDecl e ->
			if e.e_extern then
				()
			else
				let ctx = init dir e.e_path in
				generate_enum ctx e;
				close ctx
		| TTypeDecl t ->
			()
	) types;
	match !boot with
	| None -> assert false
	| Some c ->
		let ctx = init dir c.cl_path in
		ctx.inits <- List.rev !inits;
		generate_class ctx c;
		close ctx
