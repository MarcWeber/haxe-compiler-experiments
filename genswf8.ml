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
open Swf
open Ast
open Type

type context = {
	(* code *)
	opcodes : actions;
	mutable code_pos : int;
	mutable stack_size : int;
	mutable opt_push : bool;
	mutable ident_count : int;

	(* management *)
	idents : (string,int) Hashtbl.t;
	types : (module_path,(string * bool)) Hashtbl.t;
	mutable statics : (string * string * texpr) list;
	mutable regs : (string,int option) PMap.t;
	mutable reg_count : int;
	mutable reg_max : int;
	mutable fun_stack : int;

	(* loops *)
	mutable cur_block : texpr list;
	mutable breaks : (unit -> unit) list;
	mutable continues : (int -> unit) list;
	mutable loop_stack : int;
}

let error p = Typer.error "Invalid expression" p
let stack_error p = Typer.error "Stack error" p

(* -------------------------------------------------------------- *)
(* Bytecode Helpers *)

type kind = 
	| VarReg of int
	| VarStr
	| VarObj
	| VarClosure

type push_style =
	| VStr of string
	| VInt of int
	| VInt32 of int32
	| VFloat of float
	| VReg of int
	| VThis
	| VNull
	| VSuper

let stack_delta = function
	| APush l -> List.length l
	| ASetReg _ -> 0
	| AAdd | ADivide | ASubtract | AMultiply | AMod | AStringAdd -> -1
	| AAnd | AOr | AXor | AShl | AShr | AAsr -> -1
	| ACompare | AGreater -> -1
	| AEval | ANot | AJump _ | AToInt | AToNumber | AToString | ATry _ | ASwap -> 0
	| ACondJump _ -> -1
	| AEqual | APhysEqual -> -1
	| ANew -> -1 (** only if 0 params **)
	| AObject | AInitArray -> 0 (** calculated outside **)
	| ASet -> -2
	| APop -> -1
	| AFunction _ | AFunction2 _ -> 1	
	| ADup -> 1
	| AWith _ -> -1
	| AObjGet -> -1
	| AObjSet -> -3
	| ALocalVar -> -1
	| ALocalAssign -> -2
	| AReturn -> -1
	| AGetURL2 _ -> -2
	| ADeleteObj | AInstanceOf | ACast -> -1
	| AExtends | AImplements -> -2
	| AEnum2 | ATrace | AThrow -> -1
	| AIncrement | ADecrement | AChr | AOrd | ARandom | ADelete | AGetTimer | ATypeOf | ATargetPath -> 0
	| AObjCall | ACall | ANewMethod -> assert false
	| AStringPool _ -> 0
	| op -> failwith ("Unknown stack delta for " ^ (ActionScript.action_string (fun _ -> "") 0 op))

let write ctx op =
	let write b op =
		DynArray.add ctx.opcodes op;
		ctx.code_pos <- ctx.code_pos + 1;
		ctx.stack_size <- ctx.stack_size + stack_delta op;
		ctx.opt_push <- b
	in
	match op with
	| APush l when ctx.opt_push ->
		(match DynArray.last ctx.opcodes with
		| (APush l2) as a ->
			ctx.code_pos <- ctx.code_pos - 1;
			ctx.stack_size <- ctx.stack_size - stack_delta a;
			DynArray.delete_last ctx.opcodes;
			write true (APush (l2 @ l))
		| _ ->
			assert false)
	| APush _ ->
		write true op
	| _ ->
		write false op

let call ctx kind n =
	let op , n = (match kind with
		| VarReg r ->
			write ctx (APush [PReg r;PUndefined]);
			AObjCall , n + 2
		| VarStr -> 
			ACall , n + 1
		| VarClosure | VarObj ->
			AObjCall , n + 2
	) in
	DynArray.add ctx.opcodes op;
	ctx.opt_push <- false;
	ctx.code_pos <- ctx.code_pos + 1;
	ctx.stack_size <- ctx.stack_size - n

let new_call ctx kind n  =
	let op , n = (match kind with
		| VarReg r ->
			write ctx (APush [PReg r;PUndefined]);
			ANewMethod , n + 2
		| VarStr -> 
			ANew , n + 1
		| VarClosure | VarObj ->
			ANewMethod , n + 2
	) in
	DynArray.add ctx.opcodes op;
	ctx.opt_push <- false;
	ctx.code_pos <- ctx.code_pos + 1;
	ctx.stack_size <- ctx.stack_size - n

let push ctx items =
	write ctx (APush (List.map (fun i ->
		match i with
		| VStr str ->
			let n = (try
				Hashtbl.find ctx.idents str
			with Not_found ->
				let n = ctx.ident_count in
				ctx.ident_count <- n + 1;
				Hashtbl.add ctx.idents str n;
				n
			) in
			if n <= 0xFF then 
				PStack n
			else
				PStack2 n
		| VInt n ->
			PInt (Int32.of_int n)
		| VInt32 n ->
			PInt n
		| VFloat f ->
			PDouble f
		| VThis ->
			PReg 1
		| VNull ->
			PNull
		| VSuper ->
			PReg 2
		| VReg n ->
			PReg n
	) items))

let pop ctx n dec =	
	let rec loop n =
		if n <> 0 then begin
			write ctx APop;
			loop (n - 1)
		end;
	in
	if n < 0 then assert false;
	let old_s = ctx.stack_size in
	loop n;
	if not dec then ctx.stack_size <- old_s

let cjmp ctx =
	write ctx (ACondJump 0);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.opcodes - 1 in
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		DynArray.set ctx.opcodes op_pos (ACondJump delta);
		ctx.opt_push <- false
	)

let jmp ctx =
	write ctx (AJump 0);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.opcodes - 1 in
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		DynArray.set ctx.opcodes op_pos (AJump delta);
		ctx.opt_push <- false
	)

let pos ctx =
	ctx.opt_push <- false;
	let start_pos = ctx.code_pos in
	(fun ~cond ->
		let delta = start_pos - (ctx.code_pos + 1) in
		write ctx (if cond then ACondJump delta else AJump delta);
	)

let jmp_pos ctx cond =
	write ctx (AJump 0);
	let start_pos = ctx.code_pos in
	let op_pos = DynArray.length ctx.opcodes - 1 in
	(fun pos ->
		let delta = pos - start_pos in
		DynArray.set ctx.opcodes op_pos (if cond then ACondJump delta else AJump delta);
		ctx.opt_push <- false
	)
	

let setvar ?(retval=false) ctx = function
	| VarReg (-1) -> assert false (** true, false, null **)
	| VarReg n -> write ctx (ASetReg n); if not retval then write ctx APop
	| VarStr
	| VarObj
	| VarClosure as s -> 
		if retval then write ctx (ASetReg 0);
		write ctx (if s = VarStr then ASet else AObjSet);
		if retval then push ctx [VReg 0]

let getvar ctx = function
	| VarReg (-1) -> () (** true, false, null **)
	| VarReg n -> push ctx [VReg n]
	| VarStr -> write ctx AEval
	| VarObj -> write ctx AObjGet
	| VarClosure ->
		push ctx [VInt 2; VStr "@closure"];
		call ctx VarStr 2

let func ctx need_super need_args args =
	let default_flags = ThisRegister :: (if need_args then [] else [ArgumentsNoVar]) in
	let f = {
		f2_name = "";
		f2_args = args;
		f2_codelen = 0;
		f2_nregs = 0;
		f2_flags = (if need_super then SuperRegister :: default_flags else SuperNoVar :: default_flags);
	} in
	write ctx (AFunction2 f);
	let start_pos = ctx.code_pos in
	let old_stack = ctx.fun_stack in
	let old_rmax = ctx.reg_max in
	ctx.fun_stack <- ctx.stack_size;
	ctx.reg_max <- ctx.reg_count;
	(fun() ->
		let delta = ctx.code_pos - start_pos in
		f.f2_codelen <- delta;
		f.f2_nregs <- ctx.reg_max + 1;
		if ctx.fun_stack <> ctx.stack_size then assert false;
		ctx.fun_stack <- old_stack;
		ctx.reg_max <- old_rmax;
	)

let open_block ctx =
	let old_regs = ctx.regs in
	let old_rcount = ctx.reg_count in
	let old_block = ctx.cur_block in
	(fun() ->
		ctx.regs <- old_regs;
		ctx.reg_count <- old_rcount;
		ctx.cur_block <- old_block;
	)

let begin_loop ctx =
	let old_breaks = ctx.breaks in
	let old_cont = ctx.continues in
	ctx.breaks <- [];
	ctx.continues <- [];
	ctx.loop_stack <- ctx.stack_size;
	(fun pos ->		
		List.iter (fun f -> f()) ctx.breaks;
		List.iter (fun f -> f pos) ctx.continues;
		ctx.breaks <- old_breaks;
		ctx.continues <- old_cont;
	)

let alloc_reg ctx =
	ctx.reg_count <- ctx.reg_count + 1;
	if ctx.reg_count > ctx.reg_max then ctx.reg_max <- ctx.reg_count;
	ctx.reg_count

let best_eq t = 
	match follow t with
	| TMono _
	| TDynamic _
	| TInst ({ cl_path = ([],"String") },_) ->
		AEqual
	| TInst _
	| TEnum _
	| TFun _
	| TAnon _ ->
		APhysEqual

(* -------------------------------------------------------------- *)
(* Generation Helpers *)

let idents_cache = Hashtbl.create 0

let cfind flag cst e =
	let vname = (match cst with TConst TSuper -> "super" | TLocal v -> v | _ -> assert false) in
	let rec loop2 e =
		match e.eexpr with
		| TFunction _ -> ()
		| TBlock _ ->
			(try
				iter loop2 e;
			with
				Not_found -> ())
		| TVars vl ->
			List.iter (fun (v,t,e) ->
				(match e with
				| None -> ()
				| Some e -> loop2 e);
				if v = vname then raise Not_found;
			) vl
		| TConst TSuper ->
			if vname = "super" then raise Exit
		| TLocal v ->
			if v = vname then raise Exit
		| _ ->
			iter loop2 e
	in
	let rec loop e =
		match e.eexpr with
		| TFunction f ->
			if not (List.exists (fun (a,_) -> a = vname) f.tf_args) then loop2 f.tf_expr
		| TBlock _ ->
			(try
				iter loop e;
			with
				Not_found -> ())
		| TVars vl ->
			List.iter (fun (v,t,e) ->
				(match e with
				| None -> ()
				| Some e -> loop e);
				if v = vname then raise Not_found;
			) vl
		| _ ->
			iter loop e
	in
	try
		(if flag then loop2 else loop) e;
		false
	with
		Exit -> 
			true

let define_var ctx v ef exprs =
	if List.exists (cfind false (TLocal v)) ctx.cur_block then begin
		push ctx [VStr v];
		ctx.regs <- PMap.add v None ctx.regs;
		match ef with
		| None -> 
			write ctx ALocalVar
		| Some f ->
			f();
			write ctx ALocalAssign
	end else begin
		let r = alloc_reg ctx in
		ctx.regs <- PMap.add v (Some r) ctx.regs;
		match ef with
		| None -> ()
		| Some f ->
			f();
			setvar ctx (VarReg r)
	end

let gen_ident =
	let rand_char() =
		let n = Random.int 62 in
		if n < 26 then Char.chr (n + int_of_char 'a') else
		if n < 52 then Char.chr (n - 26 + int_of_char 'A') else
		Char.chr (n - 52 + int_of_char '0')
	in
	let rec loop() =
		let c = String.create 3 in
		let pos = [|[|0;1;2|];[|0;2;1|];[|1;2;0|]|].(Random.int 3) in
		c.[pos.(0)] <- rand_char();
		c.[pos.(1)] <- rand_char();
		c.[pos.(2)] <- '@';
		if Hashtbl.mem idents_cache c then
			loop()
		else begin
			Hashtbl.add idents_cache c ();
			c
		end;
	in
	loop

let gen_type ctx t extern =
	try
		let id , e = Hashtbl.find ctx.types t in
		if e <> extern then assert false;
		id
	with
		Not_found ->
			let id = gen_ident() in
			Hashtbl.add ctx.types t (id,extern);
			id

let no_value ctx retval =
	(* does not push a null but still increment the stack like if
	   a real value was pushed *)
	if retval then ctx.stack_size <- ctx.stack_size + 1

(* -------------------------------------------------------------- *)
(* Generation *)

let unescape_chars s p = 
	let b = Buffer.create 0 in
	let rec loop esc i =
		if i = String.length s then
			()
		else
			let c = s.[i] in
			if esc then begin
				let inext = ref (i + 1) in
				(match c with
				| 'n' -> Buffer.add_char b '\n'
				| 'r' -> Buffer.add_char b '\r'
				| 't' -> Buffer.add_char b '\t'
				| '"' | '\'' | '\\' -> Buffer.add_char b c
				| '0'..'3' ->
					let c = (try
						char_of_int (int_of_string ("0o" ^ String.sub s i 3))
					with _ ->
						raise (Lexer.Error (Lexer.Invalid_character c,p))
					) in
					Buffer.add_char b c;
					inext := !inext + 2;
				| 'x' ->
					let c = (try
						char_of_int (int_of_string ("0x" ^ String.sub s (i+1) 2))
					with _ ->
						raise (Lexer.Error (Lexer.Invalid_character c,p))
					) in
					Buffer.add_char b c;
					inext := !inext + 2;
				| _ -> raise (Lexer.Error (Lexer.Invalid_character c,p)));
				loop false !inext;
			end else
				match c with
				| '\\' -> loop true (i + 1)
				| c ->
					Buffer.add_char b c;
					loop false (i + 1)
	in
	loop false 0;
	Buffer.contents b

let rec gen_constant ctx c p =
	match c with
	| TInt s -> (try push ctx [VInt32 (Int32.of_string s)] with _ -> gen_constant ctx (TFloat s) p)
	| TFloat s -> push ctx [VFloat (try float_of_string s with _ -> error p)]
	| TString s -> push ctx [VStr (unescape_chars s p)]
	| TBool b -> write ctx (APush [PBool b])
	| TNull -> push ctx [VNull]
	| TDone -> push ctx [VStr "@done"]
	| TThis
	| TSuper -> assert false

let access_local ctx s =
	match (try PMap.find s ctx.regs with Not_found -> None) with
	| None ->
		push ctx [VStr s];
		VarStr
	| Some r ->
		VarReg r

let rec gen_access ctx forcall e =
	match e.eexpr with
	| TConst TSuper ->
		(* for superconstructor *)
		if forcall then begin
			push ctx [VSuper];
			write ctx (APush [PUndefined]);
			VarObj
		end else
			VarReg 2
	| TConst TThis ->
		VarReg 1
	| TMember f ->
		push ctx [VReg 1; VStr f];
		(match follow e.etype with
		| TFun _ -> VarClosure
		| _ -> VarObj)
	| TLocal "__arguments__" ->
		push ctx [VStr "arguments"];
		VarStr
	| TLocal s ->
		access_local ctx s
	| TField (e2,f) ->
		gen_expr ctx true e2;
		push ctx [VStr f];
		(match follow e.etype with
		| TFun _ -> VarClosure
		| _ -> VarObj)
	| TArray (ea,eb) ->
		gen_expr ctx true ea;
		gen_expr ctx true eb;
		VarObj
	| TEnumField (en,f) ->
		push ctx [VStr (gen_type ctx en.e_path false)];
		write ctx AEval;
		push ctx [VStr f];
		(match follow e.etype with
		| TFun _ -> VarClosure
		| _ -> VarObj)
	| TType t ->
		push ctx [VStr (match t with
			| TClassDecl c -> gen_type ctx c.cl_path c.cl_extern
			| TEnumDecl e -> gen_type ctx e.e_path false
		)];
		VarStr
	| _ ->
		if not forcall then error e.epos;
		gen_expr ctx true e;
		write ctx (APush [PUndefined]);
		VarObj

and gen_try_catch ctx retval e catchs =
	let tdata = {
		tr_style = TryRegister 0;
		tr_trylen = 0;
		tr_catchlen = None;
		tr_finallylen = None;
	} in
	write ctx (ATry tdata);
	let start = ctx.code_pos in
	gen_expr ctx retval e;
	let jump_end = jmp ctx in
	tdata.tr_trylen <- ctx.code_pos - start;
	let start = ctx.code_pos in
	let end_throw = ref true in
	let jumps = List.map (fun (name,t,e) ->	
		if not !end_throw then
			(fun () -> ())
		else let t = (match follow t with
			| TEnum (e,_) -> Some (TEnumDecl e)
			| TInst (c,_) -> Some (TClassDecl c)
			| TFun _
			| TAnon _ ->
				assert false
			| TMono _
			| TDynamic _ ->
				None
		) in
		let next_catch = (match t with
		| None -> 
			end_throw := false;
			push ctx [VStr name;VReg 0];
			write ctx ALocalAssign;
			gen_expr ctx retval e;			
			(fun() -> ())
		| Some t ->
			getvar ctx (gen_access ctx false (mk (TType t) (mk_mono()) e.epos));
			push ctx [VReg 0; VInt 2; VStr "@instanceof"];
			call ctx VarStr 2;
			write ctx ANot;
			let c = cjmp ctx in
			push ctx [VStr name; VReg 0];
			write ctx ALocalAssign;
			gen_expr ctx retval e;
			c
		) in
		if retval then ctx.stack_size <- ctx.stack_size - 1;
		let j = jmp ctx in
		next_catch();
		j
	) catchs in
	if !end_throw && catchs <> [] then begin
		push ctx [VReg 0];
		write ctx AThrow;
	end;
	if catchs <> [] then tdata.tr_catchlen <- Some (ctx.code_pos - start);
	List.iter (fun j -> j()) jumps;
	jump_end();

and gen_switch ctx retval e cases def =
	gen_expr ctx true e;
	let r = alloc_reg ctx in
	write ctx (ASetReg r);
	let rec loop = function
		| [] -> 
			write ctx APop;
			[]
		| [(e,x)] ->
			gen_expr ctx true e;
			write ctx (best_eq e.etype);
			[cjmp ctx,x]
		| (e,x) :: l ->
			gen_expr ctx true e;
			write ctx (best_eq e.etype);
			let j = cjmp ctx in
			push ctx [VReg r];
			(j,x) :: loop l
	in
	let dispatch = loop cases in
	(match def with
	| None -> if retval then push ctx [VNull]
	| Some e -> gen_expr ctx retval e);
	let jend = jmp ctx in
	let jends = List.map (fun (j,e) ->
		j();
		gen_expr ctx retval e;
		if retval then ctx.stack_size <- ctx.stack_size - 1;
		jmp ctx;
	) dispatch in		
	jend();
	List.iter (fun j -> j()) jends

and gen_match ctx retval e cases def =
	gen_expr ctx true e;
	let renum = alloc_reg ctx in
	write ctx (ASetReg renum);
	push ctx [VInt 0];
	write ctx AObjGet;
	let rtag = alloc_reg ctx in
	write ctx (ASetReg rtag);
	let gen_match e x =
		match e.eexpr with
		| TMatch (e,constr,args) ->
			push ctx [VStr constr];
			write ctx APhysEqual;
			args
		| _ ->
			assert false
	in
	let rec loop = function
		| [] -> 
			write ctx APop;
			[]
		| [(e,x)] ->
			let args = gen_match e x in
			[cjmp ctx,args,x]
		| (e,x) :: l ->
			let args = gen_match e x in
			let j = cjmp ctx in
			push ctx [VReg rtag];
			(j,args,x) :: loop l
	in
	let dispatch = loop cases in
	(match def with
	| None -> if retval then push ctx [VNull]
	| Some e -> gen_expr ctx retval e);
	let jend = jmp ctx in
	let jends = List.map (fun (j,args,e) ->
		let regs = ctx.regs in
		let nregs = ctx.reg_count in
		j();
		let n = ref 0 in
		List.iter (fun (a,t) ->
			incr n;
			define_var ctx a (Some (fun() ->
				push ctx [VReg renum; VInt !n];
				write ctx AObjGet
			)) [e]
		) (match args with None -> [] | Some l -> l);
		gen_expr ctx retval e;
		if retval then ctx.stack_size <- ctx.stack_size - 1;
		ctx.regs <- regs;
		ctx.reg_count <- nregs;
		jmp ctx;
	) dispatch in
	jend();
	List.iter (fun j -> j()) jends

and gen_binop ctx retval op e1 e2 =
	let gen a =
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write ctx a
	in
	match op with
	| OpAssign ->
		let k = gen_access ctx false e1 in
		gen_expr ctx true e2;
		setvar ~retval ctx k
	| OpAssignOp op ->
		let k = gen_access ctx false e1 in
		gen_binop ctx true op e1 e2;
		setvar ~retval ctx k
	| OpAdd -> gen AAdd
	| OpMult -> gen AMultiply
	| OpDiv -> gen ADivide
	| OpSub -> gen ASubtract
	| OpEq -> gen (best_eq e1.etype)
	| OpPhysEq -> gen APhysEqual
	| OpPhysNotEq ->
		gen APhysEqual;
		write ctx ANot
	| OpNotEq -> 
		gen (best_eq e1.etype);
		write ctx ANot
	| OpGt -> gen AGreater
	| OpGte ->
		gen ACompare;
		write ctx ANot
	| OpLt -> gen ACompare
	| OpLte ->
		gen AGreater;
		write ctx ANot
	| OpAnd -> gen AAnd
	| OpOr -> gen AOr
	| OpXor -> gen AXor
	| OpBoolAnd ->
		gen_expr ctx true e1;
		write ctx ADup;
		write ctx ANot;
		let jump_end = cjmp ctx in
		write ctx APop;
		gen_expr ctx true e2;
		jump_end()
	| OpBoolOr ->
		gen_expr ctx true e1;
		write ctx ADup;
		let jump_end = cjmp ctx in
		write ctx APop;
		gen_expr ctx true e2;
		jump_end()
	| OpShl -> gen AShl
	| OpShr -> gen AShr
	| OpUShr -> gen AAsr
	| OpMod -> gen AMod
	| OpInterval ->
		(* handled by typer *)
		assert false

and gen_unop ctx retval op flag e =
	match op with
	| Not -> 
		gen_expr ctx true e;
		write ctx ANot
	| Neg ->
		push ctx [VInt 0];
		gen_expr ctx true e;
		write ctx ASubtract
	| NegBits ->
		gen_expr ctx true e;
		push ctx [VInt (-1)]; 
		write ctx AXor
	| Increment
	| Decrement ->
		if retval && flag = Postfix then begin
			let k = gen_access ctx false e in
			getvar ctx k
		end;
		ignore(gen_access ctx false e);
		let k = gen_access ctx false e in
		getvar ctx k;
		write ctx (match op with Increment -> AIncrement | Decrement -> ADecrement | _ -> assert false);
		setvar ~retval:(retval && flag = Prefix) ctx k

and gen_call ctx e el =
	match e.eexpr, el with
	| TLocal "__instanceof__" ,  [e1;e2] ->
		gen_expr ctx true e1;
		gen_expr ctx true e2;
		write ctx AInstanceOf
	| TLocal "__typeof__" , [e] ->
		gen_expr ctx true e;
		write ctx ATypeOf
	| TLocal "__new__", e :: el ->
		let nargs = List.length el in
		List.iter (gen_expr ctx true) el;
		push ctx [VInt nargs];
		let k = gen_access ctx true e in
		new_call ctx k nargs
	| _ , _ ->
		let nargs = List.length el in
		List.iter (gen_expr ctx true) (List.rev el);
		push ctx [VInt nargs];
		let k = gen_access ctx true e in
		call ctx k nargs


and gen_expr_2 ctx retval e =
	match e.eexpr with
	| TConst TSuper
	| TConst TThis
	| TField _
	| TArray _
	| TLocal _
	| TMember _ 
	| TType _
	| TEnumField _ ->	
		getvar ctx (gen_access ctx false e)
	| TConst c ->
		gen_constant ctx c e.epos
	| TParenthesis e ->
		gen_expr ctx retval e
	| TBlock el ->
		let rec loop = function
			| [] ->
				if retval then push ctx [VNull]
			| [e] -> 
				ctx.cur_block <- [];
				gen_expr ctx retval e
			| e :: l -> 
				ctx.cur_block <- l;
				gen_expr ctx false e;
				loop l
		in
		let b = open_block ctx in
		loop el;
		b()	
	| TVars vl ->
		List.iter (fun (v,t,e) ->
			define_var ctx v (match e with None -> None | Some e -> Some (fun() -> gen_expr ctx true e)) ctx.cur_block
		) vl;
		if retval then push ctx [VNull]
	| TArrayDecl el ->
		let nitems = List.length el in
		List.iter (gen_expr ctx true) (List.rev el);
		push ctx [VInt nitems];
		write ctx AInitArray;
		ctx.stack_size <- ctx.stack_size - nitems;
	| TObjectDecl fl ->
		let nfields = List.length fl in
		List.iter (fun (s,v) ->
			push ctx [VStr s];
			gen_expr ctx true v
		) fl;
		push ctx [VInt nfields];
		write ctx AObject;
		ctx.stack_size <- ctx.stack_size - (nfields * 2);
	| TFunction f ->
		let block = open_block ctx in
		let reg_super = cfind true (TConst TSuper) f.tf_expr in
		ctx.regs <- PMap.empty;
		ctx.reg_count <- (if reg_super then 2 else 1);
		let rargs = List.map (fun (a,t) ->
			let no_reg = cfind false (TLocal a) f.tf_expr in
			if no_reg then begin
				ctx.regs <- PMap.add a None ctx.regs;
				0 , a
			end else begin 
				let r = alloc_reg ctx in
				ctx.regs <- PMap.add a (Some r) ctx.regs;
				r , ""
			end
		) f.tf_args in
		let tf = func ctx reg_super (cfind true (TLocal "__arguments__") f.tf_expr) rargs in
		gen_expr ctx false f.tf_expr;
		tf();
		block();
	| TIf (cond,e,None) ->
		if retval then assert false;
		gen_expr ctx true cond;
		write ctx ANot;
		let j = cjmp ctx in
		gen_expr ctx retval e;
		j()
	| TIf (cond,e,Some e2) ->
		gen_expr ctx true cond;
		let j = cjmp ctx in
		gen_expr ctx retval e2;
		let jend = jmp ctx in
		j();
		gen_expr ctx retval e;
		if retval then ctx.stack_size <- ctx.stack_size - 1;
		jend()
	| TWhile (cond,e,Ast.NormalWhile) ->
		let loop_end = begin_loop ctx in
		let cont_pos = ctx.code_pos in
		let loop = pos ctx in
		gen_expr ctx true cond;
		write ctx ANot;
		let jend = cjmp ctx in
		gen_expr ctx false e;
		loop false;
		jend();
		loop_end cont_pos
	| TWhile (cond,e,Ast.DoWhile) ->
		let loop_end = begin_loop ctx in
		let p = pos ctx in
		gen_expr ctx false e;
		gen_expr ctx true cond;
		p true;
		loop_end ctx.code_pos
	| TReturn None ->
		pop ctx (ctx.stack_size - ctx.fun_stack) false;
		write ctx (APush [PUndefined]);
		write ctx AReturn;
		no_value ctx retval
	| TReturn (Some e) ->
		pop ctx (ctx.stack_size - ctx.fun_stack) false;
		gen_expr ctx true e;
		write ctx AReturn;
		no_value ctx retval
	| TBreak ->
		pop ctx (ctx.stack_size - ctx.loop_stack) false;
		ctx.breaks <- jmp ctx :: ctx.breaks;
		no_value ctx retval
	| TContinue ->
		pop ctx (ctx.stack_size - ctx.loop_stack) false;
		ctx.continues <- jmp_pos ctx false :: ctx.continues;
		no_value ctx retval
	| TCall (e,el) ->
		gen_call ctx e el
	| TNew (c,_,el) ->
		let nargs = List.length el in
		List.iter (gen_expr ctx true) (List.rev el);
		push ctx [VInt nargs];
		push ctx [VStr (gen_type ctx c.cl_path c.cl_extern)];
		new_call ctx VarStr nargs
	| TSwitch (e,cases,def) ->
		let is_enum = cases <> [] && List.for_all (fun (e,_) -> match e.eexpr with TMatch _ -> true | _ -> false) cases in
		(if is_enum then gen_match else gen_switch) ctx retval e cases def
	| TThrow e ->
		gen_expr ctx true e;
		write ctx AThrow;
		no_value ctx retval
	| TTry (e,catchs) ->
		gen_try_catch ctx retval e catchs
	| TBinop (op,e1,e2) ->
		gen_binop ctx retval op e1 e2
	| TUnop (op,flag,e) ->
		gen_unop ctx retval op flag e
	| TMatch _ ->
		(* done : only in switch *)
		assert false
	| TFor (v,it,e) ->
		gen_expr ctx true it;
		let r = alloc_reg ctx in
		write ctx (ASetReg r);
		write ctx APop;
		let loop_end = begin_loop ctx in
		let cont_pos = ctx.code_pos in
		let j_begin = pos ctx in
		push ctx [VInt 0; VReg r; VStr "hasNext"];
		call ctx VarObj 0;
		write ctx ANot;
		let j_end = cjmp ctx in
		define_var ctx v (Some (fun() -> 
			push ctx [VInt 0; VReg r; VStr "next"];
			call ctx VarObj 0;
		)) ctx.cur_block;
		gen_expr ctx false e;
		j_begin false;
		j_end();
		loop_end cont_pos;
		if retval then getvar ctx (access_local ctx v)

and gen_expr ctx retval e =
	let old = ctx.stack_size in
	gen_expr_2 ctx retval e;
	if old <> ctx.stack_size then begin
		if old + 1 <> ctx.stack_size then stack_error e.epos;
		if not retval then write ctx APop;
	end else if retval then stack_error e.epos

let gen_class_static_field ctx cclass f =
	if f.cf_name <> "new" then
	match f.cf_expr with
	| None -> ()
	| Some e ->
		match e.eexpr with
		| TFunction _ ->
			push ctx [VReg 0; VStr f.cf_name];
			gen_expr ctx true e;
			setvar ctx VarObj
		| _ ->
			ctx.statics <- (cclass,f.cf_name,e) :: ctx.statics

let gen_class_static_init ctx (cclass,name,e) =
	push ctx [VStr cclass];
	write ctx AEval;
	push ctx [VStr name];
	gen_expr ctx true e;
	setvar ctx VarObj

let gen_class_field ctx f =
	match f.cf_expr with
	| None -> ()
	| Some e ->
		push ctx [VReg 1; VStr f.cf_name];
		gen_expr ctx true e;
		setvar ctx VarObj

let gen_enum_field ctx f =
	let ename = mk (TConst (TString f.ef_name)) f.ef_type Ast.null_pos in
	push ctx [VReg 0; VStr f.ef_name];
	(match follow f.ef_type with
	| TFun (args,r) ->
		let n = ref 0 in
		let args = List.map (fun t ->
			incr n;
			"p" ^ string_of_int (!n), t
		) args in
		let e = mk (TReturn (Some (mk (TArrayDecl (ename :: 
			List.map (fun (n,t) -> mk (TLocal n) t Ast.null_pos) args
		)) r Ast.null_pos))) (mk_mono()) Ast.null_pos in
		let fdat = {
			tf_args = args;
			tf_type = r;
			tf_expr = e;
		} in
		gen_expr ctx true (mk (TFunction fdat) (mk_mono()) Ast.null_pos);
	| t ->
		gen_expr ctx true (mk (TArrayDecl [ename]) t Ast.null_pos));
	write ctx AObjSet

let gen_path ctx (p,t) =
	match p with
	| [] ->
		push ctx [VStr t];
		write ctx AEval
	| p :: l -> 
		push ctx [VStr p];
		write ctx AEval;
		List.iter (fun p ->
			push ctx [VStr p];
			write ctx AObjGet;
		) l;
		push ctx [VStr t];
		write ctx AObjGet

let gen_type_def ctx t tdef =
	match tdef with
	| TClassDecl c ->
		if c.cl_extern || c.cl_interface then 
			()
		else
		let id = gen_type ctx t false in
		push ctx [VStr id];
		(try 
			let constr = PMap.find "new" c.cl_statics in
			(match constr.cf_expr with
			| Some ({ eexpr = TFunction _ } as e) -> gen_expr ctx true e
			| _ -> raise Not_found);
		with Not_found ->
			let f = func ctx true false [] in
			f()
		);
		write ctx (ASetReg 0);
		setvar ctx VarStr;
		(match c.cl_super with
		| None -> ()
		| Some (csuper,_) ->
			push ctx [VReg 0];
			if csuper.cl_extern then 
				gen_path ctx csuper.cl_path
			else 
				let id = gen_type ctx csuper.cl_path false in
				push ctx [VStr id];
				write ctx AEval;
			write ctx AExtends);
		push ctx [VReg 0; VStr "prototype"];
		getvar ctx VarObj;
		write ctx (ASetReg 1);
		write ctx APop;
		PMap.iter (fun _ f -> gen_class_static_field ctx id f) c.cl_statics;
		PMap.iter (fun _ f -> gen_class_field ctx f) c.cl_fields;
	| TEnumDecl e ->
		let id = gen_type ctx t false in
		push ctx [VStr id; VInt 0; VStr "Object"];
		write ctx ANew;
		write ctx (ASetReg 0);
		setvar ctx VarStr;
		PMap.iter (fun _ f -> gen_enum_field ctx f) e.e_constrs

let gen_boot ctx m =
	let id = gen_type ctx ([],"Boot") false in
	(* r0 = Boot *)
	push ctx [VStr id];
	write ctx AEval;
	write ctx (ASetReg 0);
	write ctx APop;
	(* r0._global = eval("_global") *)
	push ctx [VReg 0; VStr "_global"; VStr "_global"];
	write ctx AEval;
	write ctx AObjSet;
	(* r0._root = eval("_root") *)
	push ctx [VReg 0; VStr "_root"; VStr "_root"];
	write ctx AEval;
	write ctx AObjSet;
	(* r0.current = eval("this") *)
	push ctx [VReg 0; VStr "current"; VStr "this"];
	write ctx AEval;
	write ctx AObjSet;
	(* Boot.__init() *)
	push ctx [VInt 0; VReg 0; VStr "__init"];
	call ctx VarObj 0;
	write ctx APop

let gen_type_map ctx =
	let packs = Hashtbl.create 0 in
	let rec loop acc cur = function
		| [] ->
			(if cur = "" then 
				VarStr
			else begin 
				push ctx [VStr cur];
				write ctx AEval;
				VarObj
			end)
		| p :: l ->
			let acc = p :: acc in
			try
				loop acc (Hashtbl.find packs acc) l
			with
				Not_found ->
					let id = (if cur = "" then
						p
					else begin
						let id = gen_ident() in
						push ctx [VStr id; VStr cur];
						write ctx AEval;
						push ctx [VStr p];
						write ctx AObjGet;
						write ctx ASet;
						id
					end) in
					Hashtbl.add packs acc id;
					push ctx [VStr id];
					write ctx AEval;
					let defined = cjmp ctx in
					push ctx [VStr id; VInt 0; VStr "Object"];
					write ctx ANew;
					write ctx ASet;
					if cur <> "" then begin
						push ctx [VStr cur];
						write ctx AEval;
						push ctx [VStr p; VStr id];
						write ctx AEval;
						write ctx AObjSet;
					end;
					defined();
					loop acc id l
	in
	Hashtbl.iter (fun (p,t) (n,ext) ->
		if ext then begin
			push ctx [VStr n];
			gen_path ctx (p,t);
			write ctx ASet
		end else begin
			let k = loop [] "" p in
			push ctx [VStr t;VStr n];
			write ctx AEval;
			setvar ctx k
		end
	) ctx.types

let to_utf8 str =
	try
		UTF8.validate str;
		str;
	with
		UTF8.Malformed_code -> 
			let b = UTF8.Buf.create 0 in
			String.iter (fun c -> UTF8.Buf.add_char b (UChar.of_char c)) str;
			UTF8.Buf.contents b

let generate file ver modules =
	let ctx = {
		opcodes = DynArray.create();
		code_pos = 0;
		stack_size = 0;
		ident_count = 0;
		opt_push = false;
		idents = Hashtbl.create 0;
		types = Hashtbl.create 0;
		regs = PMap.empty;
		reg_count = 0;
		reg_max = 0;
		cur_block = [];
		breaks = [];
		continues = [];
		loop_stack = 0;
		fun_stack = 0;
		statics = [];
	} in
	write ctx (AStringPool []);
	let boot = ref None in
	List.iter (fun m ->
		if m.mpath = ([],"Boot") then boot := Some m;
		List.iter (fun (p,t) -> gen_type_def ctx p t) m.mtypes
	) modules;
	gen_type_map ctx;
	gen_boot ctx (match !boot with None -> assert false | Some m -> m);
	List.iter (gen_class_static_init ctx) (List.rev ctx.statics);
	let idents = ctx.idents in
	let idents = Hashtbl.fold (fun ident pos acc -> (ident,pos) :: acc) idents [] in
	let idents = List.sort (fun (_,p1) (_,p2) -> compare p1 p2) idents in
	DynArray.set ctx.opcodes 0 (AStringPool (List.map (fun (id,_) -> to_utf8 id) idents));
	let w = 400 in
	let h = 300 in
	let fps = 20. in
	let bg = 0xFFFFFF in
	let header = {
		h_version = ver;
		h_size = {
			rect_nbits = if (max w h) >= 820 then 16 else 15;
			left = 0;
			top = 0;
			right = w * 20;
			bottom = h * 20;
		};
		h_frame_count = 1;
		h_fps = to_float16 fps;
		h_compressed = true;
	} in
	let tag ?(ext=false) d = {
		tid = 0;
		textended = ext;
		tdata = d;
	} in
	let tagbg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
	let tagcode = tag (TDoAction ctx.opcodes) in
	let tagshow = tag TShowFrame in
	let ch = IO.output_channel (open_out_bin file) in
	Swf.write ch (header,[tagbg;tagcode;tagshow]);
	IO.close_out ch

;;
SwfParser.init SwfZip.inflate SwfZip.deflate;
Swf.warnings := false;
