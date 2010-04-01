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
open As3
open As3hl
open Genswf9
open Type
open Common

(* --- MINI ZIP IMPLEMENTATION --- *)


type zfile = {
	fname : string;
	fcompressed : bool;
	fclen : int;
	fsize : int;
	fcrc : int32;
	fdate : float;
}

type t = {
	ch : unit IO.output;
	mutable files : zfile list;
	mutable cdr_size : int;
	mutable cdr_offset : int;
}

let zip_create o = {
	ch = IO.cast_output o;
	files = [];
	cdr_size = 0;
	cdr_offset = 0;
}

let make_crc32 data =
	let init = 0xFFFFFFFFl in
	let polynom = 0xEDB88320l in
	let crc = ref init in
	for i = 0 to String.length data - 1 do
		let b = Int32.of_int (int_of_char (String.unsafe_get data i)) in
		let tmp = ref (Int32.logand (Int32.logxor (!crc) b) 0xFFl) in
		for j = 0 to 7 do
			tmp := if Int32.to_int (Int32.logand (!tmp) 1l) == 1 then
				Int32.logxor (Int32.shift_right_logical (!tmp) 1) polynom
			else
				Int32.shift_right_logical (!tmp) 1;
		done;
		crc := Int32.logxor (Int32.shift_right_logical (!crc) 8) (!tmp);
	done;
	Int32.logxor (!crc) init

let zip_write_date z d =
	let t = Unix.localtime d in
	let hour = t.Unix.tm_hour in
	let min = t.Unix.tm_min in
	let sec = t.Unix.tm_sec lsr 1 in
	IO.write_ui16 z.ch ((hour lsl 11) lor (min lsl 5) lor sec);
	let year = t.Unix.tm_year - 80 in
	let month = t.Unix.tm_mon + 1 in
	let day = t.Unix.tm_mday in
	IO.write_ui16 z.ch ((year lsl 9) lor (month lsl 5) lor day)

let zip_write_file z name data date compress =
	IO.write_i32 z.ch 0x04034B50;
	IO.write_ui16 z.ch 0x0014; (* version *)
	IO.write_ui16 z.ch 0;
	let crc32 = make_crc32 data in
	let cdata = if compress then
		let d = Extc.zip data in
		String.sub d 2 (String.length d - 4)
	else
		data
	in
	IO.write_ui16 z.ch (if compress then 0x08 else 0x00);
	zip_write_date z date;
	IO.write_real_i32 z.ch crc32;
	IO.write_i32 z.ch (String.length cdata);
	IO.write_i32 z.ch (String.length data);
	IO.write_ui16 z.ch (String.length name);
	IO.write_ui16 z.ch 0;
	IO.nwrite z.ch name;
	IO.nwrite z.ch cdata;
	z.files <- {
		fname = name;
		fcompressed = compress;
		fclen = String.length cdata;
		fsize = String.length data;
		fcrc = crc32;
		fdate = date;
	} :: z.files

let zip_write_cdr_file z f =
	let namelen = String.length f.fname in
	IO.write_i32 z.ch 0x02014B50;
	IO.write_ui16 z.ch 0x0014;
	IO.write_ui16 z.ch 0x0014;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch (if f.fcompressed then 0x08 else 0);
	zip_write_date z f.fdate;
	IO.write_real_i32 z.ch f.fcrc;
	IO.write_i32 z.ch f.fclen;
	IO.write_i32 z.ch f.fsize;
	IO.write_ui16 z.ch namelen;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch 0;
	IO.write_i32 z.ch 0;
	IO.write_i32 z.ch z.cdr_offset;
	IO.nwrite z.ch f.fname;
	z.cdr_size <- z.cdr_size + 46 + namelen;
	z.cdr_offset <- z.cdr_offset + 30 + namelen + f.fclen

let zip_write_cdr z =
	List.iter (zip_write_cdr_file z) (List.rev z.files);
	IO.write_i32 z.ch 0x06054B50;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch 0;
	IO.write_ui16 z.ch (List.length z.files);
	IO.write_ui16 z.ch (List.length z.files);
	IO.write_i32 z.ch z.cdr_size;
	IO.write_i32 z.ch z.cdr_offset;
	IO.write_ui16 z.ch 0

(* ------------------------------- *)

let tag ?(ext=false) d = {
	tid = 0;
	textended = ext;
	tdata = d;
}

let convert_header com (w,h,fps,bg) =
	if max w h >= 1639 then failwith "-swf-header : size too large";
	{
		h_version = com.flash_version;
		h_size = {
			rect_nbits = if (max w h) >= 820 then 16 else 15;
			left = 0;
			top = 0;
			right = w * 20;
			bottom = h * 20;
		};
		h_frame_count = 1;
		h_fps = to_float16 (if fps > 127.0 then 127.0 else fps);
		h_compressed = not (Common.defined com "no-swf-compress");
	} , bg

let default_header com =
	convert_header com (400,300,30.,0xFFFFFF)

type dependency_kind =
	| DKInherit
	| DKExpr
	| DKType

let build_dependencies t = 
	let h = ref PMap.empty in	
	let add_path p k =
		h := PMap.add (p,k) () !h;
	in
	let rec add_type_rec l t =
		if List.memq t l then () else		
		match t with
		| TEnum (e,pl) ->
			add_path e.e_path DKType;
			List.iter (add_type_rec (t::l)) pl;
		| TInst (c,pl) ->
			add_path c.cl_path DKType;
			List.iter (add_type_rec (t::l)) pl;
		| TFun (pl,t2) ->
			List.iter (fun (_,_,t2) -> add_type_rec (t::l) t2) pl;
			add_type_rec (t::l) t2;
		| TAnon a ->
			PMap.iter (fun _ f -> add_type_rec (t::l) f.cf_type) a.a_fields
		| TDynamic t2 ->
			add_type_rec (t::l) t2;
		| TLazy f ->
			add_type_rec l ((!f)())
		| TMono r ->
			(match !r with
			| None -> ()
			| Some t -> add_type_rec l t)
		| TType (tt,pl) ->
			add_type_rec (t::l) tt.t_type;
			List.iter (add_type_rec (t::l)) pl
	and add_type t = 
		add_type_rec [] t
	and add_expr e =
		match e.eexpr with
		| TTypeExpr t -> add_path (Type.t_path t) DKExpr
		| TEnumField (e,_) -> add_path e.e_path DKExpr
		| TNew (c,pl,el) ->
			add_path c.cl_path DKExpr;
			List.iter add_type pl;
			List.iter add_expr el;
		| TFunction f ->
			List.iter (fun (_,_,t) -> add_type t) f.tf_args;
			add_type f.tf_type;
			add_expr f.tf_expr;
		| TFor (_,t,e1,e2) ->
			add_type t;
			add_expr e1;
			add_expr e2;
		| TVars vl ->
			List.iter (fun (_,t,e) ->
				add_type t;
				match e with
				| None -> ()
				| Some e -> add_expr e
			) vl
		| _ ->
			Type.iter add_expr e
	and add_field f =
		add_type f.cf_type;
		match f.cf_expr with
		| None -> ()
		| Some e -> add_expr e
	in
	let add_inherit (c,pl) =
		add_path c.cl_path DKInherit;
		List.iter add_type pl;
	in
	(match t with
	| TClassDecl c when not c.cl_extern ->
		List.iter add_field c.cl_ordered_fields;
		List.iter add_field c.cl_ordered_statics;
		(match c.cl_constructor with
		| None -> ()
		| Some f -> 
			add_field f;
			if c.cl_path <> (["flash"],"Boot") then add_path (["flash"],"Boot") DKExpr;
		);
		(match c.cl_init with
		| None -> ()
		| Some e -> add_expr e);
		(match c.cl_super with
		| None -> add_path ([],"Object") DKInherit;
		| Some x -> add_inherit x);
		List.iter (fun (_,t) ->
			(* add type-parameters constraints dependencies *)
			match follow t with
			| TInst (c,_) -> List.iter add_inherit c.cl_implements
			| _ -> ()
		) c.cl_types;
		List.iter add_inherit c.cl_implements;
	| TEnumDecl e when not e.e_extern ->
		PMap.iter (fun _ f -> add_type f.ef_type) e.e_constrs;
	| _ -> ());
	h := PMap.remove (([],"Int"),DKType) (!h);
	h := PMap.remove (([],"Int"),DKExpr) (!h);
	PMap.foldi (fun (c,k) () acc -> (c,k) :: acc) (!h) []

let build_swc_catalog com types =
	let node x att l =
		Xml.Element (x,att,l)
	in
	let make_path t sep =
		let path, name = t_path t in
		String.concat sep (path @ [name])
	in
	let make_id path =
		match Genswf9.real_path path with
		| [],n -> n
		| l,n -> (String.concat "." l) ^ ":" ^ n
	in
	let build_script t =
		let deps = build_dependencies t in
		node "script" [("name",make_path t "/");("mod","0")] ([
			node "def" ["id",make_id (t_path t)] [];
			node "dep" [("id","AS3");("type","n")] [];
		] @ List.map (fun (p,k) ->
			let t = (match k with
				| DKInherit -> "i"
				| DKExpr -> (match p with "flash" :: _ :: _ , _ -> "i" | _ -> "e")
				| DKType -> "s"
			) in
			node "dep" [("id",make_id p);("type",t)] []
		) deps)
	in
	let x = node "swc" ["xmlns","http://www.adobe.com/flash/swccatalog/9"] [
		node "versions" [] [
			node "swc" ["version","1.2"] [];
			node "haxe" ["version",Printf.sprintf "%d.%.2d" (com.version/100) (com.version mod 100)] [];
		];
		node "features" [] [
			node "feature-script-deps" [] [];
			node "feature-files" [] [];
		];
		node "libraries" [] [
			node "library" ["path","library.swf"] (List.map build_script types)
		];
		node "files" [] [];
	] in
	"<?xml version=\"1.0\" encoding =\"utf-8\"?>\n" ^ Xml.to_string_fmt x

let make_as3_public data =
	(* set all protected+private fields to public - this will enable overriding/reflection in haXe classes *)
	let ipublic = ref (-1) in
	let ns = Array.mapi (fun i ns ->
		match ns with
		| A3NPrivate _
		| A3NInternal _
		| A3NProtected _ 
		| A3NPublic None
			->
			ipublic := i;
			A3NPublic None
		| A3NPublic _
		| A3NNamespace _
		| A3NExplicit _
		| A3NStaticProtected _ -> ns
	) data.as3_namespaces in
	let cl = Array.map (fun c -> { c with cl3_namespace = None }) data.as3_classes in
	{ data with as3_namespaces = ns; as3_classes = cl }

let build_swf8 com codeclip exports =
	let code, clips = Genswf8.generate com in
	let cid = ref 0 in
	let clips = List.fold_left (fun acc m ->
		let ename = Ast.s_type_path m in
		if Hashtbl.mem exports ename then
			acc
		else begin
			incr cid;
			tag ~ext:true (TClip { c_id = !cid; c_frame_count = 1; c_tags = [] }) ::
			tag ~ext:true (TExport [{ exp_id = !cid; exp_name = ename }]) ::
			acc
		end;
	) [] clips in
	let code = (match codeclip with
		| None -> List.map tag code
		| Some link ->
			incr cid;
			[
				tag (TClip {
					c_id = !cid;
					c_frame_count = 1;
					c_tags = List.map tag code @ [tag TShowFrame];
				});
				tag (TExport [{ exp_id = !cid; exp_name = link }]);
			]
	) in
	clips @ code

let build_swf9 com swc =
	let code, genmethod = Genswf9.generate com in
	let code = (match swc with 
	| Some cat ->
		cat := build_swc_catalog com (List.map (fun (t,_,_) -> t) code);
		List.map (fun (t,m,f) ->
			let path = (match t_path t with
				| [], name -> name
				| path, name -> String.concat "/" path ^ "/" ^ name
			) in
			let init = {
				hls_method = m;
				hls_fields = [|f|];
			} in
			tag (TActionScript3 (Some (1,path),As3hlparse.flatten [init]))
		) code
	| None ->
		let inits = List.map (fun (_,m,f) ->
			{
				hls_method = m;
				hls_fields = [|f|];
			}
		) code in
		[tag (TActionScript3 (None,As3hlparse.flatten inits))]
	) in
	let clips = [tag (TF9Classes [{ f9_cid = None; f9_classname = "flash.Boot" }])] in
	code @ clips

let merge com priority (h1,tags1) (h2,tags2) =
  (* prioritize header+bgcolor for first swf *)
	let header = if priority then { h2 with h_version = max h2.h_version com.flash_version } else h1 in
	let tags1 = if priority then List.filter (function { tdata = TSetBgColor _ } -> false | _ -> true) tags1 else tags1 in
  (* remove unused tags *)
	let use_stage = Common.defined com "flash_use_stage" in
	let classes = ref [] in
	let nframe = ref 0 in
	let tags2 = List.filter (fun t ->
		match t.tdata with
		| TPlaceObject2 _
		| TPlaceObject3 _
		| TRemoveObject2 _
		| TRemoveObject _ -> use_stage
		| TShowFrame -> incr nframe; use_stage
		| TFilesAttributes _ | TEnableDebugger2 _ | TF9Scene _ -> false
		| TSetBgColor _ -> priority
		| TF9Classes el ->
			if com.flash_version < 9 then failwith "You can't use AS3 SWF with Flash8 target";
			if !nframe <> 0 then failwith "Classes export found outside of Frame 1";
			classes := !classes @ List.filter (fun e -> e.f9_cid <> None) el; false
		| _ -> true
	) tags2 in
  (* rebuild character ids *)
	let max_id = ref (-1) in
	List.iter (SwfParser.scan (fun id -> if id > !max_id then max_id := id; id) (fun id -> id)) tags1;
	incr max_id;
	let rec loop t =
		SwfParser.scan (fun id -> id + !max_id) (fun id -> id + !max_id) t;
		match t.tdata with
		| TClip c -> List.iter loop c.c_tags
		| _ -> ()
	in
	List.iter loop tags2;
	let classes = List.map (fun e -> match e.f9_cid with None -> e | Some id -> { e with f9_cid = Some (id + !max_id) }) !classes in
  (* do additional transforms *)
	let tags2 = List.map (fun t ->
		match t.tdata with
		| TActionScript3 (id,data) -> { t with tdata = TActionScript3 (id,make_as3_public data) }
		| _ -> t
	) tags2 in
  (* merge timelines *)
	let rec loop l1 l2 =
		match l1, l2 with
		| ({ tdata = TSetBgColor _ } as t) :: l1, _ 
		| ({ tdata = TEnableDebugger2 _ } as t) :: l1, _ 
		| ({ tdata = TFilesAttributes _ } as t) :: l1, _ ->
			t :: loop l1 l2
		| _, ({ tdata = TSetBgColor _ } as t) :: l2 ->
			t :: loop l1 l2
		| { tdata = TShowFrame } :: l1, { tdata = TShowFrame } :: l2 ->
			tag TShowFrame :: loop l1 l2
		| { tdata = TShowFrame } :: _, x :: l2 -> 
			(* wait until we finish frame on other swf *)
			x :: loop l1 l2
		| { tdata = TF9Classes el } :: l1, _ ->
			(* merge all classes together *)
			tag (TF9Classes (classes @ el)) :: loop l1 l2
		| _ , x :: l2 ->
			x :: loop l1 l2
		| x :: l1, [] -> 
			x :: loop l1 l2
		| [], [] ->
			[]
	in
	let tags = loop tags1 tags2 in
	header, tags

let generate com swf_header swf_libs =
	let t = Common.timer "generate swf" in
	let isf9 = com.flash_version >= 9 in
	let swc = if Common.defined com "swc" then Some (ref "") else None in
	if swc <> None && not isf9 then failwith "SWC support is only available for Flash9+";
	let file , codeclip = (try let f , c = ExtString.String.split com.file "@" in f, Some c with _ -> com.file , None) in
  (* list exports *)
	let exports = Hashtbl.create 0 in
	List.iter (fun lib ->
		let _, tags = lib() in
		List.iter (fun t ->
			match t.tdata with
			| TExport l -> List.iter (fun e -> Hashtbl.add exports e.exp_name ()) l
			| _ -> ()
		) tags;
	) swf_libs;
  (* build haxe swf *)
	let tags = if isf9 then build_swf9 com swc else build_swf8 com codeclip exports in
	let header, bg = (match swf_header with None -> default_header com | Some h -> convert_header com h) in
	let bg = tag (TSetBgColor { cr = bg lsr 16; cg = (bg lsr 8) land 0xFF; cb = bg land 0xFF }) in
	let debug = (if isf9 && Common.defined com "fdb" then [tag (TEnableDebugger2 (0,""))] else []) in
	let fattr = (if com.flash_version < 8 then [] else
		[tag (TFilesAttributes {
			fa_network = Common.defined com "network-sandbox";
			fa_as3 = isf9;
			fa_metadata = false;
			fa_gpu = false;
			fa_direct_blt = false;
		})]
	) in
	let swf = header, fattr @ bg :: debug @ tags @ [tag TShowFrame] in
  (* merge swf libraries *)
	let priority = ref (swf_header = None) in
	let swf = List.fold_left (fun swf lib ->
		let swf = merge com !priority swf (lib()) in
		priority := false;
		swf
	) swf swf_libs in
	t();
  (* write swf/swc *)
	let t = Common.timer "write swf" in
	(match swc with
	| Some cat ->
		let ch = IO.output_strings() in
		Swf.write ch swf;
		let swf = IO.close_out ch in
		let ch = IO.output_channel (open_out_bin file) in
		let z = zip_create ch in
		zip_write_file z "catalog.xml" (!cat) (Unix.time()) true;
		zip_write_file z "library.swf" (match swf with [s] -> s | _ -> failwith "SWF too big for SWC") (Unix.time()) false;
		zip_write_cdr z;
		IO.close_out ch;
	| None ->
		let ch = IO.output_channel (open_out_bin file) in
		Swf.write ch swf;
		IO.close_out ch;
	);
	t()

;;
SwfParser.init Extc.input_zip Extc.output_zip;
Swf.warnings := false;
