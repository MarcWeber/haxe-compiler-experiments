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

type xml = 
	| Node of string * (string * string) list * xml list
	| PCData of string

let tag name = Node (name,[],[])
let xml name att = Node (name,att,[])
let node name att childs = Node (name,att,childs)
let pcdata s = PCData s

let pmap f m = 
	PMap.fold (fun x acc -> f x :: acc) m []

let gen_path (p,n) =
	("path",String.concat "." (p @ [n]))

let rec gen_type t =
	match t with
	| TMono m -> (match !m with None -> tag "unknown" | Some t -> gen_type t)
	| TEnum (e,params) -> node "e" [gen_path e.e_path] (List.map gen_type params)
	| TInst (c,params) -> node "c" [gen_path c.cl_path] (List.map gen_type params)
	| TFun (args,r) -> node "f" [] (List.map gen_type (args @ [r]))
	| TAnon fields -> node "a" [] (pmap (fun f -> node f.cf_name [] [gen_type f.cf_type]) fields)
	| TDynamic t2 -> node "d" [] (if t == t2 then [] else [gen_type t2])

let gen_constr e =
	node e.ef_name [] (match follow e.ef_type with TFun (args,_) -> List.map gen_type args | _ -> [])

let gen_field att f =
	let att = (match f.cf_expr with None -> att | Some e -> ("line",string_of_int (Lexer.get_error_line e.epos)) :: att) in
	node f.cf_name (if f.cf_public then ("public","1") :: att else att) [gen_type f.cf_type]

let gen_type t =
	match t with
	| TClassDecl c -> 
		let stats = pmap (gen_field ["static","1"]) c.cl_statics in
		let fields = pmap (gen_field []) c.cl_fields in
		let constr = (match c.cl_constructor with None -> [] | Some f -> [gen_field [] f]) in
		node "class" [gen_path c.cl_path;("file",c.cl_pos.pfile)] (stats @ fields @ constr)
	| TEnumDecl e ->
		node "enum" [gen_path e.e_path;("file",e.e_pos.pfile)] (pmap gen_constr e.e_constrs)

let att_str att = 
	String.concat "" (List.map (fun (a,v) -> Printf.sprintf " %s=\"%s\"" a v) att)

let rec write_xml ch tabs x =
	match x with
	| Node (name,att,[]) -> 
		IO.printf ch "%s<%s%s/>" tabs name (att_str att)
	| Node (name,att,[PCData s]) -> 
		IO.printf ch "%s<%s%s>%s</%s>" tabs name (att_str att) s name
	| Node (name,att,childs) ->
		IO.printf ch "%s<%s%s>\n" tabs name (att_str att);
		List.iter (fun x ->
			write_xml ch (tabs ^ "\t") x;
			IO.printf ch "\n";
		) childs;
		IO.printf ch "%s</%s>" tabs name
	| PCData s ->
		assert false

let generate file types =
	let x = node "haxe" [] (List.map gen_type types) in
	let ch = IO.output_channel (open_out file) in
	write_xml ch "" x;
	IO.close_out ch
