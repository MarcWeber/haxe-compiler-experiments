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
open Type

let rec map f e =
	match e.eexpr with
	| TConst _
	| TLocal _
	| TEnumField _
	| TBreak
	| TContinue
	| TType _ ->
		e
	| TArray (e1,e2) ->
		{ e with eexpr = TArray (f e1,f e2) }
	| TBinop (op,e1,e2) ->
		{ e with eexpr = TBinop (op,f e1,f e2) }
	| TFor (v,e1,e2) ->
		{ e with eexpr = TFor (v,f e1,f e2) }
	| TWhile (e1,e2,flag) ->
		{ e with eexpr = TWhile (f e1,f e2,flag) }
	| TThrow e ->
		{ e with eexpr = TThrow (f e) }
	| TField (e,v) ->
		{ e with eexpr = TField (f e,v) }
	| TParenthesis e ->
		{ e with eexpr = TParenthesis (f e) }
	| TUnop (op,pre,e) ->
		{ e with eexpr = TUnop (op,pre,f e) }
	| TArrayDecl el ->
		{ e with eexpr = TArrayDecl (List.map f el) }
	| TNew (t,pl,el) ->
		{ e with eexpr = TNew (t,pl,List.map f el) }
	| TBlock el ->
		{ e with eexpr = TBlock (List.map f el) }
	| TObjectDecl el ->
		{ e with eexpr = TObjectDecl (List.map (fun (v,e) -> v, f e) el) }
	| TCall (e,el) ->
		{ e with eexpr = TCall (f e, List.map f el) }
	| TVars vl ->
		{ e with eexpr = TVars (List.map (fun (v,t,e) -> v , t , match e with None -> None | Some e -> Some (f e)) vl) }
	| TFunction fu ->
		{ e with eexpr = TFunction { fu with tf_expr = f fu.tf_expr } }
	| TIf (e,e1,e2) ->
		{ e with eexpr = TIf (f e,f e1,match e2 with None -> None | Some e -> Some (f e)) }
	| TSwitch (e,cases,def) ->
		{ e with eexpr = TSwitch (f e, List.map (fun (e1,e2) -> f e1, f e2) cases, match def with None -> None | Some e -> Some (f e)) }
	| TMatch (e,t,cases,def) ->
		{ e with eexpr = TMatch (f e, t, List.map (fun (c,l,e) -> c, l, f e) cases, match def with None -> None | Some e -> Some (f e)) }
	| TTry (e,catches) ->
		{ e with eexpr = TTry (f e, List.map (fun (v,t,e) -> v, t, f e) catches) }
	| TReturn eo ->
		{ e with eexpr = TReturn (match eo with None -> None | Some e -> Some (f e)) }

let local_find flag vname e =
	let rec loop2 e =
		match e.eexpr with
		| TFunction f ->
			if not flag && not (List.exists (fun (a,_,_) -> a = vname) f.tf_args) then loop2 f.tf_expr
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
			if not (List.exists (fun (a,_,_) -> a = vname) f.tf_args) then loop2 f.tf_expr
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

let block_vars e =
	let add_var map v = map := PMap.add v () (!map) in
	let wrap e used =
		match PMap.foldi (fun v _ acc -> v :: acc) used [] with
		| [] -> e
		| vars ->
			mk (TCall (
				(mk (TFunction {
					tf_args = List.map (fun v -> v , false, t_dynamic) vars;
					tf_type = t_dynamic;
					tf_expr = mk (TReturn (Some e)) t_dynamic e.epos;
				}) t_dynamic e.epos),
				List.map (fun v -> mk (TLocal v) t_dynamic e.epos) vars)
			) t_dynamic e.epos
	in
	let rec in_fun vars used_locals e =
		match e.eexpr with
		| TLocal v ->
			if PMap.mem v vars then add_var used_locals v
		| TFunction _ ->
			()
		| _ ->
			iter (in_fun vars used_locals) e

	and in_loop vars e =
		match e.eexpr with
		| TVars l ->
			{ e with eexpr = TVars (List.map (fun (v,t,e) ->
				let e = (match e with None -> None | Some e -> Some (in_loop vars e)) in
				add_var vars v;
				v, t, e
			) l) }
		| TTry (e,cases) ->
			let e = in_loop vars e in
			let cases = List.map (fun (v,t,e) ->
				let new_vars = PMap.add v () (!vars) in
				v , t, in_loop (ref new_vars) e
			) cases in
			{ e with eexpr = TTry (e,cases) }
		| TMatch (e,t,cases,def) ->
			let e = in_loop vars e in
			let cases = List.map (fun (c,params,e) ->
				let e = (match params with
					| None -> in_loop vars e
					| Some l ->
						let new_vars = List.fold_left (fun acc (v,t) ->
							match v with
							| None -> acc
							| Some name -> PMap.add name () acc
						) (!vars) l in
						in_loop (ref new_vars) e
				) in
				c , params , e
			) cases in
			let def = (match def with None -> None | Some e -> Some (in_loop vars e)) in
			{ e with eexpr = TMatch (e, t, cases, def) }
		| TBlock l ->
			let new_vars = (ref !vars) in
			map (in_loop new_vars) e
		| TFunction _ ->
			let new_vars = !vars in
			let used = ref PMap.empty in
			iter (in_fun new_vars used) e;
			let e = wrap e (!used) in
			let new_vars = ref (PMap.foldi (fun v _ acc -> PMap.remove v acc) (!used) new_vars) in
			map (in_loop new_vars) e
		| _ ->
			map (in_loop vars) e
	and out_loop e =
		match e.eexpr with
		| TFor _ | TWhile _ ->
			map (in_loop (ref PMap.empty)) e
		| _ ->
			map out_loop e
	in
	out_loop e
