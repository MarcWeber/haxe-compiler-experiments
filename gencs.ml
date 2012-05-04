(*
 *  haXe/C# & Java Compiler
 *  Copyright (c)2011 Cauê Waneck
 *  based on and including code by (c)2005-2008 Nicolas Cannasse, Hugh Sanderson and Franco Ponticelli
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
open Common
open Gencommon
open Gencommon.SourceWriter
open Type
open Printf
open Option

let is_cs_basic_type t =
  match follow t with
    | TInst( { cl_path = (["haxe"], "Int32") }, [] )
    | TInst( { cl_path = (["haxe"], "Int64") }, [] )
    | TInst( { cl_path = ([], "Int") }, [] )
    | TInst( { cl_path = ([], "Float") }, [] )
    | TEnum( { e_path = ([], "Bool") }, [] ) -> 
      true
    | _ -> false
    
let is_int_float t =
  match follow t with
    | TInst( { cl_path = (["haxe"], "Int32") }, [] )
    | TInst( { cl_path = (["haxe"], "Int64") }, [] )
    | TInst( { cl_path = ([], "Int") }, [] )
    | TInst( { cl_path = ([], "Float") }, [] ) -> 
      true
    | _ -> false

let parse_explicit_iface =
  let regex = Str.regexp "\\." in
  let parse_explicit_iface str =
    let split = Str.split regex str in
    let rec get_iface split pack =
      match split with
        | clname :: fn_name :: [] -> fn_name, (List.rev pack, clname)
        | pack_piece :: tl -> get_iface tl (pack_piece :: pack)
        | _ -> assert false
    in
    get_iface split []
  in parse_explicit_iface
	
let is_string t =
  match follow t with
    | TInst( { cl_path = ([], "String") }, [] ) -> true
    | _ -> false
    
(* ******************************************* *)
(* CSharpSpecificSynf *)
(* ******************************************* *)

(*
  
  Some CSharp-specific syntax filters
  
  dependencies:
    It must run before ExprUnwrap, as it may not return valid Expr/Statement expressions
    It must run before ClassInstance, as it will detect expressions that need unchanged TTypeExpr
  
*)

module CSharpSpecificSynf =
struct

  let name = "csharp_specific"
  
  let priority = solve_deps name [DBefore ExpressionUnwrap.priority; DBefore ClassInstance.priority]
  
  let get_cl_from_t t =
    match follow t with
      | TInst(cl,_) -> cl
      | _ -> assert false
  
  let traverse gen runtime_cl =
    let basic = gen.gcon.basic in
    let uint = match ( get_type gen ([], "UInt") ) with | TTypeDecl t -> t | _ -> assert false in
    let tchar = match ( get_type gen (["cs"], "Char16") ) with | TTypeDecl t -> t | _ -> assert false in
    let tchar = TType(tchar,[]) in
    let string_ext = get_cl ( get_type gen (["haxe";"lang"], "StringExt")) in
    
    let is_var = alloc_var "__is__" t_dynamic in
    let block = ref [] in
    let is_string t = match follow t with | TInst({ cl_path = ([], "String") }, []) -> true | _ -> false in
    
    let rec run e =
      match e.eexpr with 
        | TBlock bl ->
          let old_block = !block in
          block := [];
          List.iter (fun e -> let e = run e in block := e :: !block) bl;
          let ret = List.rev !block in
          block := old_block;
          
          { e with eexpr = TBlock(ret) }
        (* Std.is() *)
        | TCall(
            { eexpr = TField( { eexpr = TTypeExpr ( TClassDecl ({ cl_path = ([], "Std") }) ) }, "is") },
            [obj; { eexpr = TTypeExpr(md) }]
          ) ->
          let mk_is obj md =
            { e with eexpr = TCall( { eexpr = TLocal is_var; etype = t_dynamic; epos = e.epos }, [ 
              obj;
              { eexpr = TTypeExpr md; etype = t_dynamic (* this is after all a syntax filter *); epos = e.epos }
            ] ) }
          in
          let obj = run obj in
          (match follow_module follow md with
            | TClassDecl({ cl_path = ([], "Float") }) ->
              (* on the special case of seeing if it is a Float, we need to test if both it is a float and if it is an Int *)
              let mk_is local =
                mk_paren {
                  eexpr = TBinop(Ast.OpBoolOr, mk_is local md, mk_is local (TClassDecl (get_cl_from_t basic.tint)));
                  etype = basic.tbool;
                  epos = e.epos
                }
              in
              
              let ret = match obj.eexpr with
                | TLocal(v) -> mk_is obj
                | _ ->
                  let var = mk_temp gen "is" obj.etype in
                  let added = { obj with eexpr = TVars([var, Some(obj)]); etype = basic.tvoid } in
                  let local = mk_local var obj.epos in
                  {
                    eexpr = TBlock([ added; mk_is local ]);
                    etype = basic.tbool;
                    epos = e.epos
                  }
              in
              ret
            | _ ->
              mk_is obj md
          )
        (* end Std.is() *)
        
        (* Std.int() *)
        | TCall(
            { eexpr = TField( { eexpr = TTypeExpr ( TClassDecl ({ cl_path = ([], "Std") }) ) }, "int") },
            [obj]
          ) ->
          run (mk_cast basic.tint obj)
        (* end Std.int() *)
        
        | TField(ef, "length") when is_string ef.etype ->
          { e with eexpr = TField(run ef, "Length") }
        | TField(ef, ("toLowerCase")) when is_string ef.etype ->
          { e with eexpr = TField(run ef, "ToLower") }
        | TField(ef, ("toUpperCase")) when is_string ef.etype ->
          { e with eexpr = TField(run ef, "ToUpper") }
        
        | TCall( ( { eexpr = TField({ eexpr = TTypeExpr (TClassDecl cl) }, "fromCharCode") } ), [cc] ) ->
          { e with eexpr = TNew(get_cl_from_t basic.tstring, [], [mk_cast tchar (run cc); mk_int gen 1 cc.epos]) }
        | TCall( ( { eexpr = TField({ eexpr = TTypeExpr (TTypeDecl t) }, "fromCharCode") } ), [cc] ) when is_string (follow (TType(t,List.map snd t.t_types))) ->
          { e with eexpr = TNew(get_cl_from_t basic.tstring, [], [mk_cast tchar (run cc); mk_int gen 1 cc.epos]) }
        | TCall( ( { eexpr = TField(ef, ("charAt" as field)) } ), args )
        | TCall( ( { eexpr = TField(ef, ("charCodeAt" as field)) } ), args )
        | TCall( ( { eexpr = TField(ef, ("indexOf" as field)) } ), args )
        | TCall( ( { eexpr = TField(ef, ("lastIndexOf" as field)) } ), args )
        | TCall( ( { eexpr = TField(ef, ("split" as field)) } ), args )
        | TCall( ( { eexpr = TField(ef, ("substr" as field)) } ), args ) when is_string ef.etype ->
          { e with eexpr = TCall(mk_static_field_access_infer string_ext field e.epos [], [run ef] @ (List.map run args)) }
        
        | TCast(expr, _) when is_int_float e.etype && not (is_int_float expr.etype) ->
          let needs_cast = match gen.gfollow#run_f e.etype with
            | TInst _ -> false
            | _ -> true
          in
          
          let fun_name = match follow e.etype with
            | TInst ({ cl_path = ([], "Float") },[]) -> "toDouble"
            | _ -> "toInt"
          in
          
          let ret = {
            eexpr = TCall(
              mk_static_field_access_infer runtime_cl fun_name expr.epos [],
              [ run expr ]
            );
            etype = basic.tint;
            epos = expr.epos
          } in
          
          if needs_cast then mk_cast e.etype ret else ret
        | TCast(expr, _) when is_string e.etype ->
          (*{ e with eexpr = TCall( { expr with eexpr = TField(expr, "ToString"); etype = TFun([], basic.tstring) }, [] ) }*)
          mk_paren { e with eexpr = TBinop(Ast.OpAdd, run expr, { e with eexpr = TConst(TString("")) }) }
          
        | TBinop( Ast.OpUShr, e1, e2 ) ->
          mk_cast e.etype { e with eexpr = TBinop( Ast.OpShr, mk_cast (TType(uint,[])) (run e1), run e2 ) }
        
        | TBinop( Ast.OpAssignOp Ast.OpUShr, e1, e2 ) ->
          let mk_ushr local = 
            { e with eexpr = TBinop(Ast.OpAssign, local, run { e with eexpr = TBinop(Ast.OpUShr, local, run e2) }) }
          in
          
          let mk_local obj =
            let var = mk_temp gen "opUshr" obj.etype in
            let added = { obj with eexpr = TVars([var, Some(obj)]); etype = basic.tvoid } in
            let local = mk_local var obj.epos in
            local, added
          in
          
          let e1 = run e1 in
          
          let ret = match e1.eexpr with
            | TField({ eexpr = TLocal _ }, _)
            | TField({ eexpr = TTypeExpr _ }, _)
            | TArray({ eexpr = TLocal _ }, _)
            | TLocal(_) -> 
              mk_ushr e1
            | TField(fexpr, field) ->
              let local, added = mk_local fexpr in
              { e with eexpr = TBlock([ added; mk_ushr { e1 with eexpr = TField(local, field) }  ]); }
            | TArray(ea1, ea2) ->
              let local, added = mk_local ea1 in
              { e with eexpr = TBlock([ added; mk_ushr { e1 with eexpr = TArray(local, ea2) }  ]); }
            | _ -> (* invalid left-side expression *)
              assert false
          in
          
          ret
        
        | _ -> Type.map_expr run e
    in
    run
  
  let configure gen (mapping_func:texpr->texpr) =
    let map e = Some(mapping_func e) in
    gen.gsyntax_filters#add ~name:name ~priority:(PCustom priority) map
  
end;;
 
let connecting_string = "?" (* ? see list here http://www.fileformat.info/info/unicode/category/index.htm and here for C# http://msdn.microsoft.com/en-us/library/aa664670.aspx *)
let default_package = "cs" (* I'm having this separated as I'm still not happy with having a cs package. Maybe dotnet would be better? *)
let strict_mode = ref false (* strict mode is so we can check for unexpected information *)

(* reserved c# words *)
let reserved = let res = Hashtbl.create 120 in
  List.iter (fun lst -> Hashtbl.add res lst ("@" ^ lst)) ["abstract"; "as"; "base"; "bool"; "break"; "byte"; "case"; "catch"; "char"; "checked"; "class";
    "const"; "continue"; "decimal"; "default"; "delegate"; "do"; "double"; "else"; "enum"; "event"; "explicit";
    "extern"; "false"; "finally"; "fixed"; "float"; "for"; "foreach"; "goto"; "if"; "implicit"; "in"; "int";
    "interface"; "internal"; "is"; "lock"; "long"; "namespace"; "new"; "null"; "object"; "operator"; "out"; "override";
    "params"; "private"; "protected"; "public"; "readonly"; "ref"; "return"; "sbyte"; "sealed"; "short"; "sizeof";
    "stackalloc"; "static"; "string"; "struct"; "switch"; "this"; "throw"; "true"; "try"; "typeof"; "uint"; "ulong";
    "unchecked"; "unsafe"; "ushort"; "using"; "virtual"; "volatile"; "void"; "while"; "add"; "ascending"; "by"; "descending";
    "dynamic"; "equals"; "from"; "get"; "global"; "group"; "into"; "join"; "let"; "on"; "orderby"; "partial";
    "remove"; "select"; "set"; "value"; "var"; "where"; "yield"];
  res
  
let dynamic_anon = TAnon( { a_fields = PMap.empty; a_status = ref Closed } )
  
(* 
  On hxcs, the only type parameters allowed to be declared are the basic c# types.
  That's made like this to avoid casting problems when type parameters in this case
  add nothing to performance, since the memory layout is always the same.
  
  To avoid confusion between Generic<Dynamic> (which has a different meaning in hxcs AST), 
  all those references are using dynamic_anon, which means Generic<{}>
*)
let change_param_type md tl =
  let is_hxgeneric = (TypeParams.RealTypeParams.is_hxgeneric md) in
  let ret t = match is_hxgeneric, follow t with
    | false, _ -> t
    | true, TInst ( { cl_kind = KTypeParameter }, _ ) -> t
    | true, TInst _ | true, TEnum _ when is_cs_basic_type t -> t
    | true, TDynamic _ -> t
    | true, _ -> dynamic_anon
  in
  if is_hxgeneric && List.exists (fun t -> match follow t with | TDynamic _ -> true | _ -> false) tl then 
    List.map (fun _ -> t_dynamic) tl 
  else 
    List.map ret tl 
  
let rec get_class_modifiers meta cl_type cl_access cl_modifiers =
  match meta with
    | [] -> cl_type,cl_access,cl_modifiers
    | (":struct",[],_) :: meta -> get_class_modifiers meta "struct" cl_access cl_modifiers
    | (":protected",[],_) :: meta -> get_class_modifiers meta cl_type "protected" cl_modifiers
    | (":internal",[],_) :: meta -> get_class_modifiers meta cl_type "internal" cl_modifiers
    (* no abstract for now | (":abstract",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("abstract" :: cl_modifiers) 
    | (":static",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("static" :: cl_modifiers) TODO: support those types *)
    | (":final",[],_) :: meta -> get_class_modifiers meta cl_type cl_access ("sealed" :: cl_modifiers)
    | _ :: meta -> get_class_modifiers meta cl_type cl_access cl_modifiers

let rec get_fun_modifiers meta access modifiers =
  match meta with
    | [] -> access,modifiers
    | (":protected",[],_) :: meta -> get_fun_modifiers meta "protected" modifiers
    | (":internal",[],_) :: meta -> get_fun_modifiers meta "internal" modifiers
    | (":readonly",[],_) :: meta -> get_fun_modifiers meta access ("readonly" :: modifiers)
    | (":unsafe",[],_) :: meta -> get_fun_modifiers meta access ("unsafe" :: modifiers)
    | (":volatile",[],_) :: meta -> get_fun_modifiers meta access ("volatile" :: modifiers)
    | _ :: meta -> get_fun_modifiers meta access modifiers
    
(* this was the way I found to pass the generator context to be accessible across all functions here *)
(* so 'configure' is almost 'top-level' and will have all functions needed to make this work *)
let configure gen =
  let basic = gen.gcon.basic in

  let fn_cl = get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"],"Function")) in
  
  let null_t = (get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"],"Null")) ) in
  
  let runtime_cl = get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"],"Runtime")) in
  
  let rec change_ns ns = match ns with
    | "cs" :: "native" :: tl -> "System" :: (change_ns tl)
    | _ -> List.map (fun s ->
      let ch = String.get s 0 in
			let ch = if Char.uppercase ch <> ch then
					Char.uppercase ch
				else
					Char.lowercase ch
			in
      (Char.escaped ch) ^ (String.sub s 1 ((String.length s) - 1))
    ) ns 
  in
  
  let change_clname n = n in
  
  let change_id name = try Hashtbl.find reserved name with | Not_found -> name in
  
  let change_field = change_id in
  
  let write_id w name = write w (change_id name) in
  
  let write_field w name = write w (change_field name) in
  
  gen.gfollow#add ~name:"follow_basic" (fun t -> match t with 
      | TEnum ({ e_path = ([], "Bool") }, [])
      | TEnum ({ e_path = ([], "Void") }, [])
      | TInst ({ cl_path = ([],"Float") },[])
      | TInst ({ cl_path = ([],"Int") },[]) 
      | TType ({ t_path = [],"UInt" },[])
      | TType ({ t_path = [],"Int64" },[])
      | TType ({ t_path = ["cs"],"UInt64" },[])
      | TType ({ t_path = ["cs"],"UInt8" },[])
      | TType ({ t_path = ["cs"],"Int8" },[])
      | TType ({ t_path = ["cs"],"Int16" },[])
      | TType ({ t_path = ["cs"],"UInt16" },[]) 
      | TType ({ t_path = ["cs"],"Char16" },[])
      | TType ({ t_path = [],"Single" },[]) -> Some t
			| TInst( { cl_path = ([], "EnumValue") }, _  ) -> Some t_dynamic
      | _ -> None);
  
  let path_s path = match path with
    | ([], "String") -> "string"
    | ([], "Null") -> path_s (change_ns ["haxe"; "lang"], change_clname "Null")
    | (ns,clname) -> path_s (change_ns ns, change_clname clname)
  in
  
  let ifaces = ref (Hashtbl.create 0) in
  
  let ti64 = match ( get_type gen ([], "Int64") ) with | TTypeDecl t -> TType(t,[]) | _ -> assert false in
  
  let real_type t =
    let t = gen.gfollow#run_f t in
    match t with
      | TInst( { cl_path = (["haxe"], "Int32") }, [] ) -> gen.gcon.basic.tint
      | TInst( { cl_path = (["haxe"], "Int64") }, [] ) -> ti64
      | TEnum(_, [])
      | TInst(_, []) -> t
      | TInst(cl, params) when 
        List.exists (fun t -> match follow t with | TDynamic _ -> true | _ -> false) params &&
        Hashtbl.mem !ifaces cl.cl_path -> 
          TInst(Hashtbl.find !ifaces cl.cl_path, [])
      | TEnum(e, params) when
        List.exists (fun t -> match follow t with | TDynamic _ -> true | _ -> false) params &&
        Hashtbl.mem !ifaces e.e_path -> 
          TInst(Hashtbl.find !ifaces e.e_path, [])
      | TInst(cl, params) -> TInst(cl, change_param_type (TClassDecl cl) params)
      | TEnum(e, params) -> TEnum(e, change_param_type (TEnumDecl e) params)
      (* | TType({ t_path = ([], "Null") }, [t]) -> TInst(null_t, [t]) *)
      | TType _ -> t
      | TAnon (anon) when (match !(anon.a_status) with | Statics _ | EnumStatics _ -> true | _ -> false) -> t
      | TAnon _ -> dynamic_anon
      | TFun _ -> TInst(fn_cl,[])
      | _ -> t_dynamic
  in
  
  let is_dynamic t = match real_type t with
    | TMono _ | TDynamic _ -> true
    | TAnon anon ->
      (match !(anon.a_status) with
        | EnumStatics _ | Statics _ -> false
        | _ -> true
      )
    | _ -> false
  in
  
  let rec t_s t =
    match real_type t with
      (* basic types *)
      | TEnum ({ e_path = ([], "Bool") }, []) -> "bool"
      | TEnum ({ e_path = ([], "Void") }, []) -> "object"
      | TInst ({ cl_path = ([],"Float") },[]) -> "double"
      | TInst ({ cl_path = ([],"Int") },[]) -> "int"
      | TType ({ t_path = [],"UInt" },[]) -> "uint"
      | TType ({ t_path = [],"Int64" },[]) -> "long"
      | TType ({ t_path = ["cs"],"UInt64" },[]) -> "ulong"
      | TType ({ t_path = ["cs"],"UInt8" },[]) -> "byte"
      | TType ({ t_path = ["cs"],"Int8" },[]) -> "sbyte"
      | TType ({ t_path = ["cs"],"Int16" },[]) -> "short"
      | TType ({ t_path = ["cs"],"UInt16" },[]) -> "ushort"
      | TType ({ t_path = ["cs"],"Char16" },[]) -> "char"
      | TType ({ t_path = [],"Single" },[]) -> "float"
      | TInst ({ cl_path = ["haxe"],"Int32" },[]) -> "int"
      | TInst ({ cl_path = ["haxe"],"Int64" },[]) -> "long"
      | TInst ({ cl_path = ([], "Dynamic") }, _) -> "object"
      | TInst({ cl_path = (["cs"], "NativeArray") }, [param]) ->
        let rec check_t_s t =
          match real_type t with
            | TInst({ cl_path = (["cs"], "NativeArray") }, [param]) ->
              (check_t_s param) ^ "[]"
            | _ -> t_s (run_follow gen t)
        in
        (check_t_s param) ^ "[]"
      (* end of basic types *)
      | TInst ({ cl_kind = KTypeParameter; cl_path=p }, []) -> snd p
      | TMono r -> (match !r with | None -> "object" | Some t -> t_s (run_follow gen t))
      | TInst ({ cl_path = [], "String" }, []) -> "string"
      | TInst ({ cl_path = [], "Class" }, _) | TInst ({ cl_path = [], "Enum" }, _) -> "Haxe.Lang.Class"
      | TEnum (({e_path = p;} as e), params) -> (path_param_s (TEnumDecl e) p params)
      | TInst (({cl_path = p;} as cl), params) -> (path_param_s (TClassDecl cl) p params)
      | TType (({t_path = p;} as t), params) -> (path_param_s (TTypeDecl t) p params)
      | TAnon (anon) ->
        (match !(anon.a_status) with
          | Statics _ | EnumStatics _ -> "Haxe.Lang.Class"
          | _ -> "object")
      | TDynamic _ -> "object"
      (* No Lazy type nor Function type made. That's because function types will be at this point be converted into other types *)
      | _ -> if !strict_mode then begin trace ("[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"); assert false end else "[ !TypeError " ^ (Type.s_type (Type.print_context()) t) ^ " ]"
      
  and path_param_s md path params =
      match params with
        | [] -> path_s path
        | _ -> sprintf "%s<%s>" (path_s path) (String.concat ", " (List.map (fun t -> t_s t) (change_param_type md params)))
  in
   
  let rett_s t =
    match t with
      | TEnum ({e_path = ([], "Void")}, []) -> "void"
      | _ -> t_s t
  in

  let escape s =
    let b = Buffer.create 0 in
    for i = 0 to String.length s - 1 do
      match String.unsafe_get s i with
      | '\\' -> Buffer.add_string b "\\\\"
      | '\'' -> Buffer.add_string b "\\\'"
      | '\"' -> Buffer.add_string b "\\\""
      | c when (Char.code c) < 32 -> Buffer.add_string b (Printf.sprintf "\\x%.2X" (Char.code c))
      | c -> Buffer.add_char b c
    done;
    Buffer.contents b
  in
  
  let has_semicolon e =
    match e.eexpr with
      | TBlock _ | TFor _ | TSwitch _ | TMatch _ | TTry _ | TIf _ -> false
      | TWhile (_,_,flag) when flag = Ast.NormalWhile -> false
      | _ -> true
  in
  
  let in_value = ref false in
  
  let rec md_s md = 
    let md = follow_module (gen.gfollow#run_f) md in
    match md with
      | TClassDecl ({ cl_types = [] } as cl) ->
        t_s (TInst(cl,[]))
      | TClassDecl (cl) when not (is_hxgen md) ->
        t_s (TInst(cl,List.map (fun t -> t_dynamic) cl.cl_types))
      | TEnumDecl ({ e_types = [] } as e) ->
        t_s (TEnum(e,[]))
      | TEnumDecl (e) when not (is_hxgen md) ->
        t_s (TEnum(e,List.map (fun t -> t_dynamic) e.e_types))
      | TClassDecl cl ->
        t_s (TInst(cl,[]))
      | TEnumDecl e ->
        t_s (TEnum(e,[]))
      | TTypeDecl t ->
        t_s (TType(t, List.map (fun t -> t_dynamic) t.t_types))
  in
  
  let expr_s w e =
    in_value := false;
    let rec expr_s w e =
      let was_in_value = !in_value in
      in_value := true;
      match e.eexpr with
        | TConst c ->
          (match c with
            | TInt i32 -> 
              print w "%ld" i32;
              (match real_type e.etype with
                | TType( { t_path = ([], "Int64") }, [] ) -> write w "L";
                | _ -> ()
              )
            | TFloat s -> 
              write w s;
              (match real_type e.etype with
                | TType( { t_path = ([], "Single") }, [] ) -> write w "f"
                | _ -> ()
              )
            | TString s -> print w "\"%s\"" (escape s)
            | TBool b -> write w (if b then "true" else "false")
            | TNull -> print w "default(%s)" (t_s e.etype)
            | TThis -> write w "this"
            | TSuper -> write w "base")
        | TLocal { v_name = "__undefined__" } ->
          write w (t_s (TInst(runtime_cl, List.map (fun _ -> t_dynamic) runtime_cl.cl_types)));
          write w ".undefined";
        | TLocal { v_name = "__typeof__" } -> write w "typeof"
        | TLocal var ->
          write_id w var.v_name
        | TEnumField (e, s) ->
          print w "%s." (path_s e.e_path); write_field w s
        | TArray (e1, e2) ->
          expr_s w e1; write w "["; expr_s w e2; write w "]"
        | TBinop (op, e1, e2) ->
          expr_s w e1; write w ( " " ^ (Ast.s_binop op) ^ " " ); expr_s w e2
        | TField (e, s) | TClosure (e, s) ->
          expr_s w e; write w "."; write_field w s
        | TTypeExpr mt ->
          (match mt with
            | TClassDecl { cl_path = (["haxe"], "Int64") } -> write w (path_s (["haxe"], "Int64"))
            | TClassDecl { cl_path = (["haxe"], "Int32") } -> write w (path_s (["haxe"], "Int32"))
            | TClassDecl cl -> write w (t_s (TInst(cl, List.map (fun _ -> t_empty) cl.cl_types)))
            | TEnumDecl en -> write w (t_s (TEnum(en, List.map (fun _ -> t_empty) en.e_types)))
            | TTypeDecl td -> write w (t_s (gen.gfollow#run_f (TType(td, List.map (fun _ -> t_empty) td.t_types)))) )
        | TParenthesis e ->
          write w "("; expr_s w e; write w ")"
        | TArrayDecl el ->
          print w "new %s" (t_s e.etype);
          write w "{";
          ignore (List.fold_left (fun acc e ->
            (if acc <> 0 then write w ", ");
            expr_s w e;
            acc + 1
          ) 0 el);
          write w "}"
        | TCall ({ eexpr = TLocal( { v_name = "__is__" } ) }, [ expr; { eexpr = TTypeExpr(md) } ] ) ->
          write w "( ";
          expr_s w expr;
          write w " is ";
          write w (md_s md);
          write w " )"
        | TCall ({ eexpr = TLocal( { v_name = "__as__" } ) }, [ expr; { eexpr = TTypeExpr(md) } ] ) ->
          write w "( ";
          expr_s w expr;
          write w " as ";
          write w (md_s md);
          write w " )"
        | TCall ({ eexpr = TLocal( { v_name = "__cs__" } ) }, [ { eexpr = TConst(TString(s)) } ] ) ->
          write w s
        | TCall ({ eexpr = TLocal( { v_name = "__goto__" } ) }, [ { eexpr = TConst(TInt v) } ] ) ->
          print w "goto label%ld" v
        | TCall ({ eexpr = TLocal( { v_name = "__label__" } ) }, [ { eexpr = TConst(TInt v) } ] ) ->
          print w "label%ld: {}" v
        | TCall ({ eexpr = TLocal( { v_name = "__rethrow__" } ) }, _) ->
          write w "throw"
        | TCall (e, el) ->
          let rec extract_tparams params el =
            match el with
              | ({ eexpr = TLocal({ v_name = "$type_param" }) } as tp) :: tl ->
                extract_tparams (tp.etype :: params) tl
              | _ -> (params, el)
          in
          let params, el = extract_tparams [] el in
          
          expr_s w e;
          
          (match params with
            | [] -> ()
            | params ->
              let md = match e.eexpr with
                | TField(ef, _) -> t_to_md (run_follow gen ef.etype)
                | _ -> assert false
              in
              write w "<";
              ignore (List.fold_left (fun acc t ->
                (if acc <> 0 then write w ", ");
                write w (t_s t);
                acc + 1
              ) 0 (change_param_type md params));
              write w ">"
          );
          
          write w "(";
          ignore (List.fold_left (fun acc e ->
            (if acc <> 0 then write w ", ");
            expr_s w e;
            acc + 1
          ) 0 el);
          write w ")"
        | TNew (({ cl_path = (["cs"], "NativeArray") } as cl), params, [ size ]) ->
          let rec check_t_s t times =
            match real_type t with
              | TInst({ cl_path = (["cs"], "NativeArray") }, [param]) ->
                (check_t_s param (times+1))
              | _ -> 
                print w "new %s[" (t_s (run_follow gen t));
                expr_s w size;
                print w "]";
                let rec loop i =
                  if i <= 0 then () else (write w "[]"; loop (i-1))
                in
                loop (times - 1)
          in
          check_t_s (TInst(cl, params)) 0 
        | TNew (cl, params, el) -> 
          write w "new ";
          write w (path_param_s (TClassDecl cl) cl.cl_path params);
          write w "(";
          ignore (List.fold_left (fun acc e ->
            (if acc <> 0 then write w ", ");
            expr_s w e;
            acc + 1
          ) 0 el);
          write w ")"
        | TUnop ((Ast.Increment as op), flag, e)
        | TUnop ((Ast.Decrement as op), flag, e) ->
          (match flag with
            | Ast.Prefix -> write w ( " " ^ (Ast.s_unop op) ^ " " ); expr_s w e
            | Ast.Postfix -> expr_s w e; write w (Ast.s_unop op))
        | TUnop (op, flag, e) ->
          (match flag with
            | Ast.Prefix -> write w ( " " ^ (Ast.s_unop op) ^ " (" ); expr_s w e; write w ") "
            | Ast.Postfix -> write w "("; expr_s w e; write w (") " ^ Ast.s_unop op))
        | TVars (v_eop_l) ->
          ignore (List.fold_left (fun acc (var, eopt) ->
            (if acc <> 0 then write w ", ");
            print w "%s " (t_s var.v_type);
            write_id w var.v_name;
            (match eopt with
              | None -> ()
              | Some e ->
                write w " = ";
                expr_s w e
            );
            acc + 1
          ) 0 v_eop_l);
        | TBlock [e] when was_in_value ->
          expr_s w e
        | TBlock el ->
          begin_block w;
          let last_line = ref (-1) in
          let line_directive p =
            let cur_line = Lexer.get_error_line p in
            let is_relative_path = (String.sub p.pfile 0 1) = "." in
            let file = if is_relative_path then "../" ^ p.pfile else p.pfile in
            if cur_line <> ((!last_line)+1) then begin print w "//#line %d \"%s\"" cur_line (Ast.s_escape file); newline w end;
            last_line := cur_line in
          List.iter (fun e -> 
            line_directive e.epos;
            in_value := false;
            expr_s w e;
            (if has_semicolon e then write w ";");
            newline w
          ) el;
          end_block w
        | TIf (econd, e1, Some(eelse)) when was_in_value ->
          write w "( ";
          expr_s w (mk_paren econd);
          write w " ? ";
          expr_s w (mk_paren e1);
          write w " : ";
          expr_s w (mk_paren eelse);
          write w " )";
        | TIf (econd, e1, eelse) ->
          write w "if ";
          expr_s w (mk_paren econd);
          write w " ";
          in_value := false;
          expr_s w (mk_block e1);
          (match eelse with
            | None -> ()
            | Some e ->
              write w " else ";
              in_value := false;
              expr_s w (mk_block e)
          )
        | TWhile (econd, eblock, flag) ->
          (match flag with
            | Ast.NormalWhile ->
              write w "while ";
              expr_s w (mk_paren econd);
              write w "";
              in_value := false;
              expr_s w (mk_block eblock)
            | Ast.DoWhile ->
              write w "do ";
              in_value := false;
              expr_s w (mk_block eblock);
              write w "while ";
              in_value := true;
              expr_s w (mk_paren econd);
          )
        | TSwitch (econd, ele_l, default) ->
          write w "switch ";
          expr_s w (mk_paren econd);
          begin_block w;
          List.iter (fun (el, e) ->
            List.iter (fun e ->
              write w "case ";
              in_value := true;
              expr_s w e;
              write w ":";
            ) el;
            newline w;
            in_value := false;
            expr_s w (mk_block e);
            newline w;
            write w "break;";
            newline w
          ) ele_l;
          if is_some default then begin
            write w "default:";
            newline w;
            in_value := false;
            expr_s w (get default);
            newline w;
            write w "break;"
          end;
          end_block w
        | TTry (tryexpr, ve_l) ->
          write w "try ";
          in_value := false;
          expr_s w (mk_block tryexpr);
          List.iter (fun (var, e) ->
            print w "catch (%s %s)" (t_s var.v_type) (var.v_name);
            in_value := false;
            expr_s w (mk_block e);
            newline w
          ) ve_l
        | TReturn eopt ->
          write w "return ";
          if is_some eopt then expr_s w (get eopt)
        | TBreak -> write w "break"
        | TContinue -> write w "continue"
        | TThrow e ->
          write w "throw ";
          expr_s w e
        | TCast (e1,md_t) ->
          ((*match gen.gfollow#run_f e.etype with
            | TType({ t_path = ([], "UInt") }, []) ->
              write w "( unchecked ((uint) ";
              expr_s w e1;
              write w ") )"
            | _ ->*)
              (* FIXME I'm ignoring module type *)
              print w "((%s) (" (t_s e.etype);
              expr_s w e1;
              write w ") )"
          )
        | TFor (_,_,content) -> 
          write w "[ for not supported "; 
          expr_s w content;
          write w " ]";
          if !strict_mode then assert false
        | TObjectDecl _ -> write w "[ obj decl not supported ]"; if !strict_mode then assert false
        | TFunction _ -> write w "[ func decl not supported ]"; if !strict_mode then assert false
        | TMatch _ -> write w "[ match not supported ]"; if !strict_mode then assert false
    in
    expr_s w e
  in
   
  let get_string_params cl_types =
    match cl_types with
      | [] ->
        ("","")
      | _ ->
        let params = sprintf "<%s>" (String.concat ", " (List.map (fun (_, tcl) -> match follow tcl with | TInst(cl, _) -> snd cl.cl_path | _ -> assert false) cl_types)) in
        let params_extends = List.fold_left (fun acc (name, t) ->
          match run_follow gen t with
            | TInst (cl, p) ->
              (match cl.cl_implements with
                | [] -> acc
                | _ -> acc) (* TODO
                | _ -> (sprintf " where %s : %s" name (String.concat ", " (List.map (fun (cl,p) -> path_param_s (TClassDecl cl) cl.cl_path p) cl.cl_implements))) :: acc ) *)
            | _ -> trace (t_s t); assert false (* FIXME it seems that a cl_types will never be anything other than cl.cl_types. I'll take the risk and fail if not, just to see if that confirms *)
        ) [] cl_types in
        (params, String.concat " " params_extends)
  in
   
  let gen_class_field w is_static cl is_final cf =
    let is_interface = cl.cl_interface in
    let name, is_new, is_explicit_iface = match cf.cf_name with
      | "new" -> snd cl.cl_path, true, false
      | name when String.contains name '.' -> 
        let fn_name, path = parse_explicit_iface name in
        (path_s path) ^ "." ^ fn_name, false, true
      | name -> name, false, false
    in
    (match cf.cf_kind with
      | Var _
      | Method (MethDynamic) -> 
        if not is_interface then begin 
          let access, modifiers = get_fun_modifiers cf.cf_meta "public" [] in
          print w "%s %s%s %s %s;" access (if is_static then "static " else "") (String.concat " " modifiers) (t_s (run_follow gen cf.cf_type)) (change_field name)
        end (* TODO see how (get,set) variable handle when they are interfaces *)
      | Method mkind -> 
        let is_virtual = not is_final && match mkind with | MethInline -> false | _ when not is_new -> true | _ -> false in
        let is_override = List.mem cf.cf_name cl.cl_overrides in
        let is_virtual = is_virtual && not (has_meta ":final" cl.cl_meta) && not (is_interface) in
        let visibility = if is_interface then "" else "public" in
        
        let visibility, modifiers = get_fun_modifiers cf.cf_meta visibility [] in
        let visibility, is_virtual = if is_explicit_iface then "",false else visibility, is_virtual in
        let v_n = if is_static then "static " else if is_override && not is_interface then "override " else if is_virtual then "virtual " else "" in
        let ret_type, args = match cf.cf_type with | TFun (strbtl, t) -> (t, strbtl) | _ -> assert false in
        
        (* public static void funcName *)
        print w "%s %s%s %s %s" (visibility) v_n (String.concat " " modifiers) (if is_new then "" else rett_s (run_follow gen ret_type)) (change_field name);
        let params, params_ext = get_string_params cf.cf_params in
        (* <T>(string arg1, object arg2) with T : object *)
        print w "%s(%s)%s" (params) (String.concat ", " (List.map (fun (name, _, t) -> sprintf "%s %s" (t_s (run_follow gen t)) (change_id name)) args)) (params_ext);
        if is_interface then
          write w ";"
        else begin
          let rec loop meta =
            match meta with
              | [] -> 
                let expr = match cf.cf_expr with
                  | None -> mk (TBlock([])) t_dynamic Ast.null_pos
                  | Some s -> 
                    match s.eexpr with 
                      | TFunction tf ->
                        mk_block (tf.tf_expr)
                      | _ -> assert false (* FIXME *)
                in
                (if is_new then begin
                  let rec get_super_call el =
                    match el with
                      | ( { eexpr = TCall( { eexpr = TConst(TSuper) }, _) } as call) :: rest ->
                        Some call, rest
                      | ( { eexpr = TBlock(bl) } as block ) :: rest ->
                        let ret, mapped = get_super_call bl in
                        ret, ( { block with eexpr = TBlock(mapped) } :: rest )
                      | _ ->
                        None, el
                  in
                  match expr.eexpr with
                    | TBlock(bl) ->
                      let super_call, rest = get_super_call bl in
                      (match super_call with
                        | None -> ()
                        | Some sc ->
                          write w " : ";
                          expr_s w sc
                      );
                      begin_block w;
                      write w "unchecked ";
                      expr_s w { expr with eexpr = TBlock(rest) };
                      end_block w
                    | _ -> assert false
                end else begin
                  begin_block w;
                  write w "unchecked ";
                  expr_s w expr;
                  end_block w
                end)
              | (":functionBody", [Ast.EConst (Ast.String contents),_],_) :: tl ->
                begin_block w;
                write w contents;
                end_block w
              | _ :: tl -> loop tl
          in
          loop cf.cf_meta
          
        end);
      newline w;
      newline w
  in
  
  let check_special_behaviors w cl =
    (if PMap.mem "__get" cl.cl_fields then begin 
      let get = PMap.find "__get" cl.cl_fields in
      let idx_t, v_t = match follow get.cf_type with
        | TFun([_,_,arg_t],ret_t) ->
          t_s (run_follow gen arg_t), t_s (run_follow gen ret_t)
        | _ -> gen.gcon.error "The __get function must be a function with one argument. " get.cf_pos; assert false
      in
      List.iter (fun (cl,args) -> 
        match cl.cl_array_access with
          | None -> ()
          | Some t ->
            let changed_t = apply_params cl.cl_types (List.map (fun _ -> t_dynamic) cl.cl_types) t in
            let t_as_s = t_s (run_follow gen changed_t) in
            print w "%s %s.this[int key]" t_as_s (t_s (TInst(cl, args)));
              begin_block w;
              write w "get";
              begin_block w;
                print w "return ((%s) this.__get(key));" t_as_s;
              end_block w;
              write w "set";
              begin_block w;
                print w "this.__set(key, (%s) value);" v_t;
              end_block w;
            end_block w;
            newline w;
            newline w
      ) cl.cl_implements
    end);
    if is_some cl.cl_array_access then begin
      if not cl.cl_interface && PMap.mem "__get" cl.cl_fields && PMap.mem "__set" cl.cl_fields && not (List.mem "__get" cl.cl_overrides) then begin
        let get = PMap.find "__get" cl.cl_fields in
        let idx_t, v_t = match follow get.cf_type with
          | TFun([_,_,arg_t],ret_t) ->
            t_s (run_follow gen arg_t), t_s (run_follow gen ret_t)
          | _ -> gen.gcon.error "The __get function must be a function with one argument. " get.cf_pos; assert false
        in
        print w "public %s this[%s key]" v_t idx_t;
        begin_block w;
          write w "get";
          begin_block w;
            write w "return this.__get(key);";
          end_block w;
          write w "set";
          begin_block w;
            write w "this.__set(key, value);";
          end_block w;
        end_block w;
        newline w;
        newline w;
      end else if cl.cl_interface && is_hxgen (TClassDecl cl) then begin
        let changed_t = apply_params cl.cl_types (List.map (fun _ -> t_dynamic) cl.cl_types) (get cl.cl_array_access) in
        print w "%s this[int key]" (t_s (run_follow gen changed_t));
        begin_block w;
          write w "get;";
          newline w;
          write w "set;";
          newline w;
        end_block w;
        newline w;
        newline w
      end
    end;
    if PMap.mem "toString" cl.cl_fields && not (List.mem "toString" cl.cl_overrides) then begin
      (* FIXME we need to check for compatible type first *)
      write w "public override string ToString()";
      begin_block w;
      write w "return (string) this.toString();";
      end_block w;
      newline w;
      newline w
    end
  in

  let gen_class w cl =
    let should_close = match fst cl.cl_path with
      | [] -> false
      | ns -> 
        print w "namespace %s" (String.concat "." (change_ns ns));
        begin_block w;
        true
    in
    
    let is_main = 
      match gen.gcon.main_class with
        | Some ( (_,"Main") as path) when path = cl.cl_path ->
          (* 
            for cases where the main class is called Main, there will be a problem with creating the entry point there. 
            In this special case, a special entry point class will be created 
          *)
          write w "public class EntryPoint__Main";
          begin_block w;
          write w "public static void Main()";
          begin_block w;
          print w "global::%s.main();" (path_s path);
          end_block w;
          end_block w;
          false
        | Some path when path = cl.cl_path -> true
        | _ -> false
    in
    
    let clt, access, modifiers = get_class_modifiers cl.cl_meta (if cl.cl_interface then "interface" else "class") "public" [] in
    let is_final = clt = "struct" || has_meta ":final" cl.cl_meta in
    
    print w "%s %s%s %s" access (String.concat " " modifiers) clt (change_clname (snd cl.cl_path));
    (* type parameters *)
    let params, params_ext = get_string_params cl.cl_types in
    let extends_implements = (match cl.cl_super with | None -> [] | Some (cl,p) -> [path_param_s (TClassDecl cl) cl.cl_path p]) @ (List.map (fun (cl,p) -> path_param_s (TClassDecl cl) cl.cl_path p) cl.cl_implements) in
    (match extends_implements with
      | [] -> print w "%s %s" params params_ext
      | _ -> print w "%s : %s %s" params (String.concat ", " extends_implements) params_ext);
    (* class head ok: *)
    (* public class Test<A> : X, Y, Z where A : Y *)
    begin_block w;
    (* our constructor is expected to be a normal "new" function *
    if !strict_mode && is_some cl.cl_constructor then assert false;*)
    
    let rec loop meta =
      match meta with
        | [] ->  ()
        | (":classContents", [Ast.EConst (Ast.String contents),_],_) :: tl ->
          write w contents
        | _ :: tl -> loop tl
    in
    loop cl.cl_meta;
    
    if is_main then begin
      write w "public static void Main()";
      begin_block w;
      write w "main();";
      end_block w
    end;
    
    (match cl.cl_init with
      | None -> ()
      | Some init ->
        print w "static %s() " (snd cl.cl_path);
        expr_s w (mk_block init));
    (if is_some cl.cl_constructor then gen_class_field w false cl is_final (get cl.cl_constructor));
    (if not cl.cl_interface then 
      List.iter (gen_class_field w true cl is_final) cl.cl_ordered_statics);
    List.iter (gen_class_field w false cl is_final) cl.cl_ordered_fields;
    check_special_behaviors w cl;
    end_block w;
    if should_close then end_block w
  in
    

  let gen_enum w e =
    let should_close = match change_ns (fst e.e_path) with
      | [] -> false
      | ns -> 
        print w "namespace %s" (String.concat "." ns);
        begin_block w;
        true
    in
    
    print w "public enum %s" (change_clname (snd e.e_path));
    begin_block w;
    write w (String.concat ", " e.e_names);
    end_block w;
    
    if should_close then end_block w
  in
    
  let module_type_gen w md_tp =
    match md_tp with
      | TClassDecl cl ->
        if not cl.cl_extern then begin
          gen_class w cl;
          newline w;
          newline w
        end;
        (not cl.cl_extern)
      | TEnumDecl e ->
        if not e.e_extern then begin
          gen_enum w e;
          newline w;
          newline w
        end;
        (not e.e_extern)
      | TTypeDecl e -> 
        false
  in

  let module_gen w md_def =
    List.fold_left (fun should md -> module_type_gen w md or should) false md_def.m_types
  in
  
  (* generate source code *)
  init_ctx gen;
  
  Hashtbl.add gen.gspecial_vars "__rethrow__" true;
  Hashtbl.add gen.gspecial_vars "__typeof__" true;
  Hashtbl.add gen.gspecial_vars "__label__" true;
  Hashtbl.add gen.gspecial_vars "__goto__" true;
  Hashtbl.add gen.gspecial_vars "__is__" true;
  Hashtbl.add gen.gspecial_vars "__as__" true;
  Hashtbl.add gen.gspecial_vars "__cs__" true;
  
  gen.greal_type <- real_type;
  gen.greal_type_param <- change_param_type;
  
  SetHXGen.run_filter gen SetHXGen.default_hxgen_func;
  
  let closure_t = ClosuresToClass.DoubleAndDynamicClosureImpl.get_ctx gen 6 in
  
  (*let closure_t = ClosuresToClass.create gen 10 float_cl 
    (fun l -> l)
    (fun l -> l)
    (fun args -> args)
    (fun args -> [])
  in
  ClosuresToClass.configure gen (ClosuresToClass.default_implementation closure_t (fun e _ _ -> e));
  
  StubClosureImpl.configure gen (StubClosureImpl.default_implementation gen float_cl 10 (fun e _ _ -> e));*)
  
  let tnull = match (Hashtbl.find gen.gtypes ([],"Null")) with | TTypeDecl t -> t | _ -> assert false in
  
  HardNullableSynf.configure gen (HardNullableSynf.traverse gen 
    (fun e ->
      match gen.gfollow#run_f e.etype with
        | TType({ t_path = ([], "Null") }, [t]) ->
          { eexpr = TField(e, "value"); etype = t; epos = e.epos }
        | _ -> 
          gen.gcon.error "This expression is not a Nullable expression" e.epos; assert false
    ) 
    (fun v has_value ->
      { eexpr = TNew(null_t, [v.etype], [mk_cast v.etype v; { eexpr = TConst(TBool has_value); etype = gen.gcon.basic.tbool; epos = v.epos } ]); etype = TType(tnull, [v.etype]); epos = v.epos }
    ) 
    (fun e ->
      {
        eexpr = TCall({
            eexpr = TField(e, "toDynamic");
            etype = TFun([], t_dynamic);
            epos = e.epos
          }, []);
        etype = t_dynamic;
        epos = e.epos
      }
    )
  );
  
  IteratorsInterface.configure gen (fun e -> e);
  
  ClosuresToClass.configure gen (ClosuresToClass.default_implementation closure_t (get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"],"Function")) ));
  
  EnumToClass.configure gen (Some (fun e -> mk_cast gen.gcon.basic.tint e)) false true (get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"],"Enum")) );
  
  InterfaceVarsDeleteModf.configure gen;
  
  let dynamic_object = (get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"],"DynamicObject")) ) in
  
  let object_iface = get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"],"IHxObject")) in
  
  (*fixme: THIS IS A HACK. take this off *)
  let empty_e = match (get_type gen (["haxe";"lang"], "EmptyObject")) with | TEnumDecl e -> e | _ -> assert false in
  (*OverloadingCtor.set_new_create_empty gen ({eexpr=TEnumField(empty_e, "EMPTY"); etype=TEnum(empty_e,[]); epos=null_pos;});*)
  
  OverloadingConstructor.configure gen (TEnum(empty_e, [])) ({eexpr=TEnumField(empty_e, "EMPTY"); etype=TEnum(empty_e,[]); epos=null_pos;}) false;
  
  let rcf_static_find = mk_static_field_access_infer (get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"], "FieldLookup"))) "findHash" Ast.null_pos [] in
  let rcf_static_lookup = mk_static_field_access_infer (get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"], "FieldLookup"))) "lookupHash" Ast.null_pos [] in
  
  let can_be_float t = match follow t with
    | TInst({ cl_path = ([], "Int") }, []) 
    | TInst({ cl_path = ([], "Float") }, []) -> true
    | _ -> false
  in
  
  let rcf_on_getset_field main_expr field_expr field may_hash may_set is_unsafe =
    let is_float = can_be_float main_expr.etype in
    let fn_name = if is_some may_set then "setField" else "getField" in
    let fn_name = if is_float then fn_name ^ "_f" else fn_name in
    let pos = field_expr.epos in
    
    let is_unsafe = { eexpr = TConst(TBool is_unsafe); etype = basic.tbool; epos = pos } in
    
    let should_cast = match main_expr.etype with | TInst({ cl_path = ([], "Float") }, []) -> false | _ -> true in
    let infer = mk_static_field_access_infer runtime_cl fn_name field_expr.epos [] in
    let first_args = 
      [ field_expr; { eexpr = TConst(TString field); etype = basic.tstring; epos = pos } ] 
      @ if is_some may_hash then [ { eexpr = TConst(TInt (get may_hash)); etype = basic.tint; epos = pos } ] else []
    in
    let args = first_args @ match is_float, may_set with
      | true, Some(set) ->
        [ if should_cast then mk_cast basic.tfloat set else set ]
      | false, Some(set) ->
        [ set ]
      | _ ->
        [ is_unsafe ]
    in
    
    let call = { main_expr with eexpr = TCall(infer,args) } in
    let call = if is_float && should_cast then mk_cast main_expr.etype call else call in
    call
  in
  
  let rcf_on_call_field ecall field_expr field may_hash args =
    let infer = mk_static_field_access_infer runtime_cl "callField" field_expr.epos [] in
    
    let hash_arg = match may_hash with
      | None -> []
      | Some h -> [ { eexpr = TConst(TInt h); etype = basic.tint; epos = field_expr.epos } ]
    in
    
    let arr_call = if args <> [] then 
      { eexpr = TArrayDecl args; etype = basic.tarray t_dynamic; epos = ecall.epos } 
    else
      null (basic.tarray t_dynamic) ecall.epos
    in
    
    let call_args = 
      [field_expr; { field_expr with eexpr = TConst(TString field); etype = basic.tstring } ] 
        @ hash_arg 
        @ [ arr_call ]
    in
    
    mk_cast ecall.etype { ecall with eexpr = TCall(infer, call_args) }
  in
  
  let rcf_ctx = ReflectionCFs.new_ctx gen closure_t object_iface true rcf_on_getset_field rcf_on_call_field (fun hash hash_array ->
    { hash with eexpr = TCall(rcf_static_find, [hash; hash_array]); etype=basic.tint }
  ) (fun hash -> { hash with eexpr = TCall(rcf_static_lookup, [hash]); etype = gen.gcon.basic.tstring } ) in
  
  ReflectionCFs.set_universal_base_class gen (get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"],"HxObject")) ) object_iface dynamic_object;
  
  ReflectionCFs.implement_class_methods rcf_ctx ( get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"],"Class")) );
  
  ReflectionCFs.configure_dynamic_field_access rcf_ctx false;
  
  let closure_func = ReflectionCFs.implement_closure_cl rcf_ctx ( get_cl (Hashtbl.find gen.gtypes (["haxe";"lang"],"Closure")) ) in
  
  
  ReflectionCFs.configure rcf_ctx;
  
  let objdecl_fn = ReflectionCFs.implement_dynamic_object_ctor rcf_ctx dynamic_object in
  
  ObjectDeclMap.configure gen (ObjectDeclMap.traverse gen objdecl_fn);
  
  InitFunction.configure gen true;
  TArrayTransform.configure gen (TArrayTransform.default_implementation gen (
  fun e -> 
    match e.eexpr with 
      | TArray(e1, e2) -> 
        ( match follow e1.etype with 
          | TDynamic _ | TAnon _ | TMono _ -> true 
          | _ -> false ) 
      | _ -> assert false
  ) "__get" "__set" );
  
  let field_is_dynamic t field =
    match field_access gen (gen.greal_type t) field with
      | FClassField _ -> false
      | _ -> true
  in
  
  let is_type_param e = match follow e with
    | TInst( { cl_kind = KTypeParameter },[]) -> true
    | _ -> false
  in
  
  let is_dynamic_expr e = is_dynamic e.etype || match e.eexpr with
    | TField(tf, f) -> field_is_dynamic tf.etype f
    | _ -> false
  in
  
  let may_nullable t = match gen.gfollow#run_f t with
    | TType({ t_path = ([], "Null") }, [t]) -> 
      (match follow t with
        | TInst({ cl_path = ([], "String") }, [])
        | TInst({ cl_path = ([], "Float") }, [])
        | TInst({ cl_path = (["haxe"], "Int32")}, [] )
        | TInst({ cl_path = (["haxe"], "Int64")}, [] )
        | TInst({ cl_path = ([], "Int") }, [])
        | TEnum({ e_path = ([], "Bool") }, []) -> Some t
        | _ -> None )
    | _ -> None
  in
  
  let is_double t = match follow t with | TInst({ cl_path = ([], "Float") }, []) -> true | _ -> false in
  let is_int t = match follow t with | TInst({ cl_path = ([], "Int") }, []) -> true | _ -> false in
  
  DynamicOperators.configure gen 
    (DynamicOperators.abstract_implementation gen (fun e -> match e.eexpr with
      | TBinop (Ast.OpEq, e1, e2)
      | TBinop (Ast.OpAdd, e1, e2)
      | TBinop (Ast.OpNotEq, e1, e2) -> is_dynamic e1.etype or is_dynamic e2.etype or is_type_param e1.etype or is_type_param e2.etype
      | TBinop (Ast.OpLt, e1, e2)
      | TBinop (Ast.OpLte, e1, e2)
      | TBinop (Ast.OpGte, e1, e2)
      | TBinop (Ast.OpGt, e1, e2) -> is_dynamic e.etype or is_dynamic_expr e1 or is_dynamic_expr e2 or is_string e1.etype or is_string e2.etype
      | TBinop (_, e1, e2) -> is_dynamic e.etype or is_dynamic_expr e1 or is_dynamic_expr e2
      | TUnop (_, _, e1) -> is_dynamic_expr e1
      | _ -> false)
    (fun e1 e2 -> 
      let is_null e = match e.eexpr with | TConst(TNull) | TLocal({ v_name = "__undefined__" }) -> true | _ -> false in
      
      if is_null e1 || is_null e2 then 
        { e1 with eexpr = TBinop(Ast.OpEq, e1, e2); etype = basic.tbool }
      else begin
        let is_ref = match follow e1.etype, follow e2.etype with
          | TDynamic _, _
          | _, TDynamic _
          | TInst({ cl_path = ([], "Float") },[]), _
          | TInst( { cl_path = (["haxe"], "Int32") }, [] ), _
          | TInst( { cl_path = (["haxe"], "Int64") }, [] ), _
          | TInst({ cl_path = ([], "Int") },[]), _
          | TEnum({ e_path = ([], "Bool") },[]), _
          | _, TInst({ cl_path = ([], "Float") },[])
          | _, TInst({ cl_path = ([], "Int") },[]) 
          | _, TInst( { cl_path = (["haxe"], "Int32") }, [] )
          | _, TInst( { cl_path = (["haxe"], "Int64") }, [] )
          | _, TEnum({ e_path = ([], "Bool") },[]) 
          | TInst( { cl_kind = KTypeParameter }, [] ), _
          | _, TInst( { cl_kind = KTypeParameter }, [] ) -> false
          | _, _ -> true
        in
            
        let static = mk_static_field_access_infer (runtime_cl) (if is_ref then "refEq" else "eq") e1.epos [] in
        { eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tbool; epos=e1.epos }
      end
    )
    (fun e e1 e2 -> 
      match may_nullable e1.etype, may_nullable e2.etype with
        | Some t1, Some t2 ->
          let t1, t2 = if is_string t1 || is_string t2 then 
            basic.tstring, basic.tstring 
          else if is_double t1 || is_double t2 then
            basic.tfloat, basic.tfloat
          else if is_int t1 || is_int t2 then
            basic.tint, basic.tint
          else t1, t2 in
          { eexpr = TBinop(Ast.OpAdd, mk_cast t1 e1, mk_cast t2 e2); etype = e.etype; epos = e1.epos }
        | _ ->
          let static = mk_static_field_access_infer (runtime_cl) "plus"  e1.epos [] in
          mk_cast e.etype { eexpr = TCall(static, [e1; e2]); etype = t_dynamic; epos=e1.epos })
    (fun e1 e2 -> 
      if is_string e1.etype then begin
        { e1 with eexpr = TCall({ e1 with eexpr = TField(e1, "compareTo"); etype = TFun(["anotherString",false,gen.gcon.basic.tstring], gen.gcon.basic.tint) }, [ e2 ]); etype = gen.gcon.basic.tint }
      end else begin
        let static = mk_static_field_access_infer (runtime_cl) "compare" e1.epos [] in
        { eexpr = TCall(static, [e1; e2]); etype = gen.gcon.basic.tint; epos=e1.epos } 
      end));
  
  FilterClosures.configure gen (FilterClosures.traverse gen (fun e1 s -> true) closure_func);
    
  let base_exception = get_cl (get_type gen (["cs"; "native"], "Exception")) in
  let base_exception_t = TInst(base_exception, []) in
  
  let hx_exception = get_cl (get_type gen (["haxe";"lang"], "HaxeException")) in
  let hx_exception_t = TInst(hx_exception, []) in
  
  TryCatchWrapper.configure gen 
  (
    TryCatchWrapper.traverse gen 
      (fun t -> try unify t base_exception_t; false with | Unify_error _ -> true)
      (fun throwexpr expr ->
        let wrap_static = mk_static_field_access (hx_exception) "wrap" (TFun([("obj",false,t_dynamic)], base_exception_t)) expr.epos in
        { throwexpr with eexpr = TThrow { expr with eexpr = TCall(wrap_static, [expr]) }; etype = gen.gcon.basic.tvoid }
      ) 
      (fun v_to_unwrap pos ->
        let local = mk_cast hx_exception_t { eexpr = TLocal(v_to_unwrap); etype = v_to_unwrap.v_type; epos = pos } in
        { eexpr = TField(local, "obj"); epos = pos; etype = t_dynamic }
      ) 
      (fun rethrow ->
        { rethrow with eexpr = TCall(mk_local (alloc_var "__rethrow__" t_dynamic) rethrow.epos, [rethrow]) }
      ) 
      (base_exception_t) 
      (hx_exception_t) 
      (fun v e -> e)
  );
  
  let native_class_wrapper = get_cl (get_type gen (["haxe";"lang"], "NativeClassWrapper")) in
  
  let get_typeof e =
    { e with eexpr = TCall( { eexpr = TLocal( alloc_var "__typeof__" t_dynamic ); etype = t_dynamic; epos = e.epos }, [e] ) }
  in
  
  ClassInstance.configure gen (ClassInstance.traverse gen (fun e mt ->
    if is_hxgen mt then begin
      {
        eexpr = TCall({
          eexpr = TField(e, gen.gmk_internal_name "hx" "getClassStatic");
          etype = TFun([], e.etype);
          epos = e.epos
        }, []);
        etype = e.etype;
        epos = e.epos;
      }
    end else begin
      {
        eexpr = TNew(native_class_wrapper, [], [ get_typeof e ]);
        etype = e.etype;
        epos = e.epos
      }
    end
  ));
  
  let v = alloc_var "$type_param" t_dynamic in
  TypeParams.configure gen (fun ecall efield params elist ->
    { ecall with eexpr = TCall(efield, (List.map (fun t -> { eexpr = TLocal(v); etype = t; epos = ecall.epos }) params) @ elist) }
  );
  
  CastDetect.configure gen (CastDetect.default_implementation gen (Some (TEnum(empty_e, []))));
  
  (*FollowAll.configure gen;*)
  
  SwitchToIf.configure gen (SwitchToIf.traverse gen (fun e ->
    match e.eexpr with
      | TSwitch(cond, cases, def) ->
        (match gen.gfollow#run_f cond.etype with
          | TInst({ cl_path = ([], "Int") },[])
          | TInst({ cl_path = ([], "String") },[]) ->
            (List.exists (fun (c,_) -> 
              List.exists (fun expr -> match expr.eexpr with | TConst _ -> false | _ -> true ) c
            ) cases)
          | _ -> true
        )
      | _ -> assert false
  ) true ) ;
  
  (*
    starting to set gtparam_cast.
  *)
  
  (* NativeArray: the most important. *)
  
  (*
    var new_arr = new NativeArray<TO_T>(old_arr.Length);
    var i = -1;
    while( i < old_arr.Length )
    {
      new_arr[i] = (TO_T) old_arr[i];
    }
  *)
  
  let native_arr_cl = get_cl ( get_type gen (["cs"], "NativeArray") ) in
  
  let get_narr_param t = match follow t with
    | TInst({ cl_path = (["cs"], "NativeArray") }, [param]) -> param
    | _ -> assert false
  in
  
  let gtparam_cast_native_array e to_t =
    let old_param = get_narr_param e.etype in
    let new_param = get_narr_param to_t in
    
    let new_v = mk_temp gen "new_arr" to_t in
    let i = mk_temp gen "i" basic.tint in
    let old_len = { eexpr = TField(e, "Length"); etype = basic.tint; epos = e.epos } in
    let block = [
      { 
        eexpr = TVars(
        [ 
          new_v, Some( {
            eexpr = TNew(native_arr_cl, [new_param], [old_len] );
            etype = to_t;
            epos = e.epos
          } );
          i, Some( mk_int gen (-1) e.epos )
        ]); 
        etype = basic.tvoid; 
        epos = e.epos };
      { 
        eexpr = TWhile(
          { 
            eexpr = TBinop(
              Ast.OpLt, 
              { eexpr = TUnop(Ast.Increment, Ast.Prefix, mk_local i e.epos); etype = basic.tint; epos = e.epos },
              old_len
            );
            etype = basic.tbool;
            epos = e.epos
          },
          {
            eexpr = TBinop(
              Ast.OpAssign,
              { eexpr = TArray(mk_local new_v e.epos, mk_local i e.epos); etype = new_param; epos = e.epos },
              mk_cast new_param (mk_cast t_dynamic { eexpr = TArray(e, mk_local i e.epos); etype = old_param; epos = e.epos })
            );
            etype = new_param;
            epos = e.epos
          },
          Ast.NormalWhile
        );
        etype = basic.tvoid;
        epos = e.epos;
      };
      mk_local new_v e.epos
    ] in
    { eexpr = TBlock(block); etype = to_t; epos = e.epos }
  in
  
  Hashtbl.add gen.gtparam_cast (["cs"], "NativeArray") gtparam_cast_native_array;
  
  (* end set gtparam_cast *)
  
  let my_ifaces = TypeParams.RealTypeParams.configure gen (fun e t -> gen.gcon.warning ("Cannot cast to " ^ (debug_type t)) e.epos; mk_cast t e) in
  
  ifaces := my_ifaces;
  
  ExpressionUnwrap.configure gen (ExpressionUnwrap.traverse gen (fun e -> Some { eexpr = TVars([mk_temp gen "expr" e.etype, Some e]); etype = gen.gcon.basic.tvoid; epos = e.epos }));
  
  IntDivisionSynf.configure gen (IntDivisionSynf.default_implementation gen true);
  
  ArrayDeclSynf.configure gen (ArrayDeclSynf.default_implementation gen native_arr_cl);
  
  let goto_special = alloc_var "__goto__" t_dynamic in
  let label_special = alloc_var "__label__" t_dynamic in
  SwitchBreakSynf.configure gen (SwitchBreakSynf.traverse gen 
    (fun e_loop n api ->
      api ({ eexpr = TCall( mk_local label_special e_loop.epos, [ mk_int gen n e_loop.epos ] ); etype = t_dynamic; epos = e_loop.epos }) false;
      e_loop
    ) 
    (fun e_break n api ->
      { eexpr = TCall( mk_local goto_special e_break.epos, [ mk_int gen n e_break.epos ] ); etype = t_dynamic; epos = e_break.epos }
    )
  );
  
  CSharpSpecificSynf.configure gen (CSharpSpecificSynf.traverse gen runtime_cl);
  
  run_filters gen;
  
  TypeParams.RenameTypeParameters.run gen;
  
  let t = Common.timer "code generation" in
  
	generate_modules gen "cs" "src" module_gen;
  
  t()

(* end of configure function *)
	
let before_generate con = 
  List.iter (Codegen.fix_overrides con) con.types

let generate con =
  let gen = new_ctx con in
  configure gen
  
