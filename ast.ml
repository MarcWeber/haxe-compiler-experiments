(*
 * Copyright (C)2005-2013 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)

type pos = {
	pfile : string;
	pmin : int;
	pmax : int;
}

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

module Meta = struct
	type strict_meta =
		| Abstract
		| Access
		| Alias
		| Allow
		| Annotation
		| ArrayAccess
		| AutoBuild
		| BaseInterface
		| Bind
		| Bitmap
		| Build
		| BuildXml
		| Class
		| ClassCode
		| Commutative
		| CompilerGenerated
		| CoreApi
		| CoreType
		| CppFileCode
		| CppNamespaceCode
		| Debug
		| Decl
		| DefParam
		| Depend
		| Deprecated
		| DynamicObject
		| Enum
		| Expose
		| Extern
		| FakeEnum
		| File
		| Final
		| Font
		| From
		| FunctionCode
		| FunctionTailCode
		| Generic
		| Getter
		| Hack
		| HaxeGeneric
		| HeaderClassCode
		| HeaderCode
		| HeaderNamespaceCode
		| HxGen
		| IfFeature
		| Impl
		| Include
		| InitPackage
		| Internal
		| IsVar
		| JavaNative
		| Keep
		| KeepInit
		| KeepSub
		| Meta
		| Macro
		| MaybeUsed
		| MultiType
		| Native
		| NativeGen
		| NativeGeneric
		| NoCompletion
		| NoDebug
		| NoDoc
		| NoPackageRestrict
		| NoStack
		| NotNull
		| NoUsing
		| Ns
		| Op
		| Optional
		| Overload
		| PrivateAccess
		| Protected
		| Public
		| PublicFields
		| ReadOnly
		| RealPath
		| Remove
		| Require
		| ReplaceReflection
		| Rtti
		| Runtime
		| RuntimeValue
		| Setter
		| SkipCtor
		| SkipReflection
		| Sound
		| Struct
		| SuppressWarnings
		| Synchronized
		| Throws
		| To
		| Transient
		| ValueUsed
		| VarArgs
		| Volatile
		| UnifyMinDynamic
		| Unreflective
		| Unsafe
		| Usage
		| Used
		| Last
		| Dollar of string
		| Custom of string

	type meta_usage =
		| TClass
		| TClassField
		| TAbstract
		| TAbstractField
		| TEnum
		| TTypedef

	type meta_parameter =
		| HasParam of string
		| Platform of platform
		| Platforms of platform list
		| UsedOn of meta_usage
		| UsedOnEither of meta_usage list

	let to_string = function
		| Abstract -> ":abstract",("",[Platforms [Java;Cs]])
		| Access -> ":access",("Forces private access to package, type or field",[HasParam "Target path";UsedOnEither [TClass;TClassField]])
		| Alias -> ":alias",("",[Platforms [Java;Cs]])
		| Allow -> ":allow",("Allows private access from package, type or field",[HasParam "Target path";UsedOnEither [TClass;TClassField]])
		| Annotation -> ":annotation",("",[Platforms [Java;Cs]])
		| ArrayAccess -> ":arrayAccess",("Allows [] access on an abstract",[UsedOnEither [TAbstract;TAbstractField]])
		| AutoBuild -> ":autoBuild",("Extends @:build metadata to all extending and implementing classes",[HasParam "Build macro call";UsedOn TClass])
		| BaseInterface -> ":baseInterface",("",[Platforms [Java;Cs]])
		| Bind -> ":bind",("Override Swf class declaration",[Platform Flash;UsedOn TClass])
		| Bitmap -> ":bitmap",("Embeds given bitmap data into the class (must extend flash.display.BitmapData)",[HasParam "Bitmap file path";UsedOn TClass;Platform Flash])
		| Build -> ":build",("Builds a class or enum from a macro",[HasParam "Build macro call";UsedOnEither [TClass;TEnum]])
		| BuildXml -> ":buildXml",("",[Platform Cpp])
		| Class -> ":class",("",[Platforms [Java;Cs]])
		| ClassCode -> ":classCode",("",[Platforms [Java;Cs]])
		| Commutative -> ":commutative",("Declares an abstract operator as commutative",[UsedOn TAbstractField])
		| CompilerGenerated -> ":compilerGenerated",("",[Platforms [Java;Cs]])
		| CoreApi -> ":coreApi",("Identifies this class as a core api class (forces Api check)",[UsedOnEither [TClass;TEnum;TTypedef;TAbstract]])
		| CoreType -> ":coreType",("Identifies an abstract as core type so that it requires no implementation",[UsedOn TAbstract])
		| CppFileCode -> ":cppFileCode",("",[Platform Cpp])
		| CppNamespaceCode -> ":cppNamespaceCode",("",[Platform Cpp])
		| Debug -> ":debug",("Forces debug information to be generated into the Swf even without -debug",[UsedOnEither [TClass;TClassField]; Platform Flash])
		| Decl -> ":decl",("",[Platform Cpp])
		| DefParam -> ":defParam",("?",[])
		| Depend -> ":depend",("",[Platform Cpp])
		| Deprecated -> ":deprecated",("",[Platforms [Java;Cs]])
		| DynamicObject -> ":dynamicObject",("",[Platforms [Java;Cs]])
		| Enum -> ":enum",("",[Platforms [Java;Cs]])
		| Expose -> ":expose",("Makes the class available on the window object",[HasParam "?Name=Class path";UsedOn TClass;Platform Js])
		| Extern -> ":extern",("Marks the field as extern so it is not generated",[UsedOn TClassField])
		| FakeEnum -> ":fakeEnum",("Treat enum as collection of values of the specified type",[HasParam "Type name";UsedOn TEnum])
		| File -> ":file",("Includes a given binary file into the target Swf and associates it with the class (must extend flash.utils.ByteArray)",[HasParam "File path";UsedOn TClass;Platform Flash])
		| Final -> ":final",("Prevents a class from being extended",[UsedOn TClass])
		| Font -> ":font",("Embeds the given TrueType font into the class (must extend flash.text.Font)",[HasParam "TTF path";HasParam "Range String";UsedOn TClass])
		| From -> ":from",("Specifies that the field of the abstract is a cast operation from the type identified in the function",[UsedOn TAbstractField])
		| FunctionCode -> ":functionCode",("",[Platform Cpp])
		| FunctionTailCode -> ":functionTailCode",("",[Platform Cpp])
		| Generic -> ":generic",("Marks a class or class field as generic so each type parameter combination generates its own type/field",[UsedOnEither [TClass;TClassField]])
		| Getter -> ":getter",("Generates a native getter function on the given field",[HasParam "Class field name";UsedOn TClassField;Platform Flash])
		| Hack -> ":hack",("Allows extending classes marked as @:final",[UsedOn TClass])
		| HaxeGeneric -> ":haxeGeneric",("",[Platforms [Java;Cs]])
		| HeaderClassCode -> ":headerClassCode",("",[Platform Cpp])
		| HeaderCode -> ":headerCode",("",[Platform Cpp])
		| HeaderNamespaceCode -> ":headerNamespaceCode",("",[Platform Cpp])
		| HxGen -> ":hxGen",("",[Platforms [Java;Cs]])
		| IfFeature -> ":ifFeature",("Causes a field to be kept by DCE if the given feature is part of the compilation",[HasParam "Feature name";UsedOn TClassField])
		| Impl -> ":impl",("Used internally to mark abstract implementation fields",[UsedOn TAbstractField])
		| Include -> ":include",("",[Platform Cpp])
		| InitPackage -> ":initPackage",("?",[])
		| Internal -> ":internal",("",[Platforms [Java;Cs]])
		| IsVar -> ":isVar",("Forces a physical field to be generated for properties that otherwise would not require one",[UsedOn TClassField])
		| JavaNative -> ":javaNative",("",[Platforms [Java;Cs]])
		| Keep -> ":keep",("Causes a field or type to be kept by DCE",[])
		| KeepInit -> ":keepInit",("Causes a class to be kept by DCE even if all its field are removed",[UsedOn TClass])
		| KeepSub -> ":keepSub",("Extends @:keep metadata to all implementing and extending classes",[UsedOn TClass])
		| Meta -> ":meta",("Internally used to mark a class field as being the metadata field",[])
		| Macro -> ":macro",("(deprecated)",[])
		| MaybeUsed -> ":maybeUsed",("Internally used by DCE to mark fields that might be kept",[])
		| MultiType -> ":multiType",("Specifies that an abstract chooses its this-type from its @:to functions",[UsedOn TAbstract])
		| Native -> ":native",("Rewrites the path of a class or enum during generation",[HasParam "Output type path";UsedOnEither [TClass;TEnum]])
		| NativeGen -> ":nativeGen",("",[Platforms [Java;Cs]])
		| NativeGeneric -> ":nativeGeneric",("",[Platforms [Java;Cs]])
		| NoCompletion -> ":noCompletion",("Prevents the compiler from suggesting completion on this field",[UsedOn TClassField])
		| NoDebug -> ":noDebug",("Does not generate debug information into the Swf even if -debug is set",[UsedOnEither [TClass;TClassField];Platform Flash])
		| NoDoc -> ":noDoc",("Prevents a type from being included in documentation generation",[])
		| NoPackageRestrict -> ":noPackageRestrict",("?",[])
		| NoStack -> ":noStack",("",[Platform Cpp])
		| NotNull -> ":notNull",("Declares an abstract type as not accepting null values",[UsedOn TAbstract])
		| NoUsing -> ":noUsing",("Prevents a field from being used with 'using'",[UsedOn TClassField])
		| Ns -> ":ns",("Internally used by the Swf generator to handle namespaces",[Platform Flash])
		| Op -> ":op",("Declares an abstract field as being an operator overload",[HasParam "The operation";UsedOn TAbstractField])
		| Optional -> ":optional",("Marks the field of a structure as optional",[UsedOn TClassField])
		| Overload -> ":overload",("Allows the field to be called with different argument types",[HasParam "Function specification (no expression)";UsedOn TClassField])
		| Public -> ":public",("Marks a class field as being public",[UsedOn TClassField])
		| PublicFields -> ":publicFields",("Forces all class fields of inheriting classes to be public",[UsedOn TClass])
		| PrivateAccess -> ":privateAccess",("Internally used by the typer to allow context-sensitive private access",[])
		| Protected -> ":protected",("Marks a class field as being protected",[UsedOn TClassField])
		| ReadOnly -> ":readOnly",("",[Platforms [Java;Cs]])
		| RealPath -> ":realPath",("Internally used on @:native types to retain original path information",[])
		| Remove -> ":remove",("Causes an interface to be removed from all implementing classes before generation",[UsedOn TClass])
		| Require -> ":require",("Allows access to a field only if the specified compiler flag is set",[HasParam "Compiler flag to check";UsedOn TClassField])
		| ReplaceReflection -> ":replaceReflection",("",[Platforms [Java;Cs]])
		| Rtti -> ":rtti",("Adds runtime type informations",[UsedOn TClass])
		| Runtime -> ":runtime",("?",[])
		| RuntimeValue -> ":runtimeValue",("Marks an abstract as being a runtime value",[UsedOn TAbstract])
		| Setter -> ":setter",("Generates a native getter function on the given field",[HasParam "Class field name";UsedOn TClassField;Platform Flash])
		| SkipCtor -> ":skipCtor",("",[Platforms [Java;Cs]])
		| SkipReflection -> ":skipReflection",("",[Platforms [Java;Cs]])
		| Sound -> ":sound",( "Includes a given .wav or .mp3 file into the target Swf and associates it with the class (must extend flash.media.Sound)",[HasParam "File path";UsedOn TClass;Platform Flash])
		| Struct -> ":struct",("",[Platforms [Java;Cs]])
		| SuppressWarnings -> ":suppressWarnings",("",[Platforms [Java;Cs]])
		| Synchronized -> ":synchronized",("",[Platforms [Java;Cs]])
		| Throws -> ":throws",("",[Platforms [Java;Cs]])
		| To -> ":to",("Specifies that the field of the abstract is a cast operation to the type identified in the function",[UsedOn TAbstractField])
		| Transient -> ":transient",("",[Platforms [Java;Cs]])
		| ValueUsed -> ":valueUsed",("Internally used by DCE to mark an abstract value as used",[])
		| VarArgs -> ":varArgs",("",[Platforms [Java;Cs]])
		| Volatile -> ":volatile",("",[Platforms [Java;Cs]])
		| UnifyMinDynamic -> ":unifyMinDynamic",("Allows a collection of types to unify to Dynamic",[UsedOn TClassField])
		| Unreflective -> ":unreflective",("",[Platform Cpp])
		| Unsafe -> ":unsafe",("",[Platforms [Java;Cs]])
		| Usage -> ":usage",("?",[])
		| Used -> ":used",("Internally used by DCE to mark a class or field as used",[])
		| Last -> assert false
		| Dollar s -> "$" ^ s,("",[])
		| Custom s -> s,("",[])

	let hmeta =
		let h = Hashtbl.create 0 in
		let rec loop i =
			let m = Obj.magic i in
			if m <> Last then begin
				Hashtbl.add h (fst (to_string m)) m;
				loop (i + 1);
			end;
		in
		loop 0;
		h

	let parse s = try Hashtbl.find hmeta (":" ^ s) with Not_found -> Custom (":" ^ s)

	let from_string s =
		if s = "" then Custom "" else match s.[0] with
		| ':' -> (try Hashtbl.find hmeta s with Not_found -> Custom s)
		| '$' -> Dollar (String.sub s 1 (String.length s - 1))
		| _ -> Custom s

	(* removed
	:functionBody -> :functionCode
	*)
	let has m ml = List.exists (fun (m2,_,_) -> m = m2) ml
	let get m ml = List.find (fun (m2,_,_) -> m = m2) ml
end

type keyword =
	| Function
	| Class
	| Var
	| If
	| Else
	| While
	| Do
	| For
	| Break
	| Continue
	| Return
	| Extends
	| Implements
	| Import
	| Switch
	| Case
	| Default
	| Static
	| Public
	| Private
	| Try
	| Catch
	| New
	| This
	| Throw
	| Extern
	| Enum
	| In
	| Interface
	| Untyped
	| Cast
	| Override
	| Typedef
	| Dynamic
	| Package
	| Inline
	| Using
	| Null
	| True
	| False
	| Abstract
	| Macro

type binop =
	| OpAdd
	| OpMult
	| OpDiv
	| OpSub
	| OpAssign
	| OpEq
	| OpNotEq
	| OpGt
	| OpGte
	| OpLt
	| OpLte
	| OpAnd
	| OpOr
	| OpXor
	| OpBoolAnd
	| OpBoolOr
	| OpShl
	| OpShr
	| OpUShr
	| OpMod
	| OpAssignOp of binop
	| OpInterval
	| OpArrow

type unop =
	| Increment
	| Decrement
	| Not
	| Neg
	| NegBits

type constant =
	| Int of string
	| Float of string
	| String of string
	| Ident of string
	| Regexp of string * string

type token =
	| Eof
	| Const of constant
	| Kwd of keyword
	| Comment of string
	| CommentLine of string
	| Binop of binop
	| Unop of unop
	| Semicolon
	| Comma
	| BrOpen
	| BrClose
	| BkOpen
	| BkClose
	| POpen
	| PClose
	| Dot
	| DblDot
	| Arrow
	| IntInterval of string
	| Sharp of string
	| Question
	| At
	| Dollar of string

type unop_flag =
	| Prefix
	| Postfix

type while_flag =
	| NormalWhile
	| DoWhile

type type_path = {
	tpackage : string list;
	tname : string;
	tparams : type_param_or_const list;
	tsub : string option;
}

and type_param_or_const =
	| TPType of complex_type
	| TPExpr of expr

and complex_type =
	| CTPath of type_path
	| CTFunction of complex_type list * complex_type
	| CTAnonymous of class_field list
	| CTParent of complex_type
	| CTExtend of type_path * class_field list
	| CTOptional of complex_type

and func = {
	f_params : type_param list;
	f_args : (string * bool * complex_type option * expr option) list;
	f_type : complex_type option;
	f_expr : expr option;
}

and expr_def =
	| EConst of constant
	| EArray of expr * expr
	| EBinop of binop * expr * expr
	| EField of expr * string
	| EParenthesis of expr
	| EObjectDecl of (string * expr) list
	| EArrayDecl of expr list
	| ECall of expr * expr list
	| ENew of type_path * expr list
	| EUnop of unop * unop_flag * expr
	| EVars of (string * complex_type option * expr option) list
	| EFunction of string option * func
	| EBlock of expr list
	| EFor of expr * expr
	| EIn of expr * expr
	| EIf of expr * expr * expr option
	| EWhile of expr * expr * while_flag
	| ESwitch of expr * (expr list * expr option * expr option) list * expr option option
	| ETry of expr * (string * complex_type * expr) list
	| EReturn of expr option
	| EBreak
	| EContinue
	| EUntyped of expr
	| EThrow of expr
	| ECast of expr * complex_type option
	| EDisplay of expr * bool
	| EDisplayNew of type_path
	| ETernary of expr * expr * expr
	| ECheckType of expr * complex_type
	| EMeta of metadata_entry * expr

and expr = expr_def * pos

and type_param = {
	tp_name : string;
	tp_params :	type_param list;
	tp_constraints : complex_type list;
}

and documentation = string option

and metadata_entry = (Meta.strict_meta * expr list * pos)
and metadata = metadata_entry list

and access =
	| APublic
	| APrivate
	| AStatic
	| AOverride
	| ADynamic
	| AInline
	| AMacro

and class_field_kind =
	| FVar of complex_type option * expr option
	| FFun of func
	| FProp of string * string * complex_type option * expr option

and class_field = {
	cff_name : string;
	cff_doc : documentation;
	cff_pos : pos;
	mutable cff_meta : metadata;
	mutable cff_access : access list;
	mutable cff_kind : class_field_kind;
}

type enum_flag =
	| EPrivate
	| EExtern

type class_flag =
	| HInterface
	| HExtern
	| HPrivate
	| HExtends of type_path
	| HImplements of type_path

type abstract_flag =
	| APrivAbstract
	| AFromType of complex_type
	| AToType of complex_type
	| AIsType of complex_type

type enum_constructor = {
	ec_name : string;
	ec_doc : documentation;
	ec_meta : metadata;
	ec_args : (string * bool * complex_type) list;
	ec_pos : pos;
	ec_params : type_param list;
	ec_type : complex_type option;
}

type ('a,'b) definition = {
	d_name : string;
	d_doc : documentation;
	d_params : type_param list;
	d_meta : metadata;
	d_flags : 'a list;
	d_data : 'b;
}

type import_mode =
	| INormal
	| IAsName of string
	| IAll

type type_def =
	| EClass of (class_flag, class_field list) definition
	| EEnum of (enum_flag, enum_constructor list) definition
	| ETypedef of (enum_flag, complex_type) definition
	| EAbstract of (abstract_flag, class_field list) definition
	| EImport of (string * pos) list * import_mode
	| EUsing of type_path

type type_decl = type_def * pos

type package = string list * type_decl list

let is_lower_ident i =
	let rec loop p =
		match String.unsafe_get i p with
		| 'a'..'z' -> true
		| '_' -> if p + 1 < String.length i then loop (p + 1) else true
		| _ -> false
	in
	loop 0

let pos = snd

let is_postfix (e,_) = function
	| Increment | Decrement -> (match e with EConst _ | EField _ | EArray _ -> true | _ -> false)
	| Not | Neg | NegBits -> false

let is_prefix = function
	| Increment | Decrement -> true
	| Not | Neg | NegBits -> true

let base_class_name = snd

let null_pos = { pfile = "?"; pmin = -1; pmax = -1 }

let punion p p2 =
	{
		pfile = p.pfile;
		pmin = min p.pmin p2.pmin;
		pmax = max p.pmax p2.pmax;
	}

let rec punion_el el = match el with
	| [] ->
		null_pos
	| (_,p) :: [] ->
		p
	| (_,p) :: el ->
		punion p (punion_el el)

let s_type_path (p,s) = match p with [] -> s | _ -> String.concat "." p ^ "." ^ s

let parse_path s =
	match List.rev (ExtString.String.nsplit s ".") with
	| [] -> failwith "Invalid empty path"
	| x :: l -> List.rev l, x

let s_escape s =
	let b = Buffer.create (String.length s) in
	for i = 0 to (String.length s) - 1 do
		match s.[i] with
		| '\n' -> Buffer.add_string b "\\n"
		| '\t' -> Buffer.add_string b "\\t"
		| '\r' -> Buffer.add_string b "\\r"
		| '"' -> Buffer.add_string b "\\\""
		| '\\' -> Buffer.add_string b "\\\\"
		| c -> Buffer.add_char b c
	done;
	Buffer.contents b

let s_constant = function
	| Int s -> s
	| Float s -> s
	| String s -> "\"" ^ s_escape s ^ "\""
	| Ident s -> s
	| Regexp (r,o) -> "~/" ^ r ^ "/"

let s_access = function
	| APublic -> "public"
	| APrivate -> "private"
	| AStatic -> "static"
	| AOverride -> "override"
	| ADynamic -> "dynamic"
	| AInline -> "inline"
	| AMacro -> "macro"

let s_keyword = function
	| Function -> "function"
	| Class -> "class"
	| Static -> "static"
	| Var -> "var"
	| If -> "if"
	| Else -> "else"
	| While -> "while"
	| Do -> "do"
	| For -> "for"
	| Break -> "break"
	| Return -> "return"
	| Continue -> "continue"
	| Extends -> "extends"
	| Implements -> "implements"
	| Import -> "import"
	| Switch -> "switch"
	| Case -> "case"
	| Default -> "default"
	| Private -> "private"
	| Public -> "public"
	| Try -> "try"
	| Catch -> "catch"
	| New -> "new"
	| This -> "this"
	| Throw -> "throw"
	| Extern -> "extern"
	| Enum -> "enum"
	| In -> "in"
	| Interface -> "interface"
	| Untyped -> "untyped"
	| Cast -> "cast"
	| Override -> "override"
	| Typedef -> "typedef"
	| Dynamic -> "dynamic"
	| Package -> "package"
	| Inline -> "inline"
	| Using -> "using"
	| Null -> "null"
	| True -> "true"
	| False -> "false"
	| Abstract -> "abstract"
	| Macro -> "macro"

let rec s_binop = function
	| OpAdd -> "+"
	| OpMult -> "*"
	| OpDiv -> "/"
	| OpSub -> "-"
	| OpAssign -> "="
	| OpEq -> "=="
	| OpNotEq -> "!="
	| OpGte -> ">="
	| OpLte -> "<="
	| OpGt -> ">"
	| OpLt -> "<"
	| OpAnd -> "&"
	| OpOr -> "|"
	| OpXor -> "^"
	| OpBoolAnd -> "&&"
	| OpBoolOr -> "||"
	| OpShr -> ">>"
	| OpUShr -> ">>>"
	| OpShl -> "<<"
	| OpMod -> "%"
	| OpAssignOp op -> s_binop op ^ "="
	| OpInterval -> "..."
	| OpArrow -> "=>"

let s_unop = function
	| Increment -> "++"
	| Decrement -> "--"
	| Not -> "!"
	| Neg -> "-"
	| NegBits -> "~"

let s_token = function
	| Eof -> "<end of file>"
	| Const c -> s_constant c
	| Kwd k -> s_keyword k
	| Comment s -> "/*"^s^"*/"
	| CommentLine s -> "//"^s
	| Binop o -> s_binop o
	| Unop o -> s_unop o
	| Semicolon -> ";"
	| Comma -> ","
	| BkOpen -> "["
	| BkClose -> "]"
	| BrOpen -> "{"
	| BrClose -> "}"
	| POpen -> "("
	| PClose -> ")"
	| Dot -> "."
	| DblDot -> ":"
	| Arrow -> "->"
	| IntInterval s -> s ^ "..."
	| Sharp s -> "#" ^ s
	| Question -> "?"
	| At -> "@"
	| Dollar v -> "$" ^ v

let unescape s =
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
					let c = (try char_of_int (int_of_string ("0o" ^ String.sub s i 3)) with _ -> raise Exit) in
					Buffer.add_char b c;
					inext := !inext + 2;
				| 'x' ->
					let c = (try char_of_int (int_of_string ("0x" ^ String.sub s (i+1) 2)) with _ -> raise Exit) in
					Buffer.add_char b c;
					inext := !inext + 2;
				| _ ->
					raise Exit);
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


let map_expr loop (e,p) =
	let opt f o =
		match o with None -> None | Some v -> Some (f v)
	in
	let rec tparam = function
		| TPType t -> TPType (ctype t)
		| TPExpr e -> TPExpr (loop e)
	and cfield f =
		{ f with cff_kind = (match f.cff_kind with
			| FVar (t,e) -> FVar (opt ctype t, opt loop e)
			| FFun f -> FFun (func f)
			| FProp (get,set,t,e) -> FProp (get,set,opt ctype t,opt loop e))
		}
	and ctype = function
		| CTPath t -> CTPath (tpath t)
		| CTFunction (cl,c) -> CTFunction (List.map ctype cl, ctype c)
		| CTAnonymous fl -> CTAnonymous (List.map cfield fl)
		| CTParent t -> CTParent (ctype t)
		| CTExtend (t,fl) -> CTExtend (tpath t, List.map cfield fl)
		| CTOptional t -> CTOptional (ctype t)
	and tparamdecl t =
		{ tp_name = t.tp_name; tp_constraints = List.map ctype t.tp_constraints; tp_params = List.map tparamdecl t.tp_params }
	and func f =
		{
			f_params = List.map tparamdecl f.f_params;
			f_args = List.map (fun (n,o,t,e) -> n,o,opt ctype t,opt loop e) f.f_args;
			f_type = opt ctype f.f_type;
			f_expr = opt loop f.f_expr;
		}
	and tpath t = { t with tparams = List.map tparam t.tparams }
	in
	let e = (match e with
	| EConst _ -> e
	| EArray (e1,e2) -> EArray (loop e1, loop e2)
	| EBinop (op,e1,e2) -> EBinop (op,loop e1, loop e2)
	| EField (e,f) -> EField (loop e, f)
	| EParenthesis e -> EParenthesis (loop e)
	| EObjectDecl fl -> EObjectDecl (List.map (fun (f,e) -> f,loop e) fl)
	| EArrayDecl el -> EArrayDecl (List.map loop el)
	| ECall (e,el) -> ECall (loop e, List.map loop el)
	| ENew (t,el) -> ENew (tpath t,List.map loop el)
	| EUnop (op,f,e) -> EUnop (op,f,loop e)
	| EVars vl -> EVars (List.map (fun (n,t,eo) -> n,opt ctype t,opt loop eo) vl)
	| EFunction (n,f) -> EFunction (n,func f)
	| EBlock el -> EBlock (List.map loop el)
	| EFor (e1,e2) -> EFor (loop e1, loop e2)
	| EIn (e1,e2) -> EIn (loop e1, loop e2)
	| EIf (e,e1,e2) -> EIf (loop e, loop e1, opt loop e2)
	| EWhile (econd,e,f) -> EWhile (loop econd, loop e, f)
	| ESwitch (e,cases,def) -> ESwitch (loop e, List.map (fun (el,eg,e) -> List.map loop el, opt loop eg, opt loop e) cases, opt (opt loop) def)
	| ETry (e, catches) -> ETry (loop e, List.map (fun (n,t,e) -> n,ctype t,loop e) catches)
	| EReturn e -> EReturn (opt loop e)
	| EBreak -> EBreak
	| EContinue -> EContinue
	| EUntyped e -> EUntyped (loop e)
	| EThrow e -> EThrow (loop e)
	| ECast (e,t) -> ECast (loop e,opt ctype t)
	| EDisplay (e,f) -> EDisplay (loop e,f)
	| EDisplayNew t -> EDisplayNew (tpath t)
	| ETernary (e1,e2,e3) -> ETernary (loop e1,loop e2,loop e3)
	| ECheckType (e,t) -> ECheckType (loop e, ctype t)
	| EMeta (m,e) -> EMeta(m, loop e)
	) in
	(e,p)

let reify in_macro =
	let cur_pos = ref None in
	let mk_enum ename n vl p =
		let constr = (EConst (Ident n),p) in
		match vl with
		| [] -> constr
		| _ -> (ECall (constr,vl),p)
	in
	let to_const c p =
		let cst n v = mk_enum "Constant" n [EConst (String v),p] p in
		match c with
		| Int i -> cst "CInt" i
		| String s -> cst "CString" s
		| Float s -> cst "CFloat" s
		| Ident s -> cst "CIdent" s
		| Regexp (r,o) -> mk_enum "Constant" "CRegexp" [(EConst (String r),p);(EConst (String o),p)] p
	in
	let rec to_binop o p =
		let op n = mk_enum "Binop" n [] p in
		match o with
		| OpAdd -> op "OpAdd"
		| OpMult -> op "OpMult"
		| OpDiv -> op "OpDiv"
		| OpSub -> op "OpSub"
		| OpAssign -> op "OpAssign"
		| OpEq -> op "OpEq"
		| OpNotEq -> op "OpNotEq"
		| OpGt -> op "OpGt"
		| OpGte -> op "OpGte"
		| OpLt -> op "OpLt"
		| OpLte -> op "OpLte"
		| OpAnd -> op "OpAnd"
		| OpOr -> op "OpOr"
		| OpXor -> op "OpXor"
		| OpBoolAnd -> op "OpBoolAnd"
		| OpBoolOr -> op "OpBoolOr"
		| OpShl -> op "OpShl"
		| OpShr -> op "OpShr"
		| OpUShr -> op "OpUShr"
		| OpMod -> op "OpMod"
		| OpAssignOp o -> mk_enum "Binop" "OpAssignOp" [to_binop o p] p
		| OpInterval -> op "OpInterval"
		| OpArrow -> op "OpArrow"
	in
	let to_string s p =
		let len = String.length s in
		if len > 1 && s.[0] = '$' then
			(EConst (Ident (String.sub s 1 (len - 1))),p)
		else
			(EConst (String s),p)
	in
	let to_array f a p =
		(EArrayDecl (List.map (fun s -> f s p) a),p)
	in
	let to_null p =
		(EConst (Ident "null"),p)
	in
	let to_opt f v p =
		match v with
		| None -> to_null p
		| Some v -> f v p
	in
	let to_bool o p =
		(EConst (Ident (if o then "true" else "false")),p)
	in
	let to_obj fields p =
		(EObjectDecl fields,p)
	in
	let rec to_tparam t p =
		let n, v = (match t with
			| TPType t -> "TPType", to_ctype t p
			| TPExpr e -> "TPExpr", to_expr e p
		) in
		mk_enum "TypeParam" n [v] p
	and to_tpath t p =
		let fields = [
			("pack", to_array to_string t.tpackage p);
			("name", to_string t.tname p);
			("params", to_array to_tparam t.tparams p);
		] in
		to_obj (match t.tsub with None -> fields | Some s -> fields @ ["sub",to_string s p]) p
	and to_ctype t p =
		let ct n vl = mk_enum "ComplexType" n vl p in
		match t with
		| CTPath { tpackage = []; tparams = []; tsub = None; tname = n } when n.[0] = '$' ->
			to_string n p
		| CTPath t -> ct "TPath" [to_tpath t p]
		| CTFunction (args,ret) -> ct "TFunction" [to_array to_ctype args p; to_ctype ret p]
		| CTAnonymous fields -> ct "TAnonymous" [to_array to_cfield fields p]
		| CTParent t -> ct "TParent" [to_ctype t p]
		| CTExtend (t,fields) -> ct "TExtend" [to_tpath t p; to_array to_cfield fields p]
		| CTOptional t -> ct "TOptional" [to_ctype t p]
	and to_fun f p =
		let farg (n,o,t,e) p =
			let fields = [
				"name", to_string n p;
				"opt", to_bool o p;
				"type", to_opt to_ctype t p;
			] in
			to_obj (match e with None -> fields | Some e -> fields @ ["value",to_expr e p]) p
		in
		let rec fparam t p =
			let fields = [
				"name", to_string t.tp_name p;
				"constraints", to_array to_ctype t.tp_constraints p;
				"params", to_array fparam t.tp_params p;
			] in
			to_obj fields p
		in
		let fields = [
			("args",to_array farg f.f_args p);
			("ret",to_opt to_ctype f.f_type p);
			("expr",to_opt to_expr f.f_expr p);
			("params",to_array fparam f.f_params p);
		] in
		to_obj fields p
	and to_cfield f p =
		let p = f.cff_pos in
		let to_access a p =
			let n = (match a with
			| APublic -> "APublic"
			| APrivate -> "APrivate"
			| AStatic -> "AStatic"
			| AOverride -> "AOverride"
			| ADynamic -> "ADynamic"
			| AInline -> "AInline"
			| AMacro -> "AMacro"
			) in
			mk_enum "Access" n [] p
		in
		let to_kind k =
			let n, vl = (match k with
				| FVar (ct,e) -> "FVar", [to_opt to_ctype ct p;to_opt to_expr e p]
				| FFun f -> "FFun", [to_fun f p]
				| FProp (get,set,t,e) -> "FProp", [to_string get p; to_string set p; to_opt to_ctype t p; to_opt to_expr e p]
			) in
			mk_enum "FieldType" n vl p
		in
		let fields = [
			Some ("name", to_string f.cff_name p);
			(match f.cff_doc with None -> None | Some s -> Some ("doc", to_string s p));
			(match f.cff_access with [] -> None | l -> Some ("access", to_array to_access l p));
			Some ("kind", to_kind f.cff_kind);
			Some ("pos", to_pos f.cff_pos);
			(match f.cff_meta with [] -> None | l -> Some ("meta", to_meta f.cff_meta p));
		] in
		let fields = List.rev (List.fold_left (fun acc v -> match v with None -> acc | Some e -> e :: acc) [] fields) in
		to_obj fields p
	and to_meta m p =
		to_array (fun (m,el,p) _ ->
			let fields = [
				"name", to_string (fst (Meta.to_string m)) p;
				"params", to_expr_array el p;
				"pos", to_pos p;
			] in
			to_obj fields p
		) m p
	and to_pos p =
		match !cur_pos with
		| Some p ->
			p
		| None ->
		let file = (EConst (String p.pfile),p) in
		let pmin = (EConst (Int (string_of_int p.pmin)),p) in
		let pmax = (EConst (Int (string_of_int p.pmax)),p) in
		if in_macro then
			(EUntyped (ECall ((EConst (Ident "$mk_pos"),p),[file;pmin;pmax]),p),p)
		else
			to_obj [("file",file);("min",pmin);("max",pmax)] p
	and to_expr_array a p = match a with
		| [EMeta ((Meta.Dollar "a",[],_),e1),_] -> (match fst e1 with EArrayDecl el -> to_expr_array el p | _ -> e1)
		| _ -> to_array to_expr a p
	and to_expr e _ =
		let p = snd e in
		let expr n vl =
			let e = mk_enum "ExprDef" n vl p in
			to_obj [("expr",e);("pos",to_pos p)] p
		in
		let loop e = to_expr e (snd e) in
		match fst e with
		| EConst (Ident n) when n.[0] = '$' && String.length n > 1 ->
			to_string n p
		| EConst c ->
			expr "EConst" [to_const c p]
		| EArray (e1,e2) ->
			expr "EArray" [loop e1;loop e2]
		| EBinop (op,e1,e2) ->
			expr "EBinop" [to_binop op p; loop e1; loop e2]
		| EField (e,s) ->
			expr "EField" [loop e; to_string s p]
		| EParenthesis e ->
			expr "EParenthesis" [loop e]
		| EObjectDecl fl ->
			expr "EObjectDecl" [to_array (fun (f,e) -> to_obj [("field",to_string f p);("expr",loop e)]) fl p]
		| EArrayDecl el ->
			expr "EArrayDecl" [to_expr_array el p]
		| ECall (e,el) ->
			expr "ECall" [loop e;to_expr_array el p]
		| ENew (t,el) ->
			expr "ENew" [to_tpath t p;to_expr_array el p]
		| EUnop (op,flag,e) ->
			let op = mk_enum "Unop" (match op with
				| Increment -> "OpIncrement"
				| Decrement -> "OpDecrement"
				| Not -> "OpNot"
				| Neg -> "OpNeg"
				| NegBits -> "OpNegBits"
			) [] p in
			expr "EUnop" [op;to_bool (flag = Postfix) p;loop e]
		| EVars vl ->
			expr "EVars" [to_array (fun (v,t,e) p ->
				let fields = [
					"name", to_string v p;
					"type", to_opt to_ctype t p;
					"expr", to_opt to_expr e p;
				] in
				to_obj fields p
			) vl p]
		| EFunction (name,f) ->
			expr "EFunction" [to_opt to_string name p; to_fun f p]
		| EBlock el ->
			expr "EBlock" [to_expr_array el p]
		| EFor (e1,e2) ->
			expr "EFor" [loop e1;loop e2]
		| EIn (e1,e2) ->
			expr "EIn" [loop e1;loop e2]
		| EIf (e1,e2,eelse) ->
			expr "EIf" [loop e1;loop e2;to_opt to_expr eelse p]
		| EWhile (e1,e2,flag) ->
			expr "EWhile" [loop e1;loop e2;to_bool (flag = NormalWhile) p]
		| ESwitch (e1,cases,def) ->
			let scase (el,eg,e) p =
				to_obj [("values",to_expr_array el p);"guard",to_opt to_expr eg p;"expr",to_opt to_expr e p] p
			in
			expr "ESwitch" [loop e1;to_array scase cases p;to_opt (to_opt to_expr) def p]
		| ETry (e1,catches) ->
			let scatch (n,t,e) p =
				to_obj [("name",to_string n p);("type",to_ctype t p);("expr",loop e)] p
			in
			expr "ETry" [loop e1;to_array scatch catches p]
		| EReturn eo ->
			expr "EReturn" [to_opt to_expr eo p]
		| EBreak ->
			expr "EBreak" []
		| EContinue ->
			expr "EContinue" []
		| EUntyped e ->
			expr "EUntyped" [loop e]
		| EThrow e ->
			expr "EThrow" [loop e]
		| ECast (e,ct) ->
			expr "ECast" [loop e; to_opt to_ctype ct p]
		| EDisplay (e,flag) ->
			expr "EDisplay" [loop e; to_bool flag p]
		| EDisplayNew t ->
			expr "EDisplayNew" [to_tpath t p]
		| ETernary (e1,e2,e3) ->
			expr "ETernary" [loop e1;loop e2;loop e3]
		| ECheckType (e1,ct) ->
			expr "ECheckType" [loop e1; to_ctype ct p]
		| EMeta ((m,ml,p),e1) ->
			match m, ml with
			| Meta.Dollar ("" | "e"), _ ->
				e1
			| Meta.Dollar "a", _ ->
				expr "EArrayDecl" (match fst e1 with EArrayDecl el -> [to_expr_array el p] | _ -> [e1])
			| Meta.Dollar "b", _ ->
				expr "EBlock" [e1]
			(* TODO: can $v and $i be implemented better? *)
			| Meta.Dollar "v", _ ->
				(ECall ((EField ((EField ((EField ((EConst (Ident "haxe"),p),"macro"),p),"Context"),p),"makeExpr"),p),[e; to_pos (pos e)]),p)
			| Meta.Dollar "i", _ ->
				(ECall ((EField ((EField ((EField ((EConst (Ident "haxe"),p),"macro"),p),"ExprTools"),p),"asIdent"),p),[e; to_pos (pos e)]),p)
			| Meta.Dollar "p", _ ->
				(ECall ((EField ((EField ((EField ((EConst (Ident "haxe"),p),"macro"),p),"ExprTools"),p),"toFieldExpr"),p),[e]),p)
			| Meta.Custom ":pos", [pexpr] ->
				let old = !cur_pos in
				cur_pos := Some pexpr;
				let e = loop e1 in
				cur_pos := old;
				e
			| _ ->
				expr "EMeta" [to_obj [("name",to_string (fst (Meta.to_string m)) p);("params",to_expr_array ml p);("pos",to_pos p)] p;loop e1]
	and to_tparam_decl p t =
		to_obj [
			"name", to_string t.tp_name p;
			"params", (EArrayDecl (List.map (to_tparam_decl p) t.tp_params),p);
			"constraints", (EArrayDecl (List.map (fun t -> to_ctype t p) t.tp_constraints),p)
		] p
	and to_type_def (t,p) =
		match t with
		| EClass d ->
			let ext = ref None and impl = ref [] and interf = ref false in
			List.iter (function
				| HExtern | HPrivate -> ()
				| HInterface -> interf := true;
				| HExtends t -> ext := Some (to_tpath t p)
				| HImplements i -> impl := (to_tpath i p) :: !impl
			) d.d_flags;
			to_obj [
				"pack", (EArrayDecl [],p);
				"name", to_string d.d_name p;
				"pos", to_pos p;
				"meta", to_meta d.d_meta p;
				"params", (EArrayDecl (List.map (to_tparam_decl p) d.d_params),p);
				"isExtern", to_bool (List.mem HExtern d.d_flags) p;
				"kind", mk_enum "TypeDefKind" "TDClass" [(match !ext with None -> (EConst (Ident "null"),p) | Some t -> t);(EArrayDecl (List.rev !impl),p);to_bool !interf p] p;
				"fields", (EArrayDecl (List.map (fun f -> to_cfield f p) d.d_data),p)
			] p
		| _ -> assert false
	in
	(fun e -> to_expr e (snd e)), to_ctype, to_type_def
