
/**
	An abstract type that represents an Enum.
	See [Type] for the haXe Reflection API.
**/
extern class Enum {
}

/**
	The diffent possible runtime types of a value.
	See [Type] for the haXe Reflection API.
**/
enum ValueType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TFunction;
	TClass( c : Class<Dynamic> );
	TEnum( e : Enum );
	TUnknown;
}

/**
	The haXe Reflection API enables you to retreive informations about any value,
	Classes and Enums at runtime.
**/
class Type {

	/**
		Returns the class of a value or [null] if this value is not a Class instance.
	**/
	public static function getClass<T>( o : T ) : Class<T> untyped {
		#if flash9
			var cname = __global__["flash.utils.getQualifiedClassName"](o);
			if( cname == "null" || cname == "Object" || cname == "int" || cname == "Number" || cname == "Boolean" )
				return null;
			if( o.hasOwnProperty("prototype") )
				return null;
			var c = __as__(__global__["flash.utils.getDefinitionByName"](cname),Class);
			if( c.__isenum )
				return null;
			return c;
		#elseif flash
			if( o.__enum__ != null )
				return null;
			return o.__class__;
		#elseif js
			if( o == null )
				return null;
			if( o.__enum__ != null )
				return null;
			return o.__class__;
		#elseif neko
			if( __dollar__typeof(o) != __dollar__tobject )
				return null;
			var p = __dollar__objgetproto(o);
			if( p == null )
				return null;
			return p.__class__;
		#else
			return null;
		#end
	}

	/**
		Returns the enum of a value or [null] if this value is not an Enum instance.
	**/
	public static function getEnum( o : Dynamic ) : Enum untyped {
		#if flash9
			var cname = __global__["flash.utils.getQualifiedClassName"](o);
			if( cname == "null" || cname.substr(0,8) == "builtin." )
				return null;
			// getEnum(Enum) should be null
			if( o.hasOwnProperty("prototype") )
				return null;
			var c = __as__(__global__["flash.utils.getDefinitionByName"](cname),Class);
			if( !c.__isenum )
				return null;
			return c;
		#elseif flash
			return o.__enum__;
		#elseif js
			if( o == null )
				return null;
			return o.__enum__;
		#elseif neko
			if( __dollar__typeof(o) != __dollar__tobject )
				return null;
			return o.__enum__;
		#else
			return null;
		#end
	}


	/**
		Returns the super-class of a class, or null if no super class.
	**/
	public static function getSuperClass( c : Class<Dynamic> ) : Class<Dynamic> untyped {
		#if flash9
			var cname = __global__["flash.utils.getQualifiedSuperclassName"](c);
			if( cname == "Object" )
				return null;
			return __as__(__global__["flash.utils.getDefinitionByName"](cname),Class);
		#else
			return c.__super__;
		#end
	}


	/**
		Returns the complete name of a class.
	**/
	public static function getClassName( c : Class<Dynamic> ) : String {
		if( c == null )
			return null;
		#if flash9
			var str : String = untyped __global__["flash.utils.getQualifiedClassName"](c);
			switch( str ) {
			case "int": return "Int";
			case "Number": return "Float";
			case "Boolean": return "Bool";
			default:
			}
			return str.split("::").join(".");
		#else
			var a : Array<String> = untyped c.__name__;
			return a.join(".");
		#end
	}

	/**
		Returns the complete name of an enum.
	**/
	public static function getEnumName( e : Enum ) : String {
		#if flash9
			return getClassName(cast e);
		#else
			var a : Array<String> = untyped e.__ename__;
			return a.join(".");
		#end
	}

	/**
		Evaluates a class from a name. The class must have been compiled
		to be accessible.
	**/
	public static function resolveClass( name : String ) : Class<Dynamic> untyped {
		var cl : Class<Dynamic>;
		#if flash9
			try {
				cl = __as__(__global__["flash.utils.getDefinitionByName"](name),Class);
				if( cl.__isenum )
					return null;
				return cl; // skip test below
			} catch( e : Dynamic ) {
				switch( name ) {
				case "Int": return Int;
				case "Float": return Float;
				}
				return null;
			}
		#elseif flash
			cl = __eval__(name);
		#elseif js
			try {
				cl = eval(name);
			} catch( e : Dynamic ) {
				cl = null;
			}
		#elseif neko
			var path = name.split(".");
			cl = Reflect.field(untyped neko.Boot.__classes,path[0]);
			var i = 1;
			while( cl != null && i < path.length ) {
				cl = Reflect.field(cl,path[i]);
				i += 1;
			}
		#else
			cl = null;
		#end
		// ensure that this is a class
		if( cl == null || cl.__name__ == null )
			return null;
		return cl;
	}


	/**
		Evaluates an enum from a name. The enum must have been compiled
		to be accessible.
	**/
	public static function resolveEnum( name : String ) : Enum untyped {
		var e : Dynamic;
		#if flash9
			try {
				e = __global__["flash.utils.getDefinitionByName"](name);
				if( !e.__isenum )
					return null;
				return e;
			} catch( e : Dynamic ) {
				if( name == "Bool" ) return Bool;
				return null;
			}
		#elseif flash
			e = __eval__(name);
		#elseif js
			try {
				e = eval(name);
			} catch( err : Dynamic ) {
				e = null;
			}
		#elseif neko
			var path = name.split(".");
			e = Reflect.field(neko.Boot.__classes,path[0]);
			var i = 1;
			while( e != null && i < path.length ) {
				e = Reflect.field(e,path[i]);
				i += 1;
			}
		#else
			e = null;
		#end
		// ensure that this is an enum
		if( e == null || e.__ename__ == null )
			return null;
		return e;
	}

	/**
		Creates an instance of the given class with the list of constructor arguments.
	**/
	public static function createInstance<T>( cl : Class<T>, args : Array<Dynamic> ) : T untyped {
		#if flash9
			return switch( args.length ) {
			case 0: __new__(cl);
			case 1: __new__(cl,args[0]);
			case 2: __new__(cl,args[0],args[1]);
			case 3: __new__(cl,args[0],args[1],args[2]);
			case 4: __new__(cl,args[0],args[1],args[2],args[3]);
			case 5: __new__(cl,args[0],args[1],args[2],args[3],args[4]);
			case 6: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5]);
			case 7: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6]);
			case 8: __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7]);
			default: throw "Too many arguments";
			}
		#elseif flash
			var o = { __constructor__ : cl, __proto__ : cl.prototype };
			cl["apply"](o,args);
			return o;
		#elseif neko
			return __dollar__call(__dollar__objget(cl,__dollar__hash("new".__s)),cl,args.__neko());
		#elseif js
			if( args.length <= 3 )
				return __new__(cl,args[0],args[1],args[2]);
			if( args.length > 8 )
				throw "Too many arguments";
			return __new__(cl,args[0],args[1],args[2],args[3],args[4],args[5],args[6],args[7]);
		#else
			return null;
		#end
	}

	/**
		Similar to [Reflect.createInstance] excepts that the constructor is not called.
		This enables you to create an instance without any side-effect.
	**/
	public static function createEmptyInstance<T>( cl : Class<T> ) : T untyped {
		#if flash9
			try {
				flash.Boot.skip_constructor = true;
				var i = __new__(cl);
				flash.Boot.skip_constructor = false;
				return i;
			} catch( e : Dynamic ) {
				flash.Boot.skip_constructor = false;
				throw e;
			}
			return null;
		#elseif flash
			var o : Dynamic = __new__(_global["Object"]);
			o.__proto__ = cl.prototype;
			return o;
		#elseif js
			return __new__(cl,__js__("$_"));
		#elseif neko
			var o = __dollar__new(null);
			__dollar__objsetproto(o,cl.prototype);
			return o;
		#else
			return null;
		#end
	}

	/**
		Create an instance of an enum by using a constructor name and parameters.
	**/
	public static function createEnum( e : Enum, constr : String, ?params : Array<Dynamic> ) : Dynamic {
		var f = Reflect.field(e,constr);
		if( f == null ) throw "No such constructor "+constr;
		if( Reflect.isFunction(f) ) {
			if( params == null ) throw "Constructor "+constr+" need parameters";
			return Reflect.callMethod(e,f,params);
		}
		if( params != null && params.length != 0 )
			throw "Constructor "+constr+" does not need parameters";
		return f;
	}

	#if flash9
	static function describe( t : Dynamic, fact : Bool ) untyped {
		var fields = new Array();
		var xml : flash.xml.XML = __global__["flash.utils.describeType"](t);
		if( fact )
			xml = xml.factory[0];
		var methods = xml.child("method");
		for( i in 0...methods.length() )
			fields.push( Std.string(methods[i].attribute("name")) );
		var vars = xml.child("variable");
		for( i in 0...vars.length() )
			fields.push( Std.string(vars[i].attribute("name")) );
		return fields;
	}
	#end

	/**
		Returns the list of instance fields.
	**/
	public static function getInstanceFields( c : Class<Dynamic> ) : Array<String> {
		#if flash9
			return describe(c,true);
		#else
			var a = Reflect.fields(untyped c.prototype);
			c = untyped c.__super__;
			while( c != null ) {
				a = a.concat(Reflect.fields(untyped c.prototype));
				c = untyped c.__super__;
			}
			while( a.remove(__unprotect__("__class__")) ) {
				#if neko
				a.remove("__serialize");
				a.remove("__string");
				#end
			}
			return a;
		#end
	}

	/**
		Returns the list of a class static fields.
	**/
	public static function getClassFields( c : Class<Dynamic> ) : Array<String> {
		#if flash9
			var a = describe(c,false);
			a.remove("__construct__");
			return a;
		#else
			var a = Reflect.fields(c);
			a.remove(__unprotect__("__name__"));
			a.remove(__unprotect__("__interfaces__"));
			a.remove(__unprotect__("__super__"));
			#if js
			a.remove("prototype");
			#end
			#if neko
			a.remove("__construct__");
			a.remove("prototype");
			a.remove("new");
			#end
			return a;
		#end
	}

	/**
		Returns all the available constructor names for an enum.
	**/
	public static function getEnumConstructs( e : Enum ) : Array<String> {
		return untyped e.__constructs__;
	}

	/**
		Returns the runtime type of a value.
	**/
	public static function typeof( v : Dynamic ) : ValueType untyped {
		#if neko
			return switch( __dollar__typeof(v) ) {
			case __dollar__tnull: TNull;
			case __dollar__tint: TInt;
			case __dollar__tfloat: TFloat;
			case __dollar__tbool: TBool;
			case __dollar__tfunction: TFunction;
			case __dollar__tobject:
				var c = v.__class__;
				if( c != null )
					TClass(c);
				else {
					var e = v.__enum__;
					if( e != null )
						TEnum(e);
					else
						TObject;
				}
			default: TUnknown;
			}
		#elseif flash9
			var cname = __global__["flash.utils.getQualifiedClassName"](v);
			switch(cname) {
			case "null": return TNull;
			case "void": return TNull; // undefined
			case "int": return TInt;
			case "Number": return TFloat;
			case "Boolean": return TBool;
			case "Object": return TObject;
			default:
				var c : Dynamic = null;
				try {
					c = __global__["flash.utils.getDefinitionByName"](cname);
					if( v.hasOwnProperty("prototype") )
						return TObject;
					if( c.__isenum )
						return TEnum(c);
					return TClass(c);
				} catch( e : Dynamic ) {
					if( cname == "builtin.as$0::MethodClosure" || cname.indexOf("-") != -1 )
						return TFunction;
					return if( c == null ) TFunction else TClass(c);
				}
			}
			return null;
		#elseif (flash || js)
			switch( #if flash __typeof__ #else __js__("typeof") #end(v) ) {
			#if flash
			case "null": return TNull;
			#end
			case "boolean": return TBool;
			case "string": return TClass(String);
			case "number":
				if( v+1 == v )
					return TFloat;
				if( Math.ceil(v) == v )
					return TInt;
				return TFloat;
			case "object":
				#if js
				if( v == null )
					return TNull;
				#end
				var e = v.__enum__;
				if( e != null )
					return TEnum(e);
				var c = v.__class__;
				if( c != null )
					return TClass(c);
				return TObject;
			case "function":
				if( v.__name__ != null )
					return TObject;
				return TFunction;
			case "undefined":
				return TNull;
			default:
				return TUnknown;
			}
		#else
			return TUnknown;
		#end
	}

	/**
		Recursively compare two enums constructors and parameters.
	**/
	public static function enumEq<T>( a : T, b : T ) : Bool untyped {
		if( a == b )
			return true;
		#if neko
			try {
				if( a.__enum__ == null || a.tag != b.tag )
					return false;
			} catch( e : Dynamic ) {
				return false;
			}
			for( i in 0...__dollar__asize(a.args) )
				if( !enumEq(a.args[i],b.args[i]) )
					return false;
		#elseif flash9
			try {
				if( a.tag != b.tag )
					return false;
				for( i in 0...a.params.length )
					if( !enumEq(a.params[i],b.params[i]) )
						return false;
			} catch( e : Dynamic ) {
				return false;
			}
		#else
			if( a[0] != b[0] )
				return false;
			for( i in 2...a.length )
				if( !enumEq(a[i],b[i]) )
					return false;
			var e = a.__enum__;
			if( e != b.__enum__ || e == null )
				return false;
		#end
		return true;
	}

	/**
		Returns the constructor of an enum
	**/
	public static function enumConstructor( e : Dynamic ) : String {
		#if neko
			return new String(e.tag);
		#elseif flash9
			return e.tag;
		#else
			return e[0];
		#end
	}

	/**
		Returns the parameters of an enum
	**/
	public static function enumParameters( e : Dynamic ) : Array<Dynamic> {
		#if neko
			return if( e.args == null ) [] else untyped Array.new1(e.args,__dollar__asize(e.args));
		#elseif flash9
			return if( e.params == null ) [] else e.params;
		#else
			return e.slice(2);
		#end
	}

	/**
		Returns the index of the constructor of an enum
	**/
	public inline static function enumIndex( e : Dynamic ) : Int {
		#if (neko || flash9)
			return e.index;
		#else
			return e[1];
		#end
	}

}

