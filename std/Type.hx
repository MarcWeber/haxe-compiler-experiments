
/**
	An abstract type that represents a Class.
	See [Type] for the haXe Reflection API.
**/
enum Class {
}

/**
	An abstract type that represents an Enum.
	See [Type] for the haXe Reflection API.
**/
enum Enum {
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
	TClass( c : Class );
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
	public static function getClass( o : Dynamic ) : Class untyped {
		#if flash9
			var cname = __global__["flash.utils.getQualifiedClassName"](o);
			if( cname == "null" || cname == "Object" || cname == "int" || cname == "Number" || cname == "Boolean" )
				return null;
			if( o.hasOwnProperty("prototype") )
				return null;
			var c = __global__["flash.utils.getDefinitionByName"](cname);
			if( c.__isenum )
				return null;
			return c;
		#else flash
			if( o.__enum__ != null )
				return null;
			return o.__class__;
		#else js
			if( o == null )
				return null;
			if( o.__enum__ != null )
				return null;
			return o.__class__;
		#else neko
			if( __dollar__typeof(o) != __dollar__tobject )
				return null;
			var p = __dollar__objgetproto(o);
			if( p == null )
				return null;
			return p.__class__;
		#else error
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
			var c = __global__["flash.utils.getDefinitionByName"](cname);
			if( !c.__isenum )
				return null;
			return c;
		#else flash
			return o.__enum__;
		#else js
			if( o == null )
				return null;
			return o.__enum__;
		#else neko
			if( __dollar__typeof(o) != __dollar__tobject )
				return null;
			return o.__enum__;
		#else error
		#end
	}


	/**
		Returns the super-class of a class, or null if no super class.
	**/
	public static function getSuperClass( c : Class ) : Class untyped {
		#if flash9
			var cname = __global__["flash.utils.getQualifiedSuperclassName"](c);
			if( cname == "Object" )
				return null;
			return __global__["flash.utils.getDefinitionByName"](cname);
		#else true
			return c.__super__;
		#end
	}


	/**
		Returns the complete name of a class.
	**/
	public static function getClassName( c : Class ) : String {
		if( c == null )
			return null;
		#if flash9
			var name = untyped __global__["flash.utils.getQualifiedClassName"](c);
			if( name == "flash::FlashXml__" )
				return "Xml";
			return name;
		#else true
			var a : Array<String> = untyped c.__name__;
			return a.join(".");
		#end
	}

	/**
		Returns the complete name of an enum.
	**/
	public static function getEnumName( e : Enum ) : String {
		#if flash9
			var n = untyped __global__["flash.utils.getQualifiedClassName"](e);
			return n;
		#else true
			var a : Array<String> = untyped e.__ename__;
			return a.join(".");
		#end
	}

	/**
		Evaluates a class from a name. The class must have been compiled
		to be accessible.
	**/
	public static function resolveClass( name : String ) : Class {
		var cl : Class;
		untyped {
		#if flash9
			try {
				cl = __global__["flash.utils.getDefinitionByName"](name);
				if( cl.__isenum )
					return null;
				return cl; // skip test below
			} catch( e : Dynamic ) {
				return null;
			}
		#else flash
			cl = __eval__(name);
		#else js
			try {
				cl = eval(name);
			} catch( e : Dynamic ) {
				cl = null;
			}
		#else neko
			var path = name.split(".");
			cl = Reflect.field(untyped neko.Boot.__classes,path[0]);
			var i = 1;
			while( cl != null && i < path.length ) {
				cl = Reflect.field(cl,path[i]);
				i += 1;
			}
		#else error
		#end
		// ensure that this is a class
		if( cl == null || cl.__name__ == null )
			return null;
		}
		return cl;
	}


	/**
		Evaluates an enum from a name. The enum must have been compiled
		to be accessible.
	**/
	public static function resolveEnum( name : String ) : Enum {
		var e : Dynamic;
		untyped {
		#if flash9
			try {
				e = __global__["flash.utils.getDefinitionByName"](name);
				if( !e.__isenum )
					return null;
				return e;
			} catch( e : Dynamic ) {
				return null;
			}
		#else flash
			e = __eval__(name);
		#else js
			try {
				e = eval(name);
			} catch( e : Dynamic ) {
				e = null;
			}
		#else neko
			var path = name.split(".");
			e = Reflect.field(neko.Boot.__classes,path[0]);
			var i = 1;
			while( e != null && i < path.length ) {
				e = Reflect.field(e,path[i]);
				i += 1;
			}
		#else error
		#end
		// ensure that this is an enum
		if( e == null || e.__ename__ == null )
			return null;
		}
		return e;
	}


	/**
		Similar to [Reflect.createInstance] excepts that the constructor is not called.
		This enables you to create an instance without any side-effect.
	**/
	public static function createEmptyInstance( cl : Class ) untyped {
		#if flash9
			return cl.__construct__.call(null,null);
		#else flash
			var o : Dynamic = __new__(_global["Object"]);
			o.__proto__ = cl.prototype;
			return o;
		#else js
			return __new__(cl,__js__("$_"));
		#else neko
			var o = __dollar__new(null);
			__dollar__objsetproto(o,cl.prototype);
			return o;
		#else error
		#end
	}

	#if flash9
	static function describe( t : Dynamic, fact : Bool ) {
		var fields = new Array();
		var xml : Dynamic = untyped __global__["flash.utils.describeType"](t);
		if( fact )
			xml = xml.factory;
		var methods = xml.child("method");
		for( i in 0...methods.length() )
			fields.push( Std.string(untyped methods[i].attribute("name")) );
		var vars = xml.child("variable");
		for( i in 0...vars.length() )
			fields.push( Std.string(untyped vars[i].attribute("name")) );
		return fields;
	}
	#end

	/**
		Returns the list of instance fields.
	**/
	public static function getInstanceFields( c : Class ) : Array<String> {
		#if flash9
			return describe(c,true);
		#else true
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
	public static function getClassFields( c : Class ) : Array<String> {
		#if flash9
			var a = describe(c,false);
			a.remove("__construct__");
			return a;
		#else true
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
		#if flash9
			return describe(e,false);
		#else true
			var a = Reflect.fields(e);
			a.remove(__unprotect__("__ename__"));
			#if neko
			a.remove("prototype");
			#end
			return a;
		#end
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
		#else flash9
		var cname = __global__["flash.utils.getQualifiedClassName"](v);
		switch(cname) {
		case "null": return TNull;
		case "void": return TNull; // undefined
		case "int": return TInt;
		case "Number": return TFloat;
		case "Boolean": return TBool;
		case "Object": return TObject;
		default:
			try {
				var c = __global__["flash.utils.getDefinitionByName"](cname);
				if( v.hasOwnProperty("prototype") )
					return TObject;
				if( c.__isenum )
					return TEnum(c);
				return TClass(c);
			} catch( e : Dynamic ) {
				return TFunction;
			}
		}
		#else (flash || js)
		switch( #if flash __typeof__ #else true __js__("typeof") #end(v) ) {
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
		#else error
		#end
	}

}

