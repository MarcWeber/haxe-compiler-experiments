/*
 * Copyright (c) 2005, The haXe Project Contributors
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

enum BasicType {
	TNull;
	TInt;
	TFloat;
	TBool;
	TObject;
	TEnum;
	TFunction;
	TUnknown;
}

/**
	A class contains meta data about the class instance and an access to the prototype.
**/
interface Class {

	/**
		The complete class name
	**/
	var __name__ : Array<String>;

	/**
		The Class constructor, or [null] if the class does not have one
	**/
	var __construct__ : Dynamic;

	/**
		The superclass which the class extends, or [null] if the class does not extend any class
	**/
	var __super__ : Class;

	/**
		The list of interfaces implemented by this class.
	**/
	var __interfaces__ : Array<Class>;

	/**
		The class prototype, containing all class fields
	**/
	var prototype : Dynamic;
}

/**
	The Reflect API is a way to manipulate values dynamicly through an
	abstract interface in an untyped manner. Use with care.
**/
class Reflect {

	/**
		Returns the basic type of any value.
	**/
	public static function typeof( v : Dynamic ) : BasicType {
		untyped {
		#if flash
			var t = __typeof__(v);
			if( t == "number" ) {
				if( Math.ceil(v) == v && isFinite(v) )
					return TInt;
				return TFloat;
			}
			if( v == true || v == false )
				return TBool;
			if( v == null )
				return TNull;
			if( t == "function" ) {
				if( v.__interfaces__ == null )
					return TFunction;
				return TObject;
			}
			if( t == "object" || t == "string" ) {
				if( v.__enum__ != null )
					return TEnum;
				return TObject;
			}
			return TUnknown;
		#else neko
			return switch( __dollar__typeof(v) ) {
			case __dollar__tint: TInt;
			case __dollar__tfloat: TFloat;
			case __dollar__tbool: TBool;
			case __dollar__tfunction: TFunction;
			case __dollar__tobject: if( v.__enum__ == null ) TObject else TEnum;
			case __dollar__tnull: TNull;
			default: TUnknown;
			}
		#else js
			var t = __js__("typeof")(v);
			if( t == "number" ) {
				if( Math.ceil(v) == v && isFinite(v) )
					return TInt;
				return TFloat;
			}
			if( v == true || v == false )
				return TBool;
			if( v == null )
				return TNull;
			if( t == "function" ) {
				if( v.__interfaces__ == null )
					return TFunction;
				return TObject;
			}
			if( t == "object" || t == "string" ) {
				if( v.__enum__ != null )
					return TEnum;
				return TObject;
			}
			return TUnknown;
	 	#else error
	 	#end
		}
	}

	/**
		Creates an empty object.
	**/
	public static function empty() : {} {
		return untyped
		#if flash
			__new__(_global["Object"])
		#else neko
			__dollar__new(null)
		#else js
			__new__("Object")
		#else error
		#end
			;
	}

	/**
		Creates an instance of the given class with the list of constructor arguments.
	**/
	public static function createInstance( cl : Dynamic, args : Array<Dynamic> ) : Dynamic {
		if( args.length > 5 )
			throw "Too many arguments";
		return untyped
		#if flash
			switch( args.length ) {
			case 0:
				__new__(cl);
			case 1:
				__new__(cl,args[0]);
			case 2:
				__new__(cl,args[0],args[1]);
			case 3:
				__new__(cl,args[0],args[1],args[2]);
			case 4:
				__new__(cl,args[0],args[1],args[2],args[3]);
			case 5:
				__new__(cl,args[0],args[1],args[2],args[3],args[4]);
			default:
				null;
			}
		#else neko
			__dollar__call(__dollar__objget(cl,__dollar__hash("new".__s)),cl,args.__a)
		#else js
			__new__(cl,args[0],args[1],args[2],args[3],args[4])
		#else error
		#end
			;
	}

	/**
		Tells if an object has a field set. This doesn't take into account the object prototype (class methods).
	**/
	public static function hasField( o : Dynamic, field : String ) : Bool {
		untyped{
		#if flash
			return this.hasOwnProperty.call(o,field);
		#else js
			if( o.hasOwnProperty != null )
				return o.hasOwnProperty(field);
			var arr = fields(o);
			for( t in arr.iterator() )
				if( t == field ) return true;
			return false;
		#else neko
			return __dollar__typeof(o) == __dollar__tobject && __dollar__objfield(o,__dollar__hash(field.__s));
		#else error
		#end
		}
	}

	/**
		Returns the field of an object, or null if [o] is not an object or doesn't have this field.
	**/
	public static function field( o : Dynamic, field : String ) : Dynamic {
		untyped
		#if flash
			return o[field]
		#else js
			try {
				return o[field];
			} catch( e : Dynamic ) {
				return null;
			}
		#else neko
			{
				if( __dollar__typeof(o) != __dollar__tobject )
					return null;
				var fh = __dollar__hash(field.__s);
				return __dollar__objget(o,fh);
			}
		#else error
		#end
			;
	}

	/**
		Set an object field value.
	**/
	public static function setField( o : Dynamic, field : String, value : Dynamic ) : Void {
		untyped
		#if flash
			o[field] = value;
		#else js
			o[field] = value;
		#else neko
			if( __dollar__typeof(o) == __dollar__tobject )
				__dollar__objset(o,__dollar__hash(field.__s),value);
		#else error
		#end
	}

	/**
		Call a method with the given object and arguments.
	**/
	public static function callMethod( o : Dynamic, func : Dynamic, args : Array<Dynamic> ) : Dynamic {
		return untyped
		#if flash
			func.apply(o,args)
		#else js
			func.apply(o,args)
		#else neko
			__dollar__call(func,o,args.__a)
		#else error
		#end
			;
	}

	/**
		Returns the list of fields of an object, excluding its prototype (class methods).
	**/
	public static function fields( o : Dynamic ) : Array<String> {
		if( o == null ) return new Array();
		return untyped {
		#if flash
			var a = __keys__(o);
			var i = 0;
			while( i < a.length ) {
				if( !this.hasOwnProperty.call(o,a[i]) )
					a.splice(i,1);
				else
					++i;
			}
			a;
		#else js
			var a = new Array();
			var t;
			try{ t = o.__proto__; }catch(e : Dynamic){ t = null; }
			if( t != null ) o.__proto__ = null;
			untyped __js__("
				for(var i in o)
					a.push(i);
			");
			if( t != null ) o.__proto__ = t;
			var i = 0;
			if( o.hasOwnProperty != null )
				while( i < a.length ) {
					if( !o.hasOwnProperty(a[i]) )
						a.splice(i,1);
					else
						++i;
				}
			a;
		#else neko
			if( __dollar__typeof(o) != __dollar__tobject )
				new Array<String>();
			else {
				var a = __dollar__objfields(o);
				var i = 0, j = 0;
				var l = __dollar__asize(a);
				var t__string = __dollar__hash("__string".__s);
				while( i < l ) {
					var x = a[i];
					if( x != t__string ) {
						a[j] = new String(__dollar__field(x));
						j++;
					}
					i++;
				}
				i = j;
				while( i < l ) {
					a[i] = null;
					i++;
				}
				Array.new1(a,j);
			}
		#else error
		#end
		}
	}

	/**
		Returns the class of an object
	**/
	public static function getClass( o : Dynamic ) : Class {
		return untyped
		#if flash
			o.__class__;
		#else js
			if( o == null )
				null;
			else
				o.__class__;
		#else neko
			if( __dollar__typeof(o) != __dollar__tobject )
				null;
			else
				o.__class__;
		#else error
		#end
	}

	/**
		Tells if a value is a function or not.
	**/
	public static function isFunction( f : Dynamic ) : Bool {
		return untyped
		#if flash
			f.call == _global["Function"].call && f.__interfaces__ == null
		#else js
			f != null && f.call == isFunction.call && f.__interfaces__ == null
		#else neko
			__dollar__typeof(f) == __dollar__tfunction
		#else error
		#end
			;
	}

	/**
		Delete an object field.
	**/
	public static function deleteField( o : Dynamic, f : String ) : Bool {
		#if flash
			untyped {
				if( this.hasOwnProperty.call(o,f) == null ) return false;
				__delete__(o,f);
				return true;
			}
		#else js
			untyped {
				if( !hasField(o,f) ) return false;
				__js__("delete")(o[f]);
				return true;
			}
		#else neko
			return untyped __dollar__objremove(o,f.__s)
		#else error
		#end
			;
	}

}
