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

class Reflect {

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

	public static function createInstance( cl : Dynamic, args : Array<Dynamic> ) : Dynamic {
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
				throw "Too many arguments";
			}
		#else neko
			__dollar__call(__dollar__objget(cl,__dollar__hash("new".__s)),cl,args.__a)
		#else js
			__new__(cl,args[0],args[1],args[2],args[3],args[4])
		#else error
		#end
			;
	}

	public static function hasField( o : Dynamic, field : String ) : Bool {
		return untyped
		#if flash
			this.hasOwnProperty.call(o,field)
		#else js
			o.hasOwnProperty(field)
		#else neko
			__dollar__typeof(o) == __dollar__tobject && __dollar__objfield(o,__dollar__hash(field.__s))
		#else error
		#end
			;
	}

	public static function field( o : Dynamic, field : String ) : Dynamic {
		untyped
		#if flash
			{
				var f = o[field];
				if( f == null && !this.hasOwnProperty.call(o,f) )
					throw ("No such field : " + field);
				return f;
			}
		#else js
			{
				var f = o[field];
				if( f == null && !o.hasOwnProperty(f) )
					throw ("No such field : " + field);
				return f;
			}
		#else neko
			{
				if( __dollar__typeof(o) != __dollar__tobject )
					throw ("No such field : " + field);
				var fh = __dollar__hash(field.__s);
				var f = __dollar__objget(o,fh);
				if( f == null && !__dollar__objfield(o,fh) )
					throw ("No such field : " + field);
				return f;
			}
		#else error
		#end
			;
	}

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

	public static function fields( o : Dynamic ) : Array<String> {
		return untyped
		#if flash
			__keys__(o)
		#else js
			js.Boot.__keys(o)
		#else neko
			if( __dollar__typeof(o) != __dollar__tobject )
				new Array<String>();
			else {
				var a = __dollar__objfields(o);
				var i = 0;
				var l = __dollar__asize(a);
				while( i < l ) {
					a[i] = new String(__dollar__field(a[i]));
					i++;
				}
				Array.new1(a,l);
			}
		#else error
		#end
			;
	}

	public static function isFunction( f : Dynamic ) : Bool {
		return untyped
		#if flash
			f.call == _global["Function"].call
		#else js
			f.call == isFunction.call
		#else neko
			__dollar__typeof(f) == __dollar__tfunction
		#else error
		#end
			;
	}

	public static function deleteField( o : Dynamic, f : String ) {
		#if flash
			untyped __delete__(o,f)
		#else js
			untyped delete(o[f])
		#else neko
			untyped __dollar__objremove(o,f.__s)
		#else error
		#end
			;
	}

}
