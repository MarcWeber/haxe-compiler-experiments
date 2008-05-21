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

/**
	The Std class provides standard methods for manipulating basic types.
**/
class Std {

	/**
		Tells if a value v is of the type t.
	**/
	public static function is( v : Dynamic, t : Dynamic ) : Bool {
		return untyped
		#if flash
		flash.Boot.__instanceof(v,t);
		#elseif neko
		neko.Boot.__instanceof(v,t);
		#elseif js
		js.Boot.__instanceof(v,t);
		#else
		false;
		#end
	}

	/**
		Convert any value to a String
	**/
	public static function string( s : Dynamic ) : String {
		return untyped
		#if flash
		flash.Boot.__string_rec(s,"");
		#elseif neko
		new String(__dollar__string(s));
		#elseif js
		js.Boot.__string_rec(s,"");
		#else
		"";
		#end
	}

	/**
		Convert a Float to an Int, rounded down.
	**/
	public #if flash9 inline #end static function int( x : Float ) : Int {
		#if flash9
		return untyped __int__(x);
		#else
		if( x < 0 ) return Math.ceil(x);
		return Math.floor(x);
		#end
	}

	/**
		Convert a String to an Int, parsing different possible representations. Returns [null] if could not be parsed.
	**/
	public static function parseInt( x : String ) : Null<Int> {
		untyped {
		#if flash9
		var v = __global__["parseInt"](x);
		if( __global__["isNaN"](v) )
			return null;
		return v;
		#elseif flash
		var v = _global["parseInt"](x);
		if( Math.isNaN(v) )
			return null;
		return v;
		#elseif neko
		var t = __dollar__typeof(x);
		if( t == __dollar__tint )
			return x;
		if( t == __dollar__tfloat )
			return __dollar__int(x);
		if( t != __dollar__tobject )
			return null;
		return __dollar__int(x.__s);
		#elseif js
		var v = __js__("parseInt")(x);
		if( Math.isNaN(v) )
			return null;
		return v;
		#else
		return 0;
		#end
		}
	}

	/**
		Convert a String to a Float, parsing different possible reprensations.
	**/
	public static function parseFloat( x : String ) : Float {
		return untyped
		#if flash9
		__global__["parseFloat"](x);
		#elseif flash
		_global["parseFloat"](x);
		#elseif neko
		__dollar__float(x.__s);
		#elseif js
		__js__("parseFloat")(x);
		#else
		0;
		#end
	}

	/**
		Convert a character code into the corresponding single-char String.
	**/
	public static function chr( x : Int ) : String {
		return String.fromCharCode(x);
	}

	/**
		Return the character code of the first character of the String, or null if the String is empty.
	**/
	public static function ord( x : String ) : Null<Int> {
		#if flash
		if( x == "" )
			return null;
		else
			return x.charCodeAt(0);
		#elseif neko
		untyped {
			var s = __dollar__ssize(x.__s);
			if( s == 0 )
				return null;
			else
				return __dollar__sget(x.__s,0);
		}
		#elseif js
		if( x == "" )
			return null;
		else
			return x.charCodeAt(0);
		#else
		return null;
		#end
	}

	/**
		Return a random integer between 0 included and x excluded.
	**/
	public static function random( x : Int ) : Int {
		return untyped
		#if flash9
		Math.floor(Math.random()*x);
		#elseif flash
		__random__(x);
		#elseif neko
		Math._rand_int(Math.__rnd,x);
		#elseif js
		Math.floor(Math.random()*x);
		#else
		0;
		#end
	}

	/**
		Return the given resource stored using -res, or null if not defined.
	**/
	public static function resource( name : String ) : String {
		return untyped
		#if as3gen
		throw "Not supported in AS3";
		#elseif flash
		flash.Boot.__res[name];
		#elseif neko
		__dollar__objget(neko.Boot.__res,__dollar__hash(name.__s));
		#elseif js
		js.Boot.__res[name];
		#else
		null;
		#end
	}

}
