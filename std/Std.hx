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
		#else neko
		neko.Boot.__instanceof(v,t);
		#else js
		js.Boot.__instanceof(v,t);
		#else error
		#end
	}

	/**
		Convert any value to a String
	**/
	public static function string( s : Dynamic ) : String {
		return untyped
		#if flash
		flash.Boot.__string_rec(s,"");
		#else neko
		new String(__dollar__string(s));
		#else js
		js.Boot.__string_rec(s,"");
		#else error
		#end
	}

	/**
		Convert a Float to an Int, rounded down.
	**/
	public static function int( x : Float ) : Int {
		return Math.floor(x);
	}

	/**
		Convert any value to a Bool. Only 0, null and false are false, other values are true.
	**/
	public static function bool( x : Dynamic ) : Bool {
		return x != 0 && x != null && x != false;
	}

	/**
		Convert a String to an Int, parsing different possible representations
	**/
	public static function parseInt( x : String ) : Int {
		return untyped
		#if flash
		_global.parseInt(x);
		#else neko
		__dollar__int(x.__s);
		#else js
		__js__("parseInt")(x);
		#else error
		#end
	}

	/**
		Convert a String to a Float, parsing different possible reprensations.
	**/
	public static function parseFloat( x : String ) : Float {
		return untyped
		#if flash
		_global.parseFloat(x);
		#else neko
		__dollar__float(x.__s);
		#else js
		__js__("parseFloat")(x);
		#else error
		#end
	}

	/**
		Convert a character code into the corresponding single-char String.
	**/
	public static function chr( x : Int ) : String {
		return untyped
		#if flash
		String.fromCharCode(x);
		#else neko
		{
			var s = __dollar__smake(1);
			__dollar__sset(s,0,x);
			new String(s);
		}
		#else js
		String.fromCharCode(x);
		#else error
		#end
	}

	/**
		Return the character code of the first character of the String, or null if the String is empty.
	**/
	public static function ord( x : String ) : Int {
		#if flash
		if( x == "" )
			return null;
		else
			return x.charCodeAt(0);
		#else neko
		untyped {
			var s = __dollar__ssize(x.__s);
			if( s == 0 )
				return null;
			else
				return __dollar__sget(x.__s,0);
		}
		#else js
		if( x == "" )
			return null;
		else
			return x.charCodeAt(0);
		#else error
		#end
	}

	/**
		Return a random integer between 0 included and x excluded.
	**/
	public static function random( x : Int ) : Int {
		return untyped
		#if flash
		__random__(x);
		#else neko
		Math._rand_int(Math.__rnd,x);
		#else js
		Math.floor(Math.random()*x);
		#else error
		#end
	}

	/**
		Return the given resource stored using -res, or null if not defined.
	**/
	public static function resource( name : String ) : String {
		return untyped
		#if flash
		flash.Boot.__res[name];
		#else neko
		__dollar__objget(neko.Boot.__res,__dollar__hash(name.__s));
		#else js
		js.Boot.__res[name];
		#else error
		#end
	}

}
