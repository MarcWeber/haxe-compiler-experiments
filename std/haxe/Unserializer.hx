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
package haxe;
import Type.Class;
import Type.Enum;

typedef TypeResolver = {
	function resolveClass( name : String ) : Class;
	function resolveEnum( name : String ) : Enum;
}

class Unserializer {

	public static var DEFAULT_RESOLVER : TypeResolver = Type;

 	var buf : String;
 	var pos : Int;
 	var length : Int;
 	var cache : Array<Dynamic>;
 	var scache : Array<String>;
 	var resolver : TypeResolver;
 	#if neko
 	var upos : Int;
 	#end

 	public function new( buf : String ) {
 		this.buf = buf;
 		length = buf.length;
 		pos = 0;
 		#if neko
 		upos = 0;
 		#end
 		scache = new Array();
 		cache = new Array();
 		setResolver(DEFAULT_RESOLVER);
 	}

 	public function setResolver( r ) {
		if( r == null )
			resolver = {
				resolveClass : function(_) { return null; },
				resolveEnum : function(_) { return null; }
			};
		else
			resolver = r;
	}

 	function readDigits() {
 		var k = 0;
 		var s = false;
 		var fpos = pos;
 		while( true ) {
 			var c = buf.charCodeAt(pos);
 			if( c == null )
 				break;
 			if( c == 45 ) { // negative sign
 				if( pos != fpos )
 					break;
 				s = true;
 				pos++;
 				continue;
 			}
			c -= 48;
 			if( c < 0 || c > 9 )
 				break;
 			k = k * 10 + c;
 			pos++;
 		}
 		if( s )
 			k *= -1;
 		return k;
 	}

	function unserializeObject(o) {
 		while( true ) {
 			if( pos >= length )
 				throw "Invalid object";
 			if( buf.charCodeAt(pos) == 103 ) /*g*/
 				break;
 			var k = unserialize();
 			if( !Std.is(k,String) )
 				throw "Invalid object key";
 			var v = unserialize();
 			Reflect.setField(o,k,v);
 		}
 		pos++;
	}

 	public function unserialize() : Dynamic {
 		switch( buf.charCodeAt(pos++) ) {
 		case 110: // n
 			return null;
 		case 116: // t
 			return true;
 		case 102: // f
 			return false;
 		case 122: // z
 			return 0;
 		case 105: // i
 			return readDigits();
 		case 100: // d
 			var p1 = pos;
 			while( true ) {
 				var c = buf.charCodeAt(pos);
 				// + - . , 0-9
 				if( (c >= 43 && c < 58) || c == 101 /*e*/ || c == 69 /*E*/ )
 					pos++;
 				else
 					break;
 			}
 			var s = buf.substr(p1,pos-p1);
 			var f = Std.parseFloat(s);
 			if( f == null )
 				throw ("Invalid float "+s);
 			return f;
 		case 107: // k
 			return Math.NaN;
 		case 109: // m
 			return Math.NEGATIVE_INFINITY;
 		case 112: // p
 			return Math.POSITIVE_INFINITY;
 		case 115: // s
 			var len = readDigits();
 			if( buf.charAt(pos++) != ":" || length - pos < len )
				throw "Invalid string length";
			#if neko
			var s = neko.Utf8.sub(buf,pos-upos,len);
			pos += s.length;
			upos += s.length - len;
			#else true
 			var s = buf.substr(pos,len);
 			pos += len;
 			#end
			scache.push(s);
			return s;
 		case 106: // j
 			var len = readDigits();
 			if( buf.charAt(pos++) != ":" )
 				throw "Invalid string length";
 			#if neko
			if( length - pos < len )
				throw "Invalid string length";
 			var s = neko.Utf8.sub(buf,pos-upos,len);
 			pos += s.length;
 			upos += s.length - len;
 			#else true
 			var s = buf.substr(pos,len);
 			pos += len;
 			#end
 			s = s.split("\\r").join("\r").split("\\n").join("\n").split("\\\\").join("\\");
 			scache.push(s);
 			return s;
 		case 97: // a
 			var a = new Array<Dynamic>();
 			cache.push(a);
 			while( true ) {
 				if( pos >= length )
 					throw "Invalid array";
 				var c = buf.charCodeAt(pos);
 				if( c == 104 ) { /*h*/
					pos++;
 					break;
				}
 				if( c == 117 ) { /*u*/
					pos++;
 					var n = readDigits();
 					if( n <= 0 )
 						throw "Invalid array null counter";
 					a[a.length+n-1] = null;
 				} else
 					a.push(unserialize());
 			}
 			return a;
 		case 111: // o
	 		var o = Reflect.empty();
	 		cache.push(o);
			unserializeObject(o);
			return o;
 		case 114: // r
 			var n = readDigits();
 			if( n < 0 || n >= cache.length )
 				throw "Invalid reference";
 			return cache[n];
 		case 82: // R
			var n = readDigits();
			if( n < 0 || n >= scache.length )
				throw "Invalid string reference";
			return scache[n];
 		case 120: // x
			throw unserialize();
		case 99: // c
	 		var o = Reflect.empty();
	 		cache.push(o);
			var a : Array<String> = unserialize();
            if( !Std.is(a,Array) )
				throw "Invalid class name";
			for(s in a)
				if( !Std.is(s,String) )
					throw "Invalid class name";
			var cl = resolver.resolveClass(a.join("."));
			if( cl == null )
				throw "Class not found " + a.join(".");
			unserializeObject(o);
			Type.setClass(o,cl);
			return o;
		case 119: // w
			#if neko
			var e : Dynamic = {
				tag : null,
				__string : function() { return untyped neko.Boot.__enum_str(this); },
				__enum__ : null
			};
			#else true
			var e : Array<Dynamic> = new Array();
			#end
			var cpos = cache.length;
			cache.push(e);
			var a : Array<String> = unserialize();
            if( !Std.is(a,Array) )
				throw "Invalid enum name";
			for(s in a)
				if( !Std.is(s,String) )
					throw "Invalid enum name";
			var edecl = resolver.resolveEnum(a.join("."));
			if( edecl == null )
				throw "Enum not found " + a.join(".");
			untyped e.__enum__ = edecl;
			var tag : String = unserialize();
			if( !Std.is(tag,String) )
				throw "Invalid enum tag";
			#if neko
			e.tag = untyped tag.__s;
			#else true
			e.push(tag);
			#end
			var constr = Reflect.field(edecl,tag);
			if( constr == null )
				throw "Unknown enum tag "+a.join(".")+"."+tag;
			if( buf.charCodeAt(pos++) != 58 ) // ':'
				throw "Invalid enum format";
			var nargs = readDigits();
			if( nargs == 0 ) {
				cache[cpos] = constr;
				return constr;
			}
			#if neko
			var i = 0;
			e.args = untyped __dollar__amake(nargs);
			#end
			while( nargs > 0 ) {
				#if neko
				e.args[i] = unserialize();
				i += 1;
				#else true
				e.push(unserialize());
				#end
				nargs -= 1;
			}
			return e;
 		default:
 		}
 		pos--;
 		throw ("Invalid char "+buf.charAt(pos)+" at position "+pos);
 	}

	/**
		Unserialize a single value and return it.
	**/
	public static function run( v : String ) : Dynamic {
		return new Unserializer(v).unserialize();
	}

}
