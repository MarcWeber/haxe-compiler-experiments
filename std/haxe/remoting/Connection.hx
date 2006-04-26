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

class Connection implements Dynamic<Connection> {

	var __data : Dynamic;
	var __path : Array<String>;

	function new( data, path ) {
		__data = data;
		__path = path;
	}

	function __resolve(field) {
		var s = new Connection(__data,__path.copy());
		s.__path.push(field);
		return s;
	}

	public function eval() : Dynamic {
	#if flash
		return jsEval(__path.join("."));
	#else js
		var s = __data.asEval(__path.join("."));
		return new Unserializer(s).unserialize();
	#else neko
		throw "Connection::eval is not implemented";
		return null;
	#else error
	#end
	}


	// ---- platform-specific ----

	#if flash

	static function __init__() {
		flash.external.ExternalInterface.addCallback("asEval",null,asEval);
	}

	static function asEval( s : String ) : String {
		var v = flash.Lib.eval(s);
		var s = new Serializer();
		s.serialize(v);
		return s.toString();
	}

	static function jsEval( s : String ) : Dynamic {
		var s = flash.external.ExternalInterface.call("haxe.Connection.jsEval",s);
		if( s == null )
			return null;
		return new Unserializer(s).unserialize();
	}

	public static function jsConnect() : Connection {
		if( !flash.external.ExternalInterface.available )
			throw "External Interface not available";
		if( jsEval("0") != 0 )
			throw "haxe.Connection is not available in JavaScript";
		return new Connection(null,[]);
	}

	#else js

	static function jsEval( s : String ) : String {
		var v;
		var e = false;
		try {
			v = js.Lib.eval(s);
		} catch( exc : Dynamic ) {
			v = exc;
			e = true;
		}
		try {
			var s = new Serializer();
			if( e )
				s.serializeException(v);
			else
				s.serialize(v);
			var r = s.toString();
			return r;
		} catch( e : Dynamic ) {
			js.Lib.alert(e);
			return null;
		}
	}

	public static function flashConnect( objId : String ) : Connection {
		var x : Dynamic = untyped window.document[objId];
		if( x == null )
			throw "Could not find flash object '"+objId+"'";
		if( x.asEval == null ) throw "The flash object is not ready or does not contain haxe.Connection";
		return new Connection(x,[]);
	}

	#end

}
