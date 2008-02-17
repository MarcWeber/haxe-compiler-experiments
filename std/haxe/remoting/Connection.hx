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
package haxe.remoting;

class Connection implements Dynamic<Connection> {

	var __data : Dynamic;
	var __path : Array<String>;

	function new( data : Dynamic, path ) {
		__data = data;
		__path = path;
	}

	public function __resolve(field) {
		var s = new Connection(__data,__path.copy());
		s.__path.push(field);
		return s;
	}

	#if flash9
	static function escapeString( s : String ) {
		return s.split("\\").join("\\\\");
	}
	#else flash
	static function escapeString( s : String ) {
		return s.split("\\").join("\\\\").split("&").join("&amp;");
	}
	#end

	public function call( params : Array<Dynamic> ) : Dynamic {
	#if flash
		var p = __path.copy();
		var f = p.pop();
		var path = p.join(".");
		var s = new haxe.Serializer();
		s.serialize(params);
		var params = escapeString(s.toString());
		#if (flash8 || flash9)
			var s = flash.external.ExternalInterface.call("haxe.remoting.Connection.doCall",path,f,params);
			if( s == null )
				throw "Failed to call JS method "+__path.join(".");
			return new haxe.Unserializer(s).unserialize();
		#else true
			throw "JS method call is not supported on Flash < 8";
		#end
	#else js
		var s = new haxe.Serializer();
		var data;
		if( __data.remotingCall == null ) {
			var h = new haxe.Http(__data);
			untyped h.async = false;
			h.onData = function(d) { data = d; };
			h.onError = function(e) { throw e; };
			h.setHeader("X-Haxe-Remoting","1");
			s.serialize(__path);
			s.serialize(params);
			h.setParameter("__x",s.toString());
			h.request(true);
			if( data.substr(0,3) != "hxr" )
                throw "Invalid response : '"+data+"'";
			data = data.substr(3);
		} else {
			var p = __path.copy();
			var f = p.pop();
			var path = p.join(".");
			s.serialize(params);
			var params = s.toString();
			data = __data.remotingCall(path,f,params);
			if( data == null )
				throw "Failed to call Flash method "+__path.join(".");
		}
		return new haxe.Unserializer(data).unserialize();
	#else neko
		var cnx = AsyncConnection.urlConnect(__data);
		var result = null;
		untyped cnx.__path = __path;
		cnx.onError = neko.Lib.rethrow;
		cnx.call(params,function(d) { result = d; });
		return result;
	#else error
	#end
	}


	static function doCall( path : String, f : String, params : String ) : String {
		try {
			var params = new haxe.Unserializer(params).unserialize();
			#if flash
			var obj = flash.Lib.eval(path);
			#else js
			var obj = js.Lib.eval(path);
			#else true
			var obj = null;
			#end
			var fun = Reflect.field(obj,f);
			if( fun == null )
				throw "Invalid remoting call : "+path+"."+f;
			var v = Reflect.callMethod(obj,fun,params);
			var s = new haxe.Serializer();
			s.serialize(v);
			#if flash
			return escapeString(s.toString());
			#else js
			return s.toString()+"#";
			#else true
			return null;
			#end
		} catch( e : Dynamic ) {
			var s = new haxe.Serializer();
			s.serializeException(e);
			return s.toString();
		}
	}

	// ---- platform-specific ----

	#if flash

	#if no_js_remoting
	public static function init() {
	#else true
	static function __init__() {
	#end
		#if flash9
		var _ = function() { try flash.external.ExternalInterface.addCallback("remotingCall",doCall) catch( e : Dynamic ) {} }();
		#else flash8
		flash.external.ExternalInterface.addCallback("remotingCall",null,doCall);
		#end
	}

	public static function jsConnect( ?name : String ) : Connection {
		if( !flash.external.ExternalInterface.available )
			throw "External Interface not available";
		if( flash.external.ExternalInterface.call("haxe.remoting.Connection.jsRemoting") != "yes" )
			throw "haxe.remoting.Connection is not available in JavaScript";
		return new Connection(null,if( name == null ) [] else ["_cnx",name]);
	}

	#else js

	static function __init__() {
		untyped __js__("_cnx = new Object()");
	}

	static function jsRemoting() {
		return "yes";
	}

	public static function bind( name : String, obj : Dynamic ) {
		untyped __js__("_cnx")[name] = obj;
	}

	public static function flashConnect( objId : String ) : Connection {
		var x : Dynamic = untyped window.document[objId];
		if( x == null )
			throw "Could not find flash object '"+objId+"'";
		if( x.remotingCall == null ) throw "The flash object is not ready or does not contain haxe.remoting.Connection";
		return new Connection(x,[]);
	}

	public static function urlConnect( url : String ) : Connection {
		return new Connection(url,[]);
	}

	#else neko

	public static function urlConnect( url : String ) : Connection {
		return new Connection(url,[]);
	}

	#end

}
