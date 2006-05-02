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

class AsyncConnection implements Dynamic<AsyncConnection> {

	var __data : Dynamic;
	var __path : Array<String>;

	function new( data, path ) {
		__data = data;
		__path = path;
	}

	function __resolve(field) {
		var s = new AsyncConnection(__data,__path.copy());
		s.onError = onError;
		s.__path.push(field);
		return s;
	}

	public function onError( err : Dynamic ) {
	}

	public function eval( onData : Dynamic -> Void ) : Void {
		var h = new Http(__data);
		var me = this;
		h.setHeader("X-Haxe-Remoting","eval");
		h.setParameter("__x",__path.join("."));
		h.onData = function(data : String) {
			var ok = true;
			var v;
			try {
				if( data.length < 3 || data.substr(0,3) != "hxr" )
					throw "Invalid response : '"+data+"'";
				var s = new Unserializer(data.substr(3,data.length-3));
				v = s.unserialize();
			} catch( err : Dynamic ) {
				ok = false;
				me.onError(err);
			}
			if( ok )
				onData(v);
		};
		h.onError = onError;
		h.request(true);
	}

	public function call( params : Array<Dynamic>, onData : Dynamic -> Void ) : Void {
		var h = new Http(__data);
		var me = this;
		var s = new Serializer();
		s.serialize(__path);
		s.serialize(params);
		h.setHeader("X-Haxe-Remoting","call");
		h.setParameter("__x",s.toString());
		h.onData = function(data : String) {
			var ok = true;
			var v;
			try {
				if( data.length < 3 || data.substr(0,3) != "hxr" )
					throw "Invalid response : '"+data+"'";
				var s = new Unserializer(data.substr(3,data.length-3));
				v = s.unserialize();
			} catch( err : Dynamic ) {
				ok = false;
				me.onError(err);
			}
			if( ok )
				onData(v);
		};
		h.onError = onError;
		h.request(true);
	}

	public static function urlConnect( url : String ) {
		return new AsyncConnection(url,[]);
	}

}
