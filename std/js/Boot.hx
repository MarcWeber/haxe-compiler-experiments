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
package js;

class Boot {

	private static function __unhtml(s : String) {
		return s.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
	}

	private static function __trace(v,i) {
		untyped {
			var msg = i.fileName+":"+i.lineNumber+": "+__unhtml(__string_rec(v,""))+"<br/>";
			var d = document.getElementById("haxe:trace");
			if( d == null )
				alert("No haxe:trace element defined\n"+msg);
			else
				d.innerHTML += msg;
		}
	}

	private static function __clear_trace() {
		untyped {
			var d = document.getElementById("haxe:trace");
			if( d != null )
				d.innerHTML = "";
		}
	}

	private static function __closure(o,f) {
		untyped {
			var m = o[f];
			if( m == null )
				return null;
			return function() { return m.apply(o,arguments); };
		}
	}

	private static function __string_rec(o,s) {
		untyped {
			if( o == null )
			    return "null";
			if( s.length >= 5 )
				return "<...>"; // too much deep recursion
			var t = __js__("typeof(o)");
			if( t == "function" && o.__interfaces__ != null )
				t = "object";
			switch( t ) {
			case "object":
				if( __js__("o instanceof Array") ) {
					if( o.__enum__ != null ) {
						if( o.length == 1 )
							return o[0];
						var str = o[0]+"(";
						s += "    ";
						for( i in 1...o.length ) {
							if( i != 1 )
								str += "," + __string_rec(o[i],s);
							else
								str += __string_rec(o[i],s);
						}
						return str + ")";
					}
					var l = o.length;
					var i;
					var str = "[";
					s += "    ";
					for( i in 0...l )
						str += (if (i > 0) "," else "")+__string_rec(o[i],s);
					s = s.substring(4);
					str += "]";
					return str;
				}
				var tostr;
				try {
					tostr = untyped o.toString;
				} catch( e : Dynamic ) {
					// strange error on IE
					return "???";
				}
				if( tostr != null && tostr != __js__("Object.toString") ) {
					var s2 = o.toString();
					if( s2 != "[object Object]")
						return s2;
				}
				var k;
				var str = "{\n";
				s += "\t";
				__js__("for( var k in o ) { ");
					if( str.length != 2 )
						str += ", \n";
					if( k == "__class__" && o[k].__interfaces__ != null )
						__js__("continue");
					if( k == "__construct__" && __js__("typeof(o[k])") == "function" )
						str += s + k + " : <function>";
					else
						str += s + k + " : "+__string_rec(o[k],s);
				__js__("}");
				s = s.substring(1);
				str += "\n" + s + "}";
				return str;
			case "function":
				return "<function>";
			case "string":
				return o;
			default:
				return String(o);
			}
		}
	}

	private static function __instanceof(o,cl) {
		untyped {
			try {
				if( __js__("o instanceof cl") )
					return true;
			} catch( e : Dynamic ) {
			}
			var c = o.__class__;
			while( c != null ) {
				if( c == cl )
					return true;
				var il = c.__interfaces__;
				var i = 0;
				while( i < il.length ) {
					if( il[i] == cl )
						return true;
					i++;
				}
				c = c.__super__;
			}
			switch( cl ) {
			case Int:
				return (Math.ceil(o) === o) && isFinite(o);
			case Float:
				return __js__("typeof(o)") == "number";
			case Bool:
				return (o === true || o === false);
			case String:
				return __js__("typeof(o)") == "string";
			case Dynamic:
				return true;
			default:
				return false;
			}
		}
	}

	private static function __init() {
		untyped {
			Lib.isIE = (document.all != null);
			Array.prototype.copy = Array.prototype.slice;
			Array.prototype.insert = function(i,x) {
				this.splice(i,0,x);
			};
			Array.prototype.remove = function(obj) {
				var i = 0;
				var l = this.length;
				while( i < l ) {
					if( this[i] == obj ) {
						this.splice(i,1);
						return true;
					}
					i++;
				}
				return false;
			}
			Array.prototype.iterator = function() {
				return {
					cur : 0,
					max : this.length,
					arr : this,
					hasNext : function() {
						return this.cur < this.max;
					},
					next : function() {
						return this.arr[this.cur++];
					}
				}
			};
			Array.prototype.indexes = function() {
				return {
					cur : 0,
					max : this.length,
					hasNext : function() {
						return this.cur < this.max;
					},
					next : function() {
						return this.cur++;
					}
				}
			};
			Date.now = function() {
				return __new__(Date);
			};
			Date.prototype.toString = function() {
				var m = this.getMonth() + 1;
				var d = this.getDate();
				var h = this.getHours();
				var mi = this.getMinutes();
				var s = this.getSeconds();
				if( d < 10 )
					d = "0" + d;
				if( m < 10 )
					m = "0" + m;
				if( h < 10 )
					h = "0" + h;
				if( mi < 10 )
					mi = "0" + mi;
				if( s < 10 )
					s = "0" + s;
				return this.getFullYear()+"-"+m+"-"+d+" "+h+":"+mi+":"+s;
			};
			var oldsub = String.prototype.substr;
			String.prototype.substr = function(pos,len){
				if( pos < 0 ){
					pos = this.length + pos;
					if( pos < 0 ) pos = 0;
				}else if( len < 0 ){
					len = this.length + len - pos;
				}
				return oldsub.apply(this,[pos,len]);
			};
			Int = __new__("Object");
			Float = __js__("Number");
			Bool["true"] = true;
			Bool["false"] = false;
			__js__("$closure = js.Boot.__closure");
		}
	}

}
