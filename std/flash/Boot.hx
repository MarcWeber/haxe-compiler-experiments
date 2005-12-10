class Boot {

	private static function __string_rec(o,s) {
		untyped {
			if( s.length >= 20 )
				return "<...>"; // too much deep recursion
			var t = __typeof__(o);
			if( t == "movieclip" )
				t = "object";
			switch( t ) {
			case "object":
				if( __instanceof__(o,Array) ) {
					var l = o.length;
					var i;
					var str = "[";
					s += "    ";
					for i in 0...l
						str += (if (i > 0) "," else "")+__string_rec(o[i],s);
					s = s.substring(4);
					str += "]";
					return str;
				}
				var s2 = o.toString();
				if( (__typeof__(s2) == "string" || __instanceof__(s2,String)) && s2 != "[object Object]" )
					return s2;
				var k;
				var str = "{\n";
				if( typeof(o) == "movieclip" )
					str = "MC("+o._name+") "+str;
				s += "    ";
				for k in (__keys__(o)).iterator() {
					str += s + k + " : "+__string_rec(o[k],s)+"\n";
				}
				s = s.substring(4);
				str += s + "}";
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

	private static function __closure(f,o) {
		untyped {
			var f2 = function() {
				var me = __arguments__.callee;
				return me.f.apply(me.o,__arguments__);
			};
			f2.f = o[f];
			f2.o = o;
			return f2;
		}
	}

	private static function __instanceof(o,cl) {
		untyped {
			if( __instanceof__(o,cl) )
				return true;
			switch( cl ) {
			case Int:
				return Math.ceil(o) == o; // error with NaN
			case Float:
				return __typeof__(o) == "number";
			case Bool:
				return (o == true || o == false);
			case String:
				return __typeof__(o) == "string";
			default:
				return false;
			}
		}
	}

	private static function __trace(v,inf) {
		untyped {
			var tf = _root.__trace_txt;
			if( tf == null ) {
				_root.createTextField("__trace_txt",1048500,0,0,Stage.width,Stage.height);
				tf = _root.__trace_txt;
				tf.selectable = false;
				_root.__trace_lines = new Array<String>();
			}
			var s = inf.fileName+":"+inf.lineNumber+": "+__string_rec(v,"");
			var lines = _root.__trace_lines.concat(s.split("\n"));
			_root.__trace_lines = lines;
			var nlines = Stage.height / 16;
			if( lines.length > nlines )
				lines.splice(0,lines.length-nlines);
			tf.text = lines.join("\n");
		}
	}

	private static function __clear_trace() {
		untyped {
			_root.__trace_txt.removeTextField();
			_root.__trace_lines = null;
		}
	}

	private static function __init(current) {
		untyped {
			var obj = _global["Object"];
			if( flash == null )
				flash = __new__(obj);
			else if( flash.text == null )
				flash.text = __new__(obj);
			flash.text.StyleSheet = TextField["StyleSheet"];
			flash.system = __new__(obj);
			flash.system.Capabilities = System.capabilities;
			flash.system.Security = System.security;
			Math.pi = Math["PI"];

			#use_ime
			flash.system.IME = System["IME"];
			flash.system.IME._ALPHANUMERIC_FULL = System["IME"]["ALPHANUMERIC_FULL"];
			flash.system.IME._ALPHANUMERIC_HALF = System["IME"]["ALPHANUMERIC_HALF"];
			flash.system.IME._CHINESE = System["IME"]["CHINESE"];
			flash.system.IME._JAPANESE_HIRAGANA = System["IME"]["JAPANESE_HIRAGANA"];
			flash.system.IME._JAPANESE_KATAKANA_FULL = System["IME"]["JAPANESE_KATAKANA_FULL"];
			flash.system.IME._JAPANESE_KATAKANA_HALF = System["IME"]["JAPANESE_KATAKANA_HALF"];
			flash.system.IME._KOREAN = System["IME"]["KOREAN"];
			flash.system.IME._UNKNOWN = System["IME"]["UNKNOWN"];
			#end

			Node = _global["XMLNode"];
			Node.element_node = 1;
			Node.text_node = 3;
			Node.prototype.removeChild = Node.prototype.removeNode;
			Node.prototype.replaceChild = function(cnew,cold) {
				this.insertBefore(cnew,cold);
				this.removeChild(cold);
			};

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

			Stage._global = _global;
			Stage._root = _root;
			Stage.current = current;
			Int = __new__(obj);
			Float = _global["Number"];
			// prevent closure creation by setting untyped
			current["@instanceof"] = untyped __instanceof;
			current["@closure"] = untyped __closure;
		}
	}

}