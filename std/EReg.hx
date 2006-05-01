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

class EReg {

	var r : Void;
	#if neko
	var last : String;
	var global : Bool;
	#end

	public function new( r : String, opt : String ) {
		#if neko
		var a = opt.split("g");
		global = a.length > 1;
		if( global )
			opt = a.join("");
		this.r = regexp_new_options(untyped r.__s, untyped opt.__s);
		#else js
		this.r = untyped __new__("RegExp",r,opt);
		#else flash
		throw "EReg::new not implemented";
		#else error
		#end
	}

	public function match( s : String ) : Bool {
		#if neko
		var p = regexp_match(r,untyped s.__s,0,s.length);
		if( p )
			last = s;
		else
			last = null;
		return p;
		#else js
		untyped {
			r.m = r.exec(s);
			r.l = RegExp.leftContext;
			r.r = RegExp.rightContext;
			return (r.m != null);
		}
		#else flash
		throw "EReg::match not implemented";
		return false;
		#else error
		#end
	}

	public function matched( n : Int ) : String {
		#if neko
		return new String(regexp_matched(r,n));
		#else js
		return untyped if( r.m != null && n >= 0 && n < r.m.length ) r.m[n] else throw "EReg::matched";
		#else flash
		throw "EReg::matched not implemented";
		return "";
		#else error
		#end
	}

	public function matchedLeft() : String {
		#if neko
		var p = regexp_matched_pos(r,0);
		return new String(last.substr(0,p.pos));
		#else js
		if( untyped r.m == null ) throw "EReg::matchedLeft";
		return untyped r.l;
		#else flash
		throw "EReg::matchedLeft not implemented";
		return null;
		#else error
		#end
	}

	public function matchedRight() : String {
		#if neko
		var p = regexp_matched_pos(r,0);
		var sz = p.pos+p.len;
		return new String(last.substr(sz,last.length-sz));
		#else js
		if( untyped r.m == null ) throw "EReg::matchedRight";
		return untyped r.r;
		#else flash
		throw "EReg::matchedRight not implemented";
		return null;
		#else error
		#end
	}

	public function matchedPos() : { pos : Int, len : Int } {
		#if neko
		return regexp_matched_pos(r,0);
		#else js
		if( untyped r.m == null ) throw "EReg::matchedPos";
		return untyped { pos : r.m.index, len : r.m[0].length };
		#else flash
		throw "EReg::matchedPos not implemented";
		return null;
		#else error
		#end
	}

	public function split( s : String ) : Array<String> {
		#if neko
		var pos = 0;
		var len = s.length;
		var a = new Array();
		var first = true;
		do {
			if( !regexp_match(r,untyped s.__s,pos,len) )
				break;
			var p = regexp_matched_pos(r,0);
			if( p.len == 0 && !first ) {
				if( len == 0 )
					break;
				p.pos += 1;
			}
			a.push(s.substr(pos,p.pos - pos));
			var tot = p.pos + p.len - pos;
			pos += tot;
			len -= tot;
			first = false;
		} while( global );
		a.push(s.substr(pos,len));
		return a;
		#else js
		var d = "#__delim__#";
		return untyped s.replace(r,d).split(d);
		#else flash
		throw "EReg::split not implemented";
		return null;
		#else error
		#end
	}

	public function replace( s : String, by : String ) : String {
		#if neko
		var b = new StringBuf();
		var pos = 0;
		var len = s.length;
		var a = by.split("$");
		var first = true;
		do {
			if( !regexp_match(r,untyped s.__s,pos,len) )
				break;
			var p = regexp_matched_pos(r,0);
			if( p.len == 0 && !first ) {
				if( len == 0 )
					break;
				p.pos += 1;
			}
			b.addSub(s,pos,p.pos-pos);
			b.add(a[0]);
			for( i in 1...a.length ) {
				var k = a[i];
				var c = k.charCodeAt(0);
				// 1...9
				if( c >= 49 && c <= 57 ) {
					var p = regexp_matched_pos(r,c-48);
					b.addSub(s,p.pos,p.len);
					b.addSub(k,1,k.length - 1);
				} else if( c == null ) {
					b.add("$");
					i += 1;
				} else
					b.add("$"+k);
			}
			var tot = p.pos + p.len - pos;
			pos += tot;
			len -= tot;
			first = false;
		} while( global );
		b.addSub(s,pos,len);
		return b.toString();
		#else js
		return untyped s.replace(r,by);
		#else flash
		throw "EReg::replace not implemented";
		return null;
		#else error
		#end
	}

#if neko
	static var regexp_new_options = neko.Lib.load("regexp","regexp_new_options",2);
	static var regexp_match = neko.Lib.load("regexp","regexp_match",4);
	static var regexp_matched = neko.Lib.load("regexp","regexp_matched",2);
	static var regexp_matched_pos = neko.Lib.load("regexp","regexp_matched_pos",2);
#end

}
