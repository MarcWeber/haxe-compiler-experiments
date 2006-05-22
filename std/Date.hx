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
	The Date class is used for date manipulation. There is some extra functions
	available in the [DateTools] class.
**/
extern class Date
{
	/**
		Creates a new date object.
	**/
	function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void;

	/**
		Returns the timestamp of the date. It's the number of milliseconds
		elapsed since 1st January 1970.
	**/
	function getTime() : Float;

	function getHours() : Int;
	function getMinutes() : Int;
	function getSeconds() : Int;
	function getFullYear() : Int;
	function getMonth() : Int;
	function getDate() : Int;

	/**
		Returns a string representation for the Date, by using the
		standard format [YYYY-MM-DD HH:MM:SS]. See [DateTools.format] for
		other formating rules.
	**/
	function toString():String;

	/**
		Returns a Date representing the current local time.
	**/
	static function now() : Date;

	/**
		Returns a Date from a timestamp [t] which is the number of
		milliseconds elapsed since 1st January 1970.
	**/
	static function fromTime( t : Float ) : Date;

	private static function __init__() : Void untyped {
	#if js
		Date.now = function() {
			return __new__(Date);
		};
		Date.fromTime = function(t){
			var d = __new__(Date);
			d.setTime( t );
			return d;
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
	#else flash
		Date.now = function() {
			return __new__(Date);
		};
		Date.fromTime = function(t){
			var d = __new__(Date);
			d.setTime( t );
			return d;
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
	#else neko
		Date = neko.NekoDate__;
	#else error
	#end
	}

}


