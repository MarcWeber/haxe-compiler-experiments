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

extern class Math
{
	static var PI : Float;
	static var NaN : Float;
	static var NEGATIVE_INFINITY : Float;
	static var POSITIVE_INFINITY : Float;

	static function abs(value:Float):Float;
	static function min(value1:Float,value2:Float):Float;
	static function max(value1:Float,value2:Float):Float;
	static function sin(value:Float):Float;
	static function cos(value:Float):Float;
	static function atan2(value1:Float,value2:Float):Float;
	static function tan(value:Float):Float;
	static function exp(value:Float):Float;
	static function log(value:Float):Float;
	static function sqrt(value:Float):Float;
	static function round(value:Float):Int;
	static function floor(value:Float):Int;
	static function ceil(value:Float):Int;
	static function atan(value:Float):Float;
	static function asin(value:Float):Float;
	static function acos(value:Float):Float;
	static function pow(value1:Float,value2:Float):Float;
	static function random() : Float;

	static function isFinite( f : Float ) : Bool;
	static function isNaN( f : Float ) : Bool;

	private static function __init__() : Void untyped {
	#if neko
		Math = neko.NekoMath__;
	#else true
		NaN = Number["NaN"];
		NEGATIVE_INFINITY = Number["NEGATIVE_INFINITY"];
		POSITIVE_INFINITY = Number["POSITIVE_INFINITY"];
		isFinite = function(i) {
			return
			#if flash
			_global["isFinite"](i);
			#else js
			__js__("isFinite")(i);
			#else error
			#end
		};
		isNaN = function(i) {
			return
			#if flash
			_global["isNaN"](i);
			#else js
			__js__("isNaN")(i);
			#else error
			#end
		};
	#end
	}

}


