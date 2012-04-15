package jvm.native.util;
import haxe.Int64;
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
	@:overload(function() : Void { })
	@:overload(function(str : String) : Void { })
	@:overload(function(time : Int64) : Void { })
	function new(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ) : Void;

	/**
		Returns the timestamp of the date. It's the number of milliseconds
		elapsed since 1st January 1970. It might only have a per-second precision
		depending on the platforms.
	**/
	function getTime() : Int64;

	/**
		Returns the hours value of the date (0-23 range).
	**/
	function getHours() : Int;

	/**
		Returns the minutes value of the date (0-59 range).
	**/
	function getMinutes() : Int;

	/**
		Returns the seconds of the date (0-59 range).
	**/
	function getSeconds() : Int;

	/**
		Returns the full year of the date.
	**/
	function getYear() : Int;

	/**
		Returns the month of the date (0-11 range).
	**/
	function getMonth() : Int;

	/**
		Returns the day of the date (1-31 range).
	**/
	function getDate() : Int;

	/**
		Returns the week day of the date (0-6 range).
	**/
	function getDay() : Int;

	/**
		Returns a string representation for the Date, by using the
		standard format [YYYY-MM-DD HH:MM:SS]. See [DateTools.format] for
		other formating rules.
	**/
	function toString():String;
}

