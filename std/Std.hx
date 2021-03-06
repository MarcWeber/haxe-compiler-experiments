/*
 * Copyright (C)2005-2012 Haxe Foundation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
#if !(core_api || cross)
#error "Please don't add haxe/std to your classpath, instead set HAXE_LIBRARY_PATH env var"
#end

/**
	The Std class provides standard methods for manipulating basic types.
**/
extern class Std {

	/**
		Tells if a value v is of the type t. Returns false if v or t are null.
	**/
	public static function is( v : Dynamic, t : Dynamic ) : Bool;

	/**
		Converts any value to a String.

		If s is of String, Int, Float or Bool, its value is returned.

		If s is an instance of a class and that class or one of its parent classes has
		a toString() method, that method is called. If no such method is present, the result
		is unspecified.

		If s is an enum constructor without argument, the constructor's name is returned. If
		arguments exists, the constructor's name followed by the String representations of
		the arguments is returned.

		If s is a structure, the field names along with their values are returned. The field order
		and the operator separating field names and values are unspecified.

		If s is null, "null" is returned.
	**/
	public static function string( s : Dynamic ) : String;

	/**
		Converts a Float to an Int, rounded down.

		If x is NaN, NEGATIVE_INFINITY or POSITIVE_INFINITY, the result is unspecified.
	**/
	public static function int( x : Float ) : Int;

	/**
		Converts a String to an Int.

		Leading whitespaces are ignored.

		If x starts with 0x or 0X, hexadecimal notation is recognized where the following digits may
		contain 0-9 and A-F.

		Otherwise x is read as decimal number with 0-9 being allowed characters. x may also start with
		a - to denote a negative value.

		In decimal mode, parsing continues until an invalid character is detected, in which case the
		result up to that point is returned. For hexadecimal notation, the effect of invalid characters
		is unspecified.

		Leading 0s that are not part of the 0x/0X hexadecimal notation are ignored, which means octal
		notation is not supported.

		If the input cannot be recognized, the result is null.
	**/
	public static function parseInt( x : String ) : Null<Int>;

	/**
		Converts a String to a Float.

		The parsing rules for parseInt() apply here as well, with the exception of invalid input
		resulting in a NaN value instead of null.

		Additionally, decimal notation may contain a single . to denote the start of the fractions.
	**/
	public static function parseFloat( x : String ) : Float;

	/**
		Return a random integer between 0 included and x excluded.

		If x is <= 1, the result is always 0.
	**/
	public static function random( x : Int ) : Int;
}
