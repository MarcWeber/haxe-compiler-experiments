package cs.native;
import haxe.Int64;

/**
 * ...
 * @author waneck
 */

extern class DateTime 
{

	@:overload(function(year : Int, month : Int, day : Int, hour : Int, min : Int, sec : Int ):Void {})
	function new(ticks:Int64):Void;
	
	var Day(default, null):Int;
	var DayOfWeek(default, null):DayOfWeek;
	var DayOfYear(default, null):Int;
	var Hour(default, null):Int;
	var Millisecond(default, null):Int;
	var Minute(default, null):Int;
	var Second(default, null):Int;
	var Year(default, null):Int;
	var Month(default, null):Int;
	var Ticks(default, null):Int64;
	static var Now(default, null):DateTime;
	static var UtcNow(default, null):DateTime;
}


extern enum DayOfWeek
{
	Sunday;
	Monday;
	Tuesday;
	Wedsneday;
	Thursday;
	Friday;
	Saturday;
}