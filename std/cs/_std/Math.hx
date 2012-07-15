package;
import system.Random;

@:core_api @:nativegen class Math
{
	public static inline function __init__():Void
	{
		PI = system.Math.PI;
		NaN = untyped __cs__("double.NaN");
		NEGATIVE_INFINITY = untyped __cs__("double.NegativeInfinity");
		POSITIVE_INFINITY = untyped __cs__("double.PositiveInfinity");
		rand = new Random();
		
	}
	
	private static var rand:Random;
	public static var PI(default, null) : Float;
	public static var NaN(default,null) : Float;
	public static var NEGATIVE_INFINITY(default,null) : Float;
	public static var POSITIVE_INFINITY(default,null) : Float;

	public static inline function abs(v:Float):Float
	{
		return system.Math.Abs(v);
	}
	
	public static inline function min(a:Float, b:Float):Float
	{
		return (a < b) ? a : b;
	}
	
	public static inline function max(a:Float, b:Float):Float
	{
		return (a > b) ? a : b;
	}
	
	public static inline function sin(v:Float):Float
	{
		return system.Math.Sin(v);
	}
	
	public static inline function cos(v:Float):Float
	{
		return system.Math.Cos(v);
	}
	
	public static inline function atan2(y:Float, x:Float):Float
	{
		return system.Math.Atan2(y, x);
	}
	
	public static inline function tan(v:Float):Float
	{
		return system.Math.Tan(v);
	}
	
	public static inline function exp(v:Float):Float
	{
		return system.Math.Exp(v);
	}
	
	public static inline function log(v:Float):Float
	{
		return system.Math.Log(v);
	}
	
	public static inline function sqrt(v:Float):Float
	{
		return system.Math.Sqrt(v);
	}
	
	public static inline function round(v:Float):Int
	{
		return Std.int(system.Math.Round(v));
	}
	
	public static inline function floor(v:Float):Int
	{
		return Std.int(system.Math.Floor(v));
	}
	
	public static inline function ceil(v:Float):Int
	{
		return Std.int(system.Math.Ceiling(v));
	}
	
	public static inline function atan(v:Float):Float
	{
		return system.Math.Atan(v);
	}
	
	public static inline function asin(v:Float):Float
	{
		return system.Math.Asin(v);
	}
	
	public static inline function acos(v:Float):Float
	{
		return system.Math.Acos(v);
	}
	
	public static inline function pow(v:Float, exp:Float):Float
	{
		return system.Math.Pow(v, exp);
	}
	
	public static inline function random() : Float
	{
		return rand.NextDouble();
	}

	public static function isFinite( f : Float ) : Bool
	{
		return untyped __cs__("!double.IsInfinity(f)");
	}
	
	public static function isNaN( f : Float ) : Bool
	{
		return untyped __cs__("double.IsNaN(f)");
	}
}