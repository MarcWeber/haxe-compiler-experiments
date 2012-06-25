package system;
import cs.NativeArray;
import system.reflection.ConstructorInfo;

@:native("System.Type")
extern class Type 
{
	public var BaseType(default, null):Type;
	public var IsInterface(default, null):Bool;
	public var ContainsGenericParameters(default, null):Bool;
	public var IsValueType(default, null):Bool;
	public function IsAssignableFrom(c:Type):Bool;
	
	public static function GetType(name:String):Null<Type>;
	public function GetConstructors():NativeArray<ConstructorInfo>;
}