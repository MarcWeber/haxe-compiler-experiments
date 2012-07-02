package system;

/**
	Warning: This class definition is incomplete.
	In order to get most current extern definitions, install/update hxcs library with:
		haxelib install hxcs
	Please refer to http://lib.haxe.org/p/hxcs for more information.
**/
@:native('System.Console') extern class Console 
{
	static var Error(default, null):system.io.StreamWriter;
	static var In(default, null):system.io.StreamReader;
	static var Out(default, null):system.io.StreamWriter;
	
	static function Write(obj:Dynamic):Void;
	static function WriteLine(obj:Dynamic):Void;
	
	#if !(Xbox || CF || MF)
	static function OpenStandardOutput():system.io.Stream;
	static function OpenStandardInput():system.io.Stream;
	static function OpenStandardError():system.io.Stream;
	#end
}