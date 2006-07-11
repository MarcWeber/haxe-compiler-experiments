package flash;

extern class SharedObject
{
	#if flash8
	static function getLocal(name:String,?localPath:String,?secure:Bool):SharedObject;
	#else true
	static function getLocal(name:String,?localPath:String):SharedObject;
	#end

	static function getRemote(name:String,remotePath:String,?persistence:Dynamic):SharedObject;
	static function deleteAll(url:String) : Void;
	static function getDiskUsage(url:String) : Int;

	function send( handler : String, ?p1 : Dynamic, ?p2 : Dynamic, ?p3 : Dynamic, ?p4 : Dynamic, ?p5 : Dynamic ) : Void;
	function flush(?minDiskSpace:Float):Dynamic;
	function connect( cnx : NetConnection ) : Bool;
	function close():Void;
	function getSize():Float;
	function setFps(updatesPerSecond:Float):Bool;

	function onStatus(infoObject:Dynamic):Void;
	function onSync(objArray:Array<Dynamic>):Void;

	function clear() : Void;

	var data:Dynamic;

	private static function __init__() : Void untyped {
		flash.SharedObject = _global["SharedObject"];
	}

}
