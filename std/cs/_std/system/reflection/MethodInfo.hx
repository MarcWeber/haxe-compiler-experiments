package system.reflection;
import cs.NativeArray;
import cs.NativeArray;
import system.Type;

@:native('System.Reflection.MethodInfo') extern class MethodInfo extends MethodBase 
{
	function MakeGenericMethod(g:NativeArray<Type>):MethodInfo;
}