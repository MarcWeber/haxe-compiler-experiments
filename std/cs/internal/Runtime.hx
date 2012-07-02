package cs.internal;
import cs.Lib;
import cs.NativeArray;
import cs.NativeArray;
import system.Activator;
import system.IConvertible;
import system.reflection.MethodBase;
import system.reflection.MethodInfo;
import system.Type;

/**
 This class is meant for internal compiler use only. It provides the Haxe runtime
 compatibility to the host language.
**/

@:nativegen
@:native('haxe.lang.Runtime')
@:classContents('
	public static object getField(haxe.lang.HxObject obj, string field, int fieldHash, bool throwErrors)
	{
		if (obj == null && !throwErrors) return null;
		return obj.__hx_getField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, throwErrors, false, false);
	}
	
	public static double getField_f(haxe.lang.HxObject obj, string field, int fieldHash, bool throwErrors)
	{
		if (obj == null && !throwErrors) return 0.0;
		return obj.__hx_getField_f(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, throwErrors, false);
	}
	
	public static object setField(haxe.lang.HxObject obj, string field, int fieldHash, object value)
	{
		return obj.__hx_setField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, value, false);
	}
	
	public static double setField_f(haxe.lang.HxObject obj, string field, int fieldHash, double value)
	{
		return obj.__hx_setField_f(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, value, false);
	}
	
	public static object callField(haxe.lang.HxObject obj, string field, int fieldHash, Array args)
	{
		return obj.__hx_invokeField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, args);
	}
')
@:keep class Runtime 
{
	public static var undefined(default, never):Dynamic = { };
	
	@:functionBody('
		return new haxe.lang.Closure(obj, field, hash);
	')
	public static function closure(obj:Dynamic, hash:Int, field:String):Dynamic
	{
		return null;
	}
	
	@:functionBody('
			if (System.Object.ReferenceEquals(v1, v2))
				return true;
			if (v1 == null || v2 == null)
				return false;
			
			System.IConvertible v1c = v1 as System.IConvertible;
			
			if (v1c != null)
			{
				System.IConvertible v2c = v2 as System.IConvertible;
				
				if (v2c == null)
				{
					return false;
				}
				
				System.TypeCode t1 = v1c.GetTypeCode();
				System.TypeCode t2 = v2c.GetTypeCode();
				if (t1 == t2)
					return v1c.Equals(v2c);
				
				switch(t1)
				{
					case System.TypeCode.Int64:
					case System.TypeCode.UInt64:
						return v1c.ToUInt64(null) == v2c.ToUInt64(null);
					default:
						return v1c.ToDouble(null) == v2c.ToDouble(null);
				}
			}
			
			System.ValueType v1v = v1 as System.ValueType;
			if (v1v != null)
			{
				return v1.Equals(v2);
			}
			
			return false;
	')
	public static function eq(v1:Dynamic, v2:Dynamic):Bool
	{
		return false;
	}
	
	@:functionBody('
			return System.Object.ReferenceEquals(v1, v2);
	')
	public static function refEq(v1: { }, v2: { } ):Bool
	{
		return false;
	}
	
	@:functionBody('
			return (obj == null) ? 0.0 : ((System.IConvertible) obj).ToDouble(null);
	')
	public static function toDouble(obj:Dynamic):Float
	{
		return 0.0;
	}
	
	@:functionBody('
			return (obj == null) ? 0 : ((System.IConvertible) obj).ToInt32(null);
	')
	public static function toInt(obj:Dynamic):Int
	{
		return 0;
	}
	
	@:functionBody('
			System.IConvertible cv1 = obj as System.IConvertible;
			if (cv1 != null)
			{
				return cv1.ToDouble(null) == cv1.ToInt32(null);
			}
			return false;
	')
	public static function isInt(obj:Dynamic):Bool
	{
		return false;
	}
	
	@:functionBody('
			System.IConvertible cv1 = v1 as System.IConvertible;
			if (cv1 != null)
			{
				System.IConvertible cv2 = v2 as System.IConvertible;
				
				if (cv2 == null)
				{
					throw new System.ArgumentException("Cannot compare " + v1.GetType().ToString() + " and " + v2.GetType().ToString());
				}
				
				switch(cv1.GetTypeCode())
				{
					case System.TypeCode.String:
						if (cv2.GetTypeCode() != System.TypeCode.String)
							throw new System.ArgumentException("Cannot compare " + v1.GetType().ToString() + " and " + v2.GetType().ToString());
						return v1.ToString().CompareTo(v2);
					/*case System.TypeCode.Int64:
					case System.TypeCode.UInt64:
						return ((int) (cv1.ToUInt64() - cv2.ToUInt64())) no Int64 operator support */
					default:
						return ((int) (cv1.ToDouble(null) - cv2.ToDouble(null)));
				}
			}
			
			System.IComparable c1 = v1 as System.IComparable;
			System.IComparable c2 = v2 as System.IComparable;
			
			if (c1 == null || c2 == null)
			{
				if (c1 == c2)
					return 0;
				
				throw new System.ArgumentException("Cannot compare " + v1.GetType().ToString() + " and " + v2.GetType().ToString());
			}
			
			return c1.CompareTo(c2);
	')
	public static function compare(v1:Dynamic, v2:Dynamic):Int
	{
		return 0;
	}
	
	@:functionBody('
			if (v1 is string || v2 is string)
				return (v1 + "") + (v2 + "");
			
			System.IConvertible cv1 = v1 as System.IConvertible;
			if (cv1 != null)
			{
				System.IConvertible cv2 = v2 as System.IConvertible;
				
				if (cv2 == null)
				{
					throw new System.ArgumentException("Cannot dynamically add " + v1.GetType().ToString() + " and " + v2.GetType().ToString());
				}
				
				return cv1.ToDouble(null) + cv2.ToDouble(null);
			}
			
			throw new System.ArgumentException("Cannot dynamically add " + v1 + " and " + v2);
	')
	public static function plus(v1:Dynamic, v2:Dynamic):Dynamic
	{
		return null;
	}
	
	@:functionBody('
	
		if (obj == null)
			if (throwErrors) 
				throw new System.NullReferenceException("Cannot access field \'" + field + "\' of null.");
			else
				return null;
		
		System.Type t = obj as System.Type;
		System.Reflection.BindingFlags bf;
        if (t == null)
		{
			string s = obj as string;
			if (s != null)
				return haxe.lang.StringRefl.handleGetField(s, field, throwErrors);
			t = obj.GetType();
			bf = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.FlattenHierarchy;
		} else {
			if (obj == typeof(string) && field.Equals("fromCharCode"))
				return new haxe.lang.Closure(typeof(haxe.lang.StringExt), field, 0);
			
			obj = null;
			bf = System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.Public;
		}

		System.Reflection.FieldInfo f = t.GetField(field, bf);
		if (f != null)
		{
			return haxe.lang.Runtime.unbox(f.GetValue(obj));
		} else {
			System.Reflection.PropertyInfo prop = t.GetProperty(field, bf);
			if (prop == null)
			{
				System.Reflection.MemberInfo[] m = t.GetMember(field, bf);
				if (m.Length > 0)
				{
					return new haxe.lang.Closure(obj != null ? obj : t, field, 0);
				} else {
					if (throwErrors) 
						throw HaxeException.wrap("Cannot access field \'" + field + "\'.");
					else
						return null;
				}
			}
			return haxe.lang.Runtime.unbox(prop.GetValue(obj, null));
		}
	
	')
	public static function slowGetField(obj:Dynamic, field:String, throwErrors:Bool):Dynamic
	{
		return null;
	}
	
	@:functionBody('
		if (obj == null) return false;
		System.Type t = obj as System.Type;
		System.Reflection.BindingFlags bf;
        if (t == null)
		{
			string s = obj as string;
			if (s != null)
				return haxe.lang.StringRefl.handleGetField(s, field, false) != null;
			t = obj.GetType();
			bf = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.FlattenHierarchy;
		} else {
			if (t == typeof(string))
				return field.Equals("fromCharCode");
			obj = null;
			bf = System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.Public;
		}

		System.Reflection.MemberInfo[] mi = t.GetMember(field, bf);
		return mi != null && mi.Length > 0;
	')
	public static function slowHasField(obj:Dynamic, field:String):Bool
	{
		return false;
	}
	
	@:functionBody('
		if (obj == null)
			throw new System.NullReferenceException("Cannot access field \'" + field + "\' of null.");
		
		System.Type t = obj as System.Type;
		System.Reflection.BindingFlags bf;
        if (t == null)
		{
			t = obj.GetType();
			bf = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.FlattenHierarchy;
		} else {
			obj = null;
			bf = System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.Public;
		}

		System.Reflection.FieldInfo f = t.GetField(field, bf);
		if (f != null)
		{
			if (f.FieldType.ToString().StartsWith("haxe.lang.Null"))
			{
				@value = haxe.lang.Runtime.mkNullable(@value, f.FieldType);
			}
			
			f.SetValue(obj, @value);
			return @value;
		} else {
			System.Reflection.PropertyInfo prop = t.GetProperty(field, bf);
			if (prop == null)
			{
				throw haxe.lang.HaxeException.wrap("Field \'" + field + "\' not found for writing from Class " + t);
			}
			
			if (prop.PropertyType.ToString().StartsWith("haxe.lang.Null"))
			{
				@value = haxe.lang.Runtime.mkNullable(@value, prop.PropertyType);
			}
			prop.SetValue(obj, @value, null);

			return @value;
		}
		
	')
	public static function slowSetField(obj:Dynamic, field:String, value:Dynamic):Dynamic
	{
		//not implemented yet;
		throw "Not implemented";
	}
	
	public static function callMethod(obj:Dynamic, methods:NativeArray<MethodBase>, methodLength:Int, args:Array<Dynamic>):Dynamic
	{
		var length = args.length;
		var oargs:NativeArray<Dynamic> = new NativeArray(length);
		var ts:NativeArray<system.Type> = new NativeArray(length);
		
		for (i in 0...length)
		{
			oargs[i] = args[i];
			if (args[i] != null)
				ts[i] = Lib.getNativeType(args[i]);
		}
		
		var last = 0;
		
		//first filter by number of parameters and if it is assignable
		if (methodLength > 1)
		{
			for (i in 0...methodLength)
			{
				var params = methods[i].GetParameters();
				if (params.Length != length) {
					continue;
				} else {
					var fits = true;
					for (i in 0...params.Length)
					{
						var param = params[i].ParameterType;
						var strParam = param + "";
						if (untyped strParam.StartsWith("haxe.lang.Null") || ( (oargs[i] == null || Std.is(oargs[i], IConvertible) ) && cast(untyped __typeof__(IConvertible), Type).IsAssignableFrom(param) ))
						{
							continue;
						} else if (!param.ContainsGenericParameters && !param.IsAssignableFrom(ts[i])) {
							fits = false;
							break;
						}
					}
					
					if (fits)
					{
						methods[last++] = methods[i];
					}
				}
			}
			
			methodLength = last;
		}
		
		//At this time, we should be left with only one method.
		//Of course, realistically, we can be left with plenty of methods, if there are lots of variants with IConvertible
		//But at this time we still aren't rating the best methods
		//FIXME rate best methods
		
		if (methodLength == 0)
			throw "Invalid calling parameters for method " + methods[0].Name;
		
		var params = methods[0].GetParameters();
		for (i in 0...params.Length)
		{
			var param = params[i].ParameterType;
			var strParam = param + "";
			if (StringTools.startsWith(strParam, "haxe.lang.Null"))
			{
				oargs[i] = mkNullable(oargs[i], param);
			} else if (cast(untyped __typeof__(IConvertible), Type).IsAssignableFrom(param)) {
				if (oargs[i] == null) {
					if (param.IsValueType)
						oargs[i] = Activator.CreateInstance(param);
				} else {
					oargs[i] = cast(oargs[i], IConvertible).ToType(param, null);
				}
			}
		}
		
		if (methods[0].ContainsGenericParameters && Std.is(methods[0], system.reflection.MethodInfo))
		{
			var m:MethodInfo = cast methods[0];
			var tgs = m.GetGenericArguments();
			for (i in 0...tgs.Length)
			{
				tgs[i] = untyped __typeof__(Dynamic);
			}
			m = m.MakeGenericMethod(tgs);
			var retg = m.Invoke(obj, oargs);
			return cs.internal.Runtime.unbox(retg);
		}
		
		var m = methods[0];
		if (obj == null && Std.is(m, system.reflection.ConstructorInfo))
		{
			var ret = cast(m, system.reflection.ConstructorInfo).Invoke(oargs);
			return unbox(ret);
		}
		
		var ret = m.Invoke(obj, oargs);
		return unbox(ret);
	}
	
	public static function unbox(dyn:Dynamic):Dynamic
	{
		if (dyn != null && untyped (Lib.getNativeType(dyn) + "").StartsWith("haxe.lang.Null"))
		{
			return dyn.toDynamic();
		} else {
			return dyn;
		}
	}
	
	@:functionBody('
		if (nullableType.ContainsGenericParameters)
			return haxe.lang.Null<object>.ofDynamic<object>(obj);
		return nullableType.GetMethod("_ofDynamic").Invoke(null, new object[] { obj });
	')
	public static function mkNullable(obj:Dynamic, nullableType:system.Type):Dynamic
	{
		return null;
	}
	
	@:functionBody('
		if (args == null) args = new Array<object>();
		
		System.Reflection.BindingFlags bf;
		System.Type t = obj as System.Type;
		if (t == null)
		{
			string s = obj as string;
			if (s != null)
				return haxe.lang.StringRefl.handleCallField(s, field, args);
			t = obj.GetType();
			bf = System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.FlattenHierarchy;
		} else {
			if (t == typeof(string) && field.Equals("fromCharCode"))
				return haxe.lang.StringExt.fromCharCode(toInt(args[0]));
			obj = null;
			bf = System.Reflection.BindingFlags.Static | System.Reflection.BindingFlags.Public;
		}
		
		System.Reflection.MethodInfo[] mis = t.GetMethods(bf);
		int last = 0;
		for (int i = 0; i < mis.Length; i++)
		{
			if (mis[i].Name.Equals(field))
			{
				mis[last++] = mis[i];
			}
		}
		
		if (last == 0)
		{
			throw haxe.lang.HaxeException.wrap("Method \'" + field + "\' not found on type " + t);
		}
		
		return haxe.lang.Runtime.callMethod(obj, mis, last, args);
	')
	public static function slowCallField(obj:Dynamic, field:String, args:Array<Dynamic>):Dynamic
	{
		throw "not implemented";
	}
	
	@:functionBody('
		haxe.lang.HxObject hxObj = obj as haxe.lang.HxObject;
		if (hxObj != null)
			return hxObj.__hx_invokeField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, args);
		
		return slowCallField(obj, field, args);
	')
	public static function callField(obj:Dynamic, field:String, fieldHash:Int, args:Array<Dynamic>):Dynamic
	{
		return null;
	}
	
	@:functionBody('
	
		haxe.lang.HxObject hxObj = obj as haxe.lang.HxObject;
		if (hxObj != null)
			return hxObj.__hx_getField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, throwErrors, false, false);
		
		return slowGetField(obj, field, throwErrors);
	
	')
	public static function getField(obj:Dynamic, field:String, fieldHash:Int, throwErrors:Bool):Dynamic
	{
		return null;
	}
	
	@:functionBody('
	
		haxe.lang.HxObject hxObj = obj as haxe.lang.HxObject;
		if (hxObj != null)
			return hxObj.__hx_getField_f(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, throwErrors, false);
		
		return (double)slowGetField(obj, field, throwErrors);
	
	')
	public static function getField_f(obj:Dynamic, field:String, fieldHash:Int, throwErrors:Bool):Float
	{
		return 0.0;
	}
	
	@:functionBody('
	
		haxe.lang.HxObject hxObj = obj as haxe.lang.HxObject;
		if (hxObj != null)
			return hxObj.__hx_setField(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, value, false);
		
		return slowSetField(obj, field, value);
	
	')
	public static function setField(obj:Dynamic, field:String, fieldHash:Int, value:Dynamic):Dynamic
	{
		return null;
	}
	
	@:functionBody('

		haxe.lang.HxObject hxObj = obj as haxe.lang.HxObject;
		if (hxObj != null)
			return hxObj.__hx_setField_f(field, (fieldHash == 0) ? haxe.lang.FieldLookup.hash(field) : fieldHash, value, false);
		
		return (double)slowSetField(obj, field, value);
	
	')
	public static function setField_f(obj:Dynamic, field:String, fieldHash:Int, value:Float):Float
	{
		return 0.0;
	}
	
	public static function toString(obj:Dynamic):String
	{
		if (obj == null) 
			return null;
		return untyped obj.ToString();
	}
	
	@:functionBody('
			if (t1 == null || t2 == null)
				return t1 == t2;
			return t1.Name.Equals(t2.Name);
	')
	public static function typeEq(t1:system.Type, t2:system.Type):Bool
	{
		return false;
	}
}

@:native("haxe.lang.EmptyObject") private enum EmptyObject
{
	EMPTY;
}