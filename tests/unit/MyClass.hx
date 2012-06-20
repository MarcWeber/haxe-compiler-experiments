package unit;

class MyClass {

	#if as3 public #end var val : Int;

	public var ref : MyClass;
	public var intValue : Int;
	public var stringValue : String;

	public function new(v) {
		val = v;
		intValue = 55;
	}

	public function get() {
		return val;
	}

	public function set(v) {
		val = v;
	}

	public function add(x,y) {
		return val + x + y;
	}
}

class MyParent {
	public function new() { }
	function a() return 11
	function b() return 20
}

class MyChild1 extends MyParent {
	public override function a() { return 12; }
	override function b() return 21
	function c() return 19
}

class MyChild2 extends MyParent {
	public function test1(mc1:MyChild1) return mc1.b()
}

interface I1 { }
class Base { public var s:String; public function new() { } }
class Child1 extends Base { public function new() { super(); } }
class Child2 extends Base, implements I1 { public function new() { super(); } }
class Child2_1 extends Child2 { public function new() { super(); } }
class Unrelated implements I1 { public var s:String; public var t:Int;  public function new() { } }

interface I2 implements I1 { }
class ClassI2 implements I2 { public function new() { } }

class CI1 extends Base, implements I1 { public function new() { super(); } }
class CI2 extends Base, implements I1 { public function new() { super(); } }
class CII1 extends CI1 { public function new() { super(); } }
class CII2 extends CI2 { public function new() { super(); } }

class PClassBase<T> { public function new() {  } }
class PClass1<T> extends PClassBase<Float> { public function new() { super(); } }
class PClass2<T> extends PClassBase<T> { public function new(t:T) { super(); } }

interface CovI {
	public function covariant():Base;
}

interface CovI2 implements CovI {
	public function covariant():Child2;
}

class Cov1 {
	public function new() { }
	public function covariant():Base { return new Base(); }
}

class Cov2 extends Cov1, implements CovI {
	public function new() { super(); }
	public override function covariant():Child1 { return new Child1(); }
}

class Cov3 implements CovI2
{
	public function new() { }
	public function covariant():Child2_1 { return new Child2_1(); }
}

class Ctrv1 {
	public function new() { }
	public function contravariant(arg:Child1) { }
}

class Ctrv2 extends Ctrv1 {
	public function new() { super(); }
	public override function contravariant(arg:Base) { }
}

class InitBase {
	public var i = 2;
	public var s = "foo";
	public var b = true;
	public var t = String;
	public function new() { }
}

class InitChild extends InitBase { }

class InitChildWithCtor extends InitBase {
	public function new(_) {
		super();
	}
}

class InitWithoutCtor {
	public var i = 2;
}

class InitProperties {
	public var accNull(default, null):Int = 3;
	public var accDefault(default, default):Int = 3;
	public var accFunc(default, set_accFunc):Int = 3;
	public var accNever(default, never):Int = 3;
	public var accDynamic(default, dynamic):Int = 3;
	
	function set_accFunc(v) return throw "setter was called"
	
	public function new() { }
}

class ParamConstraintsClass {
	public function new() { }
	static public function staticSingle< A:Base > (a:A):A { return a; }
	public function memberSingle< A:Base > (a:A):A { return a; }
	public function memberMultiple < A:(Base, I1) > (a:A):A { return a; }
	public function memberComplex < A:I1, B:List<A> > (a:A, b:B) { return b; }
	public function memberBasic < A:String, B:Array<A> > (a:A, b:B) { return b[0]; }
	
	@:overload(function< A, B:Array<A> > (a:A, b:B):Void { } )
	public function memberOverload<A,B>(a:String, b:String) { }
}