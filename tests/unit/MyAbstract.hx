package unit;

abstract MyAbstract(Int) {

    public inline function new(x) {
        this = x;
    }

    public inline function incr() {
        return ++this;
    }

    public inline function toInt() : Int {
        return this;
    }

}

abstract TemplateWrap(haxe.Template) {
	public inline function new(x) {
		this = new haxe.Template(x);
	}
	
	public inline function get()
		return this
	
	@:from static inline public function fromString(s:String) {
		return new TemplateWrap(s);
	}
	
	@:to inline function toString() {
		return this.execute( { t: "really works!"});
	}
}

abstract Meter(Float) {
	public inline function new(f)
		this = f
	
	public inline function get()
		return this
		
	@:from static public inline function fromFloat(f:Float)
		return new Meter(f)
}

abstract Kilometer(Float) {
	public inline function new(f)
		this = f
		
	@:from static public inline function fromMeter(m:Meter)
		return new Kilometer(m.get() / 1000.)
		
	@:to public inline function toFloat()
		return this
}