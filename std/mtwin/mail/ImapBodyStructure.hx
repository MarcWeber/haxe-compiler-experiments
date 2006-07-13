package mtwin.mail;

class ImapBodyStructure {
	public var ctype0: String;
	public var ctype1: String;
	public var params : Hash<String>;
	public var parts: List<ImapBodyStructure>;

	// single-part specific
	public var id: String;
	public var description : String;
	public var encoding : String;
	public var size : Int;
	public var disposition : String;
	public var dispositionParams : Hash<String>;

	//
	public var __length : Int;
	public var imapId : String;

	public function new(){
		parts = new List();
		params = new Hash();
	}

	public function getMainPart( ?level : Int, ?priority : Int, ?cpriority : Int ) : ImapBodyStructure {
		if( level == null ) level = 0;
		if( priority == null ) priority = 0;
		if( cpriority == null ) cpriority = 0;

		if( ctype0 != "multipart" || (level == 0 && parts.length == 0) ){
			if( level == 0 ) return this;
			if( ctype1 == "html" ) return this;
			if( ctype1 == "plain" && cpriority > 0 ) return this;
		}else{
			if( level == 0 ){
				// multipart !
				// si c'est au premier niveau, c'est une boucle principale, avec priorité qui augmente
				do {
					do {
						var r = null;
						for( part in parts ){
							r = part.getMainPart( level + 1, priority, cpriority );
							if( r != null ) break;
						}
						if( r != null ) return r;
						priority++;
					}while( priority <= 1 );
					cpriority++;
				}while( cpriority <= 1 );
			}else{
				// là c'est des boucles qui se déclanche si c'est ok
				if( ctype1 == "alternative" || priority > 0 ){
					var r = null;
					for( part in parts ){
						r = part.getMainPart( level + 1, priority, cpriority );
						if( r != null ) return r;
					}
				}
			}
		}
		return null;
	}

	public function listAttachment( ?level : Int ) : List<ImapBodyStructure> {
		if( level == null ) level = 0;
		var ret = new List();
		if( ctype0 != "multipart" ){
			if( level != 0 && disposition != null ){
				ret.add( this );
			}
		}else if( ctype1 != "alternative" ){
			for( part in parts ){
				for( v in part.listAttachment( level + 1 ) ){
					ret.add( v );
				}
			}
		}
		return ret;
	}

	public static function parse( s : String, ?id : String ) : ImapBodyStructure{
		if( id == null ) id = "";
		var len = s.length;
		var parCount = 0;
		var p = 0;
		var ret = new ImapBodyStructure();
		ret.imapId = id;
		var addPart = function( p ){
			ret.parts.add( p );
		};
		var tmp = {pName: null,argPos: 0};
		var addElement = function( e : String ){
			if( ret.ctype0 == null ){
				ret.ctype0 = e;
			}else if( ret.ctype1 == null ){
				ret.ctype1 = e;
				tmp.argPos = 0;
			}else{
				if( e == "NIL" ) return;
				if( ret.ctype0 == "multipart" ){
					switch( tmp.argPos ){
						case 1:
							if( tmp.pName == null ) 
								tmp.pName = e;
							else{
								ret.params.set(tmp.pName,e);
								tmp.pName = null;
							}
						case 4:
						case 5:
					}
				}else{
					switch( tmp.argPos ){
						case 1:
							if( tmp.pName == null ) 
								tmp.pName = e;
							else{
								ret.params.set(tmp.pName,e);
								tmp.pName = null;
							}
						case 2:
							ret.id = e;
						case 3:
							ret.description = e;
						case 4:
							ret.encoding = e;
						case 5:
							ret.size = Std.parseInt(e);
						default:
							var dispoPos = if( ret.ctype0 == "text" ) 8 else if( ret.ctype0 == "message" ) 10 else 7;
							if( tmp.argPos == dispoPos ){
								if( parCount == 1 ){
									ret.disposition = e;
									ret.dispositionParams = new Hash();
								}else{
									if( tmp.pName == null ) 
										tmp.pName = e;
									else{
										ret.dispositionParams.set(tmp.pName,e);
										tmp.pName = null;
									}
								}
							}
					}
				}
				
			}
		};
		while( p < len ){
			var c = s.charAt(p);
			p++;
			switch( c ){
				case "(":
					if( ret.ctype1 == null ){
						var newPart = parse( s.substr(p,s.length-p), (if( id == "" ) "" else id + "." )+ (ret.parts.length+1) );
						addPart( newPart );
						ret.ctype0 = "multipart";
						p += newPart.__length;
					}else
						parCount++;
				case ")":
					parCount--;
					if( parCount < 0 ){
						ret.__length = p;
						return ret;
					}
				case "\"":
					var b = new StringBuf();
					var prev = null;
					while( p < len ){
						var c2 = s.charAt(p);
						p++;
						if( c2 == "\"" && prev != "\\" )
							break;
						b.add( c2 );
						prev = c2;
					}
					addElement( b.toString() );
				case " ":
					if( parCount == 0 ){
						tmp.argPos++;
					}
				default:
					var b = new StringBuf();
					p--;
					while( p < len ){
						var c2 = s.charAt(p);
						p++;
						if( c2 == ")" || c2 == " " ){
							p--;
							break;
						}
						b.add( c2 );
					}
					addElement( b.toString() );
			}
		}
		ret.__length = p;
		return ret;
	}

}
