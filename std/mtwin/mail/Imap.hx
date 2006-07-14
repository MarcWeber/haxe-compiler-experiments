/*
 * Copyright (c) 2006, Motion-Twin
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   - Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   - Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY MOTION-TWIN "AS IS" AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE HAXE PROJECT CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
package mtwin.mail;

import neko.Socket;
import mtwin.mail.Exception;

signature ImapMailbox {
	name: String,
	flags: List<String>,
	hasChildren: Bool
}

enum ImapSection {
	Flags;
	Uid;
	BodyStructure;
	Envelope;
	InternalDate;
	Body(ss:ImapBodySection);
	BodyPeek(ss:ImapBodySection);
}

enum ImapBodySection {
	Header;
	Mime;
	Text;
	SubSection(id:String,ss:ImapBodySection);
}

enum ImapRange {
	Single(i:Int);
	Range(s:Int,e:Int);
	Composite(l:Array<ImapRange>);
}

class Imap {
	public static var DEBUG = false;

	var cnx : Socket;
	var count : Int;

	static var REG_RESP = ~/(OK|NO|BAD) (\[([^\]]+)\] )?(([A-Z]{2,}) )? ?(.*)/;
	static var REG_EXISTS = ~/^([0-9]+) EXISTS$/;
	static var REG_RECENT = ~/^([0-9]+) RECENT$/;
	static var REG_UNSEEN = ~/^OK \[UNSEEN ([0-9]+)\]/;
	static var REG_FETCH_MAIN = ~/([0-9]+) FETCH \(/;
	static var REG_FETCH_PART = ~/^(BODY\[[A-Za-z0-9.]*\]|RFC822\.?[A-Z]*) \{([0-9]+)\}/;
	static var REG_FETCH_FLAGS = ~/^FLAGS \(([ \\A-Za-z0-9$]*)\) */;
	static var REG_FETCH_UID = ~/^UID ([0-9]+) */;
	static var REG_FETCH_BODYSTRUCTURE = ~/^BODY(STRUCTURE)? \(/;
	static var REG_FETCH_END = ~/^([A0-9]{4}) (OK|BAD|NO)/;
	static var REG_LIST_RESP = ~/LIST \(([ \\A-Za-z0-9]*)\) "\." "([^"]+)"/;
	static var REG_CRLF = ~/\r?\n/g;

	static function rmCRLF(s){
		return REG_CRLF.replace(s, "");
	}

	static function quote( s : String ) : String {
		return "\""+s.split("\"").join("\\\"")+"\"";
	}

	static function debug(s:String){
		if( DEBUG ) neko.Lib.print(Std.string(s)+"\n");
	}

	//////

	public function new(){
		count = 0;
	}

	/**
		Connect to Imap Server
	**/
	public function connect( host : String, port : Int ){
		if( cnx != null ) throw AlreadyConnected;
		cnx = new Socket();
		try{
			cnx.connect( Socket.resolve(host), port );
		}catch( e : Dynamic ){
			throw ConnectionError(host,port);
		}
		debug("socket connected");
		cnx.setTimeout( 1 );
		cnx.readLine();
	}

	/**
		Login to server
	**/
	public function login( user : String, pass : String ){	
		var r = command("LOGIN",user+" "+pass,true);
		if( !r.success ){
			throw BadResponse(r.response);
		}
	}

	/**
		Close connection to server
	**/
	public function close(){
		cnx.close();
		cnx = null;
	}

	/**
		List mailboxes that match pattern (all mailboxes if pattern is null)
	**/
	public function mailboxes( ?pattern : String ) : List<ImapMailbox> {
		var r;
		if( pattern == null ){
			r = command("LIST","\".\" \"*\"",true);
		}else{
			r = command("LIST","\".\" \""+pattern+"\"",true);
		}
		if( !r.success ){
			throw BadResponse(r.response);
		}

		var ret = new List();
		for( v in r.result ){
			if( REG_LIST_RESP.match(v) ){	
				var name = REG_LIST_RESP.matched(2);
				var flags = REG_LIST_RESP.matched(1).split(" ");

				var t = {name: name,flags: new List(),hasChildren: false};

				for( v in flags ){
					if( v == "" ) continue;
					
					if( v == "\\HasNoChildren" ){
						t.hasChildren = false;
					}else if( v == "\\HasChildren" ){
						t.hasChildren = true;
					}

					t.flags.add( v );
				}

				ret.add(t);
			}
		}
		return ret;
	}

	/**
		Select a mailbox
	**/
	public function select( mailbox : String ){
		var r = command("SELECT",quote(mailbox),true);
		if( !r.success ) 
			throw BadResponse(r.response);
		
		var ret = {recent: 0,exists: 0,firstUnseen: null};
		for( v in r.result ){
			if( REG_EXISTS.match(v) ){
				ret.exists = Std.parseInt(REG_EXISTS.matched(1));
			}else if( REG_UNSEEN.match(v) ){
				ret.firstUnseen = Std.parseInt(REG_UNSEEN.matched(1));
			}else if( REG_RECENT.match(v) ){
				ret.recent = Std.parseInt(REG_RECENT.matched(1));
			}
		}

		return ret;
	}

	/**
		Search for messages. Pattern syntax described in RFC 2060, section 6.4.4
	**/
	public function search( ?pattern : String ){
		if( pattern == null ) pattern = "ALL";
		var r = command("SEARCH",pattern,true);
		if( !r.success ){
			throw BadResponse(r.response);
		}

		var l = new List();

		for( v in r.result ){
			if( StringTools.startsWith(v,"SEARCH ") ){
				var t = v.substr(7,v.length-7).split(" ");
				for( i in t ){
					l.add( Std.parseInt(i) );
				}
			}
		}

		return l;
	}

	/**
		Search for messages, fetch those found.
	**/
	public function fetchSearch( pattern : String, ?section : Array<ImapSection> ){
		if( section == null ) section = [BodyPeek(null)];
		var r = search(pattern);
		if( r.length == 0 ) return new IntHash();

		var t = new Array<ImapRange>();
		for( i in r ){
			t.push(Single(i));
		}

		return fetchRange( Composite(t), section );
	}

	/**
		Fetch one message by its id ou uid
	**/
	public function fetchOne( id : Int, ?section : Array<ImapSection>, ?useUid : Bool ) {
		if( section == null ) section = [BodyPeek(null)];
		if( useUid == null ) useUid = false;

		var r = fetchRange( Single(id), section, useUid );
		if( !r.exists(id) ){
			throw ImapFetchError(id);
		}
		return r.get(id);
	}

	/**
		Fetch some messages
	**/
	public function fetchRange( iRange: ImapRange, ?iSection : Array<ImapSection>, ?useUid : Bool ){
		if( iRange == null ) return null;
		if( iSection == null ) iSection = [Body(null)];
		if( useUid == null ) useUid = false;

		var range = Tools.imapRangeString(iRange);
		var section = Tools.imapSectionString(iSection);
		
		if( useUid )
			command("UID FETCH",range+" "+section,false);
		else
			command("FETCH",range+" "+section,false);

		var ret = new IntHash();
		while( true ){
			var l = cnx.readLine();
			if( REG_FETCH_MAIN.match(l) ){
				var id = Std.parseInt(REG_FETCH_MAIN.matched(1));
				
				var o = if( ret.exists(id) ){
					ret.get(id); 
				}else {
					var o = {type: null,content: null,flags: null,uid: null,structure: null};
					ret.set(id,o);
					o;
				}

				var s = REG_FETCH_MAIN.matchedRight();
				while( s.length > 0 ){
					if( REG_FETCH_FLAGS.match( s ) ){
						o.flags = REG_FETCH_FLAGS.matched(1).split(" ");
						s = REG_FETCH_FLAGS.matchedRight();
					}else if( REG_FETCH_UID.match( s ) ){
						o.uid = Std.parseInt(REG_FETCH_UID.matched(1));
						s = REG_FETCH_UID.matchedRight();
					}else if( REG_FETCH_BODYSTRUCTURE.match( s ) ){
						var t = REG_FETCH_BODYSTRUCTURE.matchedRight().substr(0,-1);
						o.structure = ImapBodyStructure.parse( t );
						break;
					}else if( REG_FETCH_PART.match( s ) ){
						var type = REG_FETCH_PART.matched(1);
						var len = Std.parseInt(REG_FETCH_PART.matched(2));
						
						o.content = cnx.read( len );
						o.type = type;
						cnx.readLine();
						break;
					}else{
						break;
					}
				}
				
			}else if( REG_FETCH_END.match(l) ){
				var resp = REG_FETCH_END.matched(2);
				if( resp == "OK" ){
					break;
				}else{
					throw BadResponse(l);
				}
			}else{
				throw UnknowResponse(l);
			}
		}
		
		if( useUid ){
			var old = ret;
			ret = new IntHash();
			for( e in old ){
				ret.set(e.uid,e);
			}
		}

		return ret;
	}

	//

	function command( command, args, r ){
		if( cnx == null )
			throw NotConnected;

		count++;
		var c = Std.string(count);
		c = StringTools.lpad(c,"A000",4);
		cnx.write( c+" "+command+" "+args+"\r\n" );
		debug( "S: "+c+" "+command+" "+args );

		if( !r ){
			return null;
		}
		return read(c);
	}

	
	function read( c ){
		var resp = new List();
		var sb : StringBuf = null;
		while( true ){
			var line = cnx.readLine();
			debug("R: "+line);
			line = rmCRLF(line);

			if( c != null && line.substr(0,4) == c ){
				if( REG_RESP.match(line.substr(5,line.length-5)) ){
					if( sb != null ){
						resp.add( sb.toString() );
					}
					return {
						result: resp,
						success: REG_RESP.matched(1) == "OK",
						error: REG_RESP.matched(1),
						command: REG_RESP.matched(4),
						response: REG_RESP.matched(6),
						comment: REG_RESP.matched(3)
					};
				}else{
					throw UnknowResponse(line);
				}
			}else{
				if( StringTools.startsWith(line,"* ") ){
					if( sb != null ){
						resp.add( sb.toString() );
					}
					sb = new StringBuf();
					sb.add( line.substr(2,line.length - 2) );
				}else{
					if( sb != null ){
						sb.add( line+"\r\n" );
					}else{
						resp.add( line );
					}
				}
			}
		}
		return null;
	}	
}
