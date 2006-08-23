package tools.haxlib;

class SiteProxy extends haxe.remoting.Proxy<tools.haxlib.SiteApi> {
}

class Main {

	static var VERSION = 100;
	static var SERVER = {
		host : "localhost",
		port : 2000,
		url : "index.n"
	};

	var argcur : Int;
	var args : Array<String>;
	var commands : List<{ name : String, doc : String, f : Void -> Void }>;
	var site : SiteProxy;

	function new() {
		argcur = 1;
		args = neko.Sys.args();
		commands = new List();
		addCommand("search",search,"list libraries matching a word");
		addCommand("infos",infos,"list informations on a given library");
		addCommand("user",user,"list informations on a given user");
		addCommand("submit",submit,"submit or update a library package");
		site = new SiteProxy(haxe.remoting.Connection.urlConnect("http://"+SERVER.host+":"+SERVER.port+"/"+SERVER.url).api);
	}

	function param( name ) {
		if( args.length > argcur )
			return args[argcur++];
		neko.Lib.print(name+" : ");
		return neko.io.File.stdin().readLine();
	}

	function addCommand( name, f, doc ) {
		commands.add({ name : name, doc : doc, f : f });
	}

	function usage() {
		var vmin = Std.string(VERSION % 100);
		var ver = Std.int(VERSION/100) + "." + if( vmin.length == 0 ) "0"+vmin else vmin;
		print("Haxe Library Manager "+ver+" - (c)2006 Motion-Twin");
		print(" Usage : haxlib [command] [options]");
		print(" Commands :");
		for( c in commands )
			print("  "+c.name+" : "+c.doc);
		neko.Sys.exit(1);
	}

	function run() {
		var cmd = args[0];
		if( cmd == null )
			usage();
		for( c in commands )
			if( c.name == cmd ) {
				c.f();
				return;
			}
		print("Unknown command "+cmd);
		usage();
	}

	// ---- COMMANDS --------------------

 	function search() {
		var word = param("Search word");
		var l = site.search(word);
		for( s in l )
			print(s.name);
		print(l.length+" libraries found");
	}

	function infos() {
		var prj = param("Library name");
		var inf = site.infos(prj);
		print("Name: "+inf.name);
		print("Desc: "+inf.desc);
		print("Website: "+inf.url);
		print("Owner: "+inf.owner);
		print("Version: "+inf.curversion);
		print("Releases: ");
		if( inf.versions.length == 0 )
			print("  (no version released yet)");
		for( v in inf.versions )
			print("   "+v.date+" "+v.name+" : "+v.comments);
	}

	function user() {
		var uname = param("User name");
		var inf = site.user(uname);
		print("Id: "+inf.name);
		print("Name: "+inf.fullname);
		print("Mail: "+inf.email);
		print("Libraries: ");
		if( inf.libraries.length == 0 )
			print("  (no libraries)");
		for( p in inf.libraries )
			print("  "+p);
	}

	function register(name) {
		print("This is your first submission as '"+name+"'");
		print("Please enter the following informations for registration");
		var email = param("Email");
		var fullname = param("Fullname");
		var pass = haxe.Md5.encode(param("Password"));
		param("Enter to Confirm");
		site.register(name,pass,email,fullname);
		return pass;
	}

	function submit() {
		var file = param("Package");
		var data = neko.io.File.getContent(file);
		var zip = neko.zip.File.read(new neko.io.StringInput(data));
		var infos = Datas.readInfos(zip);
		var password;
		site.checkLibOwner(infos.lib,infos.user);
		if( site.isNewUser(infos.user) )
			password = register(infos.user);
		else {
			password = haxe.Md5.encode(param("Password"));
			if( !site.checkPassword(infos.user,password) )
				throw "Invalid password for "+infos.user;
		}

		var id = site.getSubmitId();

		// directly send the file data over Http
		// we can't use haxe.Http because we want *sent* data progress
		var s = new neko.io.Socket();
		s.connect(neko.io.Socket.resolve(SERVER.host),SERVER.port);
		s.write("POST /"+SERVER.url+"?submit="+id);
		s.write(" HTTP/1.1\r\nHost: "+SERVER.host+"\r\n");
		s.write("Content-Type: application/octet-stream\r\n");
		s.write("Content-Length: "+data.length+"\r\n");
		s.write("\r\n");
		var pos = 0;
		var bufsize = 1;
		print("Sending data.... ");
		while( pos < data.length ) {
			s.write(data.substr(pos,bufsize));
			pos += bufsize;
			neko.Lib.print( Std.int((pos * 100) / data.length) + "%\r" );
		}
		s.shutdown(false,true);
		s.input.readAll();
		s.close();

		var msg = site.processSubmit(id,password);
		print(msg);
	}

	// ----------------------------------

	static function print(str) {
		neko.Lib.print(str+"\n");
	}

	static function main() {
		new Main().run();
	}

}
