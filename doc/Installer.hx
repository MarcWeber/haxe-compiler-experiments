
class Installer {

	static var SYS = neko.Sys.systemName();
	static var TMP = "tmp.txt";
	static var NULL = if( SYS == "Windows" ) "NUL" else "/dev/null";
	static var wnd : xcross.Winlog;

	var baseDir : String;
	var binDir : String;
	var libDir : String;
	var debug : Bool;

	function new(dbg) {
		debug = dbg;
		if( debug ) {
			libDir = "/usr/local/lib";
			binDir = "/usr/local/bin";
		} else {
			libDir = "/usr/lib";
			binDir = "/usr/bin";
		}
		baseDir = if( SYS == "Windows" ) neko.Sys.getEnv("ProgramFiles") + "/Motion-Twin" else libDir;
	}

	function newVersion(v1,v2) {
		if( v1 == null )
			return true;
		return (v1.major * 10000 + v1.minor * 100 + v1.build < v2.major * 10000 + v2.minor * 100 + v2.build);
	}

	function ask( txt ) {
		return xcross.Api.confirm("Question",txt);
	}

	function error( txt ) {
		xcross.Api.error("Error",txt);
		throw "Installation aborted";
	}

	function display( txt ) {
		wnd.log(txt);
		neko.Sys.sleep(0.03);
	}

	function version(v : { major : Int, minor : Int, build : Int } ) {
		return v.major+"."+v.minor+if( v.build > 0 ) "."+v.build else "";
	}

	function command( cmd ) {
		display("Execute "+cmd);
		if( neko.Sys.command(cmd) != 0 )
			error("Command '"+cmd+"' failed !");
	}

	function run() {
		try {
			install();
			display("Installation Completed");
			xcross.Api.message("Done","Installation Completed");
		} catch( e : Dynamic ) {
			display("");
			display("");
			display("ERROR = "+Std.string(e));
			display(haxe.Stack.toString(haxe.Stack.exceptionStack()));
			//xcross.Api.error("Error","Installation aborted");
		}
		wnd.enabled = true;
	}

	function checkRights() {
		try {
			var tmp = baseDir + "/.tmp.haxe.inst";
			var f = neko.io.File.write(tmp,true);
			f.close();
			neko.FileSystem.deleteFile(tmp);
			return true;
		} catch( e : Dynamic ) {
			if( xcross.Api.authorize() )
				return false;
			error("You don't have the rights to write in "+baseDir+", please run the installer using 'sudo'");
			return false;
		}
	}

	function install() {
		// CLEANUP
		var dirs = [
			"/usr/local/lib/neko",
			"/usr/local/lib/haxe",
			"/opt/neko",
			"/opt/haxe",
		];
		for( d in dirs )
			if( !debug && neko.FileSystem.exists(d) )
				error("A previous haXe/Neko version seems to be installed in '"+d+"', please remove it first");
		if( debug )
			display("DEBUG MODE ON");

		// PROXY
		var p = neko.net.ProxyDetect.detect();
		if( p == null )
			display("No proxy found");
		else {
			display("Using proxy "+p.host+":"+p.port);
			haxe.Http.PROXY = p;
		}

		// GET haxe Version
		display("Getting Local haXe Version");
		neko.Sys.command("haxe 2>"+TMP);
		var content = neko.io.File.getContent(TMP);
		neko.FileSystem.deleteFile(TMP);
		var r = ~/^Haxe Compiler ([0-9]+)\.([0-9]+)/;
		var haxeVersion = null;
		if( r.match(content) )
			haxeVersion = {
				major : Std.parseInt(r.matched(1)),
				minor : Std.parseInt(r.matched(2)),
				build : 0,
			};

		// GET Neko Version
		display("Getting Local Neko Version");
		neko.Sys.command("neko >"+TMP+" 2>"+NULL);
		var content = neko.io.File.getContent(TMP);
		neko.FileSystem.deleteFile(TMP);
		var r = ~/^NekoVM ([0-9]+)\.([0-9]+)(\.([0-9]+))?/;
		var nekoVersion = null;
		if( r.match(content) )
			nekoVersion = {
				major : Std.parseInt(r.matched(1)),
				minor : Std.parseInt(r.matched(2)),
				build : Std.parseInt(r.matched(4))
			};

		// GET haXe files list
		display("Getting Latest haXe Version");
		var haxeFile = null;
		var r = ~/^haxe-([0-9]+)\.([0-9]+)(-win|-linux|-osx)(\.zip|\.tar\.gz)$/;
		for( f in haxe.Http.request("http://haxe.org/latest.n").split("\n") )
			if( r.match(f) ) {
				var pf = r.matched(3);
				switch( SYS ) {
				case "Windows": if( pf != "-win" ) continue;
				case "Linux": if( pf != "-linux" ) continue;
				case "Mac": if( pf != "-osx" ) continue;
				default: continue;
				}
				haxeFile = {
					file : f,
					version : {
						major : Std.parseInt(r.matched(1)),
						minor : Std.parseInt(r.matched(2)),
						build : 0,
					},
				};
				break;
			}
		if( haxeFile == null )
			error("No haXe File found for your plaform");

		// GET Neko files list
		display("Getting Latest Neko Version");
		var nekoFile = null;
		var r = ~/^neko-([0-9]+)\.([0-9]+)(\.([0-9]+))?(-win|-linux|-osx)(\.zip|\.tar\.gz)$/;
		for( f in haxe.Http.request("http://nekovm.org/latest.n").split("\n") )
			if( r.match(f) ) {
				var pf = r.matched(5);
				switch( SYS ) {
				case "Windows": if( pf != "-win" ) continue;
				case "Linux": if( pf != "-linux" ) continue;
				case "Mac": if( pf != "-osx" ) continue;
				default: continue;
				}
				nekoFile = {
					file : f,
					version : {
						major : Std.parseInt(r.matched(1)),
						minor : Std.parseInt(r.matched(2)),
						build : Std.parseInt(r.matched(4)),
					}
				};
				break;
			}
		if( nekoFile == null )
			error("No haXe File found for your plaform");

		// ASK QUESTIONS IF OK TO INSTALL
		var needHaxe = newVersion(haxeVersion,haxeFile.version);
		var needNeko = newVersion(nekoVersion,nekoFile.version);
		if( !needHaxe && !needNeko ) {
			if( !ask("Both your haXe and Neko versions are up-to-date, do you want to reinstall everything ?") )
				error("Installation Aborted");
			needHaxe = true;
			needNeko = true;
		} else {
			var txt = "";
			if( needNeko ) {
				txt += "Neko "+version(nekoFile.version);
				if( needHaxe )
					txt += " and ";
			}
			if( needHaxe )
				txt += "haXe "+version(haxeFile.version);
			if( !ask("Do you want to install "+txt+" ?") )
				error("Installation Aborted");
		}

		// DOWNLOAD
		if( needNeko )
			download("nekovm.org",nekoFile.file);
		if( needHaxe )
			download("haxe.org",haxeFile.file);

		// INSTALL
		if( !neko.FileSystem.exists(baseDir) )
			neko.FileSystem.createDirectory(baseDir);

		if( needNeko ) {
			copy(nekoFile.file,true);
			installNeko();
		}
		if( needHaxe ) {
			copy(haxeFile.file,false);
			installHaxe();
		}
	}

	function download( domain, file ) {
		if( neko.FileSystem.exists(file) ) {
			display("Using local version of "+file+", skipping download");
			return;
		}

		var str = new neko.io.StringOutput();
		var progress = new Progress(str);
		progress.update = function() {
			var p = progress.cur * 100 / progress.max;
			p = Std.int(p * 10) / 10;
			wnd.logProgress("Downloading "+file+" ("+p+"%)");
		};
		var h = new haxe.Http("http://"+domain+"/_media/"+file);
		var me = this;
		h.onError = function(e) {
			me.error(Std.string(e));
		};
		wnd.logProgress("Downloading "+file+"...");
		h.asyncRequest(false,progress);
		wnd.log("");

		var f = neko.io.File.write(file,true);
		f.write(str.toString());
		f.close();
	}

	function unzip( file ) {
		var ch = neko.io.File.read(file,true);
		var entries = if( neko.io.Path.extension(file) == "zip" ) neko.zip.File.readZip(ch) else neko.zip.File.readTar(ch,true);
		ch.close();
		return entries;
	}

	function copy( file, isNeko ) {
		var data = unzip(file);
		var dir = baseDir + "/" + if( isNeko ) "neko" else "haxe";
		if( !neko.FileSystem.exists(dir) )
			neko.FileSystem.createDirectory(dir);
		if( !isNeko ) {
			try {
				removeRec(dir+"/std");
			} catch( e : Dynamic ) {
			}
		}
		for( f in data ) {
			var path = f.fileName.split("/");
			path.shift(); // base directory
			if( path[path.length-1] == "" ) {
				path.pop();
				if( path.length == 0 )
					continue;
				var ddir = dir+"/"+path.join("/");
				display("Installing directory "+path.join("/"));
				if( !neko.FileSystem.exists(ddir) )
					neko.FileSystem.createDirectory(ddir);
				continue;
			}
			var filename = dir + "/" + path.join("/");
			var ch = neko.io.File.write(filename,true);
			ch.write(neko.zip.File.unzip(f));
			ch.close();
			if( SYS != "Windows" ) {
				var exe = neko.io.Path.extension(filename) == "";
				neko.Sys.command("chmod "+(if( exe ) 755 else 644)+" "+filename);
			}
		}
	}

	function link( dir, file, dest ) {
		command("rm -rf "+dest+"/"+file);
		command("ln -s "+baseDir+"/"+dir+"/"+file+" "+dest+"/"+file);
	}

	function installNeko() {
		if( SYS == "Windows" )
			return;
		var so = if( SYS == "Mac" ) ".dylib" else ".so";
		link("neko","neko",binDir);
		link("neko","nekoc",binDir);
		link("neko","nekotools",binDir);
		link("neko","libneko"+so,libDir);
	}

	function installHaxe() {
		if( SYS == "Windows" ) {
			command('"'+baseDir+'/haxe/haxesetup" -silent');
			return;
		}
		link("haxe","haxe",binDir);
		link("haxe","haxelib",binDir);
		link("haxe","haxedoc",binDir);
		// HAXELIB setup
		var haxelib = baseDir + "/haxe/lib";
		if( !neko.FileSystem.exists(haxelib) ) {
			neko.FileSystem.createDirectory(haxelib);
			neko.Sys.command("chmod 666 "+haxelib);
		}
	}

	function removeRec( file ) {
		if( !neko.FileSystem.isDirectory(file) ) {
			neko.FileSystem.deleteFile(file);
			return;
		}
		for( f in neko.FileSystem.readDirectory(file) )
			removeRec(file+"/"+f);
		neko.FileSystem.deleteDirectory(file);
	}

	static function main() {
		var debug = neko.Sys.getEnv("INST_DEBUG") != null;
		var i = new Installer(debug);
		if( !i.checkRights() )
			return;
		wnd = new xcross.Winlog("haXe Installer");
		wnd.button = "Exit";
		wnd.enabled = false;
		wnd.onClick = function() {
			xcross.Api.stop();
		};
		neko.vm.Thread.create(i.run);
		xcross.Api.loop();
	}

}

// --------- TOOLS --------------

class Progress extends neko.io.Output {

	var o : neko.io.Output;
	public var cur : Int;
	public var max : Int;

	public function new(o) {
		this.o = o;
		cur = 0;
	}

	public function update() {
	}

	public override function writeChar(c) {
		o.writeChar(c);
		cur++;
		update();
	}

	public override function writeBytes(s,p,l) {
		var r = o.writeBytes(s,p,l);
		cur += r;
		update();
		return r;
	}

	public override function close() {
		super.close();
		o.close();
	}

	public override function prepare(m) {
		max = m;
	}

}
