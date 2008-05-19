package unit;

class TestRemoting extends Test {

	static var _ = init();
	static var ecnx : haxe.remoting.ExternalConnection;
	static var ecnx2 : haxe.remoting.ExternalConnection;
	static var ecnx3 : haxe.remoting.ExternalConnection;
	static var lcnx : haxe.remoting.LocalConnection;
	static var fjscnx : haxe.remoting.FlashJsConnection;

	static function init() {
		var ctx = RemotingApi.context();
		#if flash
		ecnx = haxe.remoting.ExternalConnection.jsConnect("cnx",ctx);
		ecnx3 = haxe.remoting.ExternalConnection.jsConnect("unknown",ctx);
		lcnx = haxe.remoting.LocalConnection.connect("local",ctx,["dev.unit-tests"]);
		fjscnx = haxe.remoting.FlashJsConnection.connect(#if flash9 "haxeFlash8" #else "haxeFlash9" #end,"cnx",ctx);
		#elseif js
		ecnx = haxe.remoting.ExternalConnection.flashConnect("cnx","haxeFlash8",ctx);
		ecnx2 = haxe.remoting.ExternalConnection.flashConnect("cnx","haxeFlash9",ctx);
		ecnx3 = haxe.remoting.ExternalConnection.flashConnect("nothing","haxeFlash8",ctx);
		#end
	}

	public function test() {
		#if (flash || js)
		doTestConnection(ecnx);
		#end
		exc(function() ecnx3.api.add.call([1,3]));
		#if js
		doTestConnection(ecnx2);
		#end
		#if flash
		doTestAsyncConnection(lcnx);
		doTestAsyncConnection(fjscnx);
		#end
		#if (js || neko)
		var hcnx = haxe.remoting.HttpConnection.urlConnect("http://dev.unit-tests/remoting.n");
		doTestConnection(hcnx);
		var dcnx = haxe.remoting.AsyncDebugConnection.create(haxe.remoting.AsyncAdapter.create(hcnx));
		dcnx.setErrorDebug(function(path,args,e) {});
		dcnx.setResultDebug(function(path,args,ret) {});
		dcnx.setCallDebug(function(path,args) {});
		doTestAsyncConnection(dcnx);
		#end
		var hcnx = haxe.remoting.HttpAsyncConnection.urlConnect("http://dev.unit-tests/remoting.n");
		doTestAsyncConnection(hcnx);
		var dcnx = haxe.remoting.DelayedConnection.create();
		dcnx.connection = hcnx;
		doTestAsyncConnection(dcnx);
	}

	function doTestConnection( cnx : haxe.remoting.Connection ) {
		eq( cnx.api.add.call([1,2]), 3 );
		var strings = ["bla","\n","\r","\n\r","\t","    "," ","&","<",">","&nbsp;","&gt;","<br/>"];
		for( s in strings ) {
			infos("using "+s);
			eq( cnx.api.id.call([s]), s );
			eq( cnx.api.arr.call([[s,s,s]]), [s,s,s].join("#") );
		}
		infos(null);
		eq( cnx.api.exc.call([null]), null );
		exc( function() cnx.api.exc.call([5]) );

		exc( function() cnx.api.call([]) );
		exc( function() cnx.call([]) );

		exc( function() cnx.api.unknown.call([]) );
		exc( function() cnx.api.sub.add.call([1,2]) );
		eq( cnx.apirec.sub.add.call([1,2]), 3 );
	}

	function doTestAsyncConnection( cnx : haxe.remoting.AsyncConnection ) {
		var asyncExc = callback(asyncExc,cnx.setErrorHandler);

		async( cnx.api.add.call, [1,2], 3 );
		var strings = ["bla","\n","\r","\n\r","\t","    "," ","&","<",">","&nbsp;","&gt;","<br/>"];
		for( s in strings ) {
			async( cnx.api.id.call, [s], s );
			async( cnx.api.arr.call, [[s,s,s]], [s,s,s].join("#") );
		}
		async( cnx.api.exc.call, [null], null );
		asyncExc( cnx.api.exc.call, [5] );

		asyncExc( cnx.api.call, [] );
		asyncExc( cnx.call, [] );

		asyncExc( cnx.api.unknown.call, [] );
		asyncExc( cnx.api.sub.add.call, [1,2] );
		async( cnx.apirec.sub.add.call, [1,2], 3 );
	}

}