/*
 * Copyright (c) 2005, The haXe Project Contributors
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
 * THIS SOFTWARE IS PROVIDED BY THE HAXE PROJECT CONTRIBUTORS "AS IS" AND ANY
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
package neko.net;

private typedef ThreadInfos = {
	var id : Int;
	var t : neko.vm.Thread;
	var socks : Array<neko.net.Socket>;
}

private typedef ClientInfos<Client> = {
	var client : Client;
	var sock : neko.net.Socket;
	var thread : ThreadInfos;
	var buf : String;
	var bufpos : Int;
}

class ThreadServer<Client,Message> {

	var threads : Array<ThreadInfos>;
	var sock : neko.net.Socket;
	var worker : neko.vm.Thread;
	var timer : neko.vm.Thread;
	public var listen : Int;
	public var nthreads : Int;
	public var connectLag : Float;
	public var errorOutput : neko.io.Output;
	public var initialBufferSize : Int;
	public var maxBufferSize : Int;
	public var messageHeaderSize : Int;
	public var updateTime : Float;

	public function new() {
		threads = new Array();
		nthreads = 10;
		messageHeaderSize = 1;
		listen = 10;
		connectLag = 0.5;
		errorOutput = neko.io.File.stderr();
		initialBufferSize = (1 << 10);
		maxBufferSize = (1 << 16);
		updateTime = 1;
	}

	function runThread(t) {
		while( true ) {
			try {
				loopThread(t);
			} catch( e : Dynamic ) {
				logError(e);
			}
		}
	}

	function readClientData( c : ClientInfos<Client> ) {
		var available = c.buf.length - c.bufpos;
		if( available == 0 ) {
			var newsize = c.buf.length * 2;
			if( newsize > maxBufferSize ) {
				newsize = maxBufferSize;
				if( c.buf.length == maxBufferSize )
					throw "Max buffer size reached";
			}
			var newbuf = neko.Lib.makeString(newsize);
			neko.Lib.copyBytes(newbuf,0,c.buf,0,c.bufpos);
			c.buf = newbuf;
			available = newsize - c.bufpos;
		}
		var bytes = c.sock.input.readBytes(c.buf,c.bufpos,available);
		var pos = 0;
		var len = c.bufpos + bytes;
		while( len >= messageHeaderSize ) {
			var m = readClientMessage(c.client,c.buf,pos,len);
			if( m == null )
				break;
			pos += m.bytes;
			len -= m.bytes;
			work(callback(clientMessage,c.client,m.msg));
		}
		if( pos > 0 )
			neko.Lib.copyBytes(c.buf,0,c.buf,pos,len);
		c.bufpos = len;
	}

	function loopThread( t : ThreadInfos ) {
		if( t.socks.length > 0 )
			for( s in neko.net.Socket.select(t.socks,null,null,connectLag).read ) {
				var infos : ClientInfos<Client> = s.custom;
				try {
					readClientData(infos);
				} catch( e : Dynamic ) {
					t.socks.remove(s);
					if( !Std.is(e,neko.io.Eof) )
						logError(e);
					work(callback(clientDisconnected,infos.client));
				}
			}
		while( true ) {
			var s = neko.vm.Thread.readMessage(t.socks.length == 0);
			if( s == null )
				break;
			t.socks.push(s);
		}
	}

	function runWorker() {
		while( true ) {
			var f = neko.vm.Thread.readMessage(true);
			try {
				f();
			} catch( e : Dynamic ) {
				logError(e);
			}
		}
	}

	function work( f : Void -> Void ) {
		worker.sendMessage(f);
	}

	public function onError( e : Dynamic, stack ) {
		var estr = try Std.string(e) catch( e2 : Dynamic ) "???" + try "["+Std.string(e2)+"]" catch( e : Dynamic ) "";
		errorOutput.write( estr + "\n" + haxe.Stack.toString(stack) );
		errorOutput.flush();
	}

	function logError( e : Dynamic ) {
		var stack = haxe.Stack.exceptionStack();
		if( neko.vm.Thread.current() == worker )
			onError(e,stack);
		else
			work(callback(onError,e,stack));
	}

	function addClient( sock : neko.net.Socket ) {
		var infos : ClientInfos<Client> = {
			thread : threads[Std.random(nthreads)],
			client : clientConnected(sock),
			sock : sock,
			buf : neko.Lib.makeString(initialBufferSize),
			bufpos : 0,
		};
		sock.custom = infos;
		infos.thread.t.sendMessage(sock);
	}

	function runTimer() {
		var l = new neko.vm.Lock();
		while( true ) {
			l.wait(updateTime);
			work(update);
		}
	}

	public function run( host, port ) {
		sock = new neko.net.Socket();
		sock.bind(new neko.net.Host(host),port);
		sock.listen(listen);
		worker = neko.vm.Thread.create(runWorker);
		timer = neko.vm.Thread.create(runTimer);
		for( i in 0...nthreads ) {
			var t = { id : i, t : null, socks : new Array() };
			threads.push(t);
			t.t = neko.vm.Thread.create(callback(runThread,t));
		}
		while( true ) {
			try {
				var cl = sock.accept();
				cl.setBlocking(false);
				work(callback(addClient,cl));
			} catch( e : Dynamic ) {
				logError(e);
			}
		}
	}

	public function sendData( s : neko.net.Socket, data : String ) {
		try {
			s.write(data);
		} catch( e : Dynamic ) {
			try s.close() catch( e : Dynamic ) { };
		}
	}

	// --- CUSTOMIZABLE API ---

	public function clientConnected( s : neko.net.Socket ) : Client {
		return null;
	}

	public function clientDisconnected( c : Client ) {
	}

	public function readClientMessage( c : Client, buf : String, pos : Int, len : Int ) : { msg : Message, bytes : Int } {
		return {
			msg : null,
			bytes : len,
		};
	}

	public function clientMessage( c : Client, msg : Message ) {
	}

	public function update() {
	}

}
