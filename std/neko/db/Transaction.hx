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
package neko.db;

class Transaction {

	private static function isDeadlock(e) {
		return false;
	}

	private static function runMainLoop(mainFun,logError,count) {
		try {
			mainFun();
		} catch( e : Dynamic ) {
			if( count > 0 && isDeadlock(e) ) {
				Manager.cnx.request("ROLLBACK"); // should be already done, but in case...
				Manager.cnx.request("START TRANSACTION");
				runMainLoop(mainFun,logError,count-1);
				return;
			}
			if( logError == null ) {
				Manager.cnx.request("ROLLBACK");
				neko.Lib.rethrow(e);
			}
			logError(e); // should ROLLBACK if needed
		}
	}

	public static function main( dbparams, dbname, mainFun : Void -> Void, logError : Dynamic -> Void ) {
		Manager.initialize();
		Manager.cnx = Mysql.connect(dbparams);
		Manager.cnx.selectDB(dbname);
		Manager.cnx.request("START TRANSACTION");
		runMainLoop(mainFun,logError,3);
		Manager.cnx.request("COMMIT");
		Manager.cnx.close();
		Manager.cnx = null;
		Manager.cleanup();
	}

}
