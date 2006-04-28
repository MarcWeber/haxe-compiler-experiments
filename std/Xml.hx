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

enum XmlType {
}

extern class Xml {

	static property Node(default,null) : XmlType;
	static property PCData(default,null) : XmlType;
	static property CData(default,null) : XmlType;
	static property Comment(default,null) : XmlType;
	static property DocType(default,null) : XmlType;
	static property Prolog(default,null) : XmlType;
	static property Document(default,null) : XmlType;

	static function parse( s : String ) : Xml;

	static function createNode( name : String ) : Xml;
	static function createPCData( data : String ) : Xml;
	static function createCData( data : String ) : Xml;
	static function createComment( data : String ) : Xml;
	static function createDocType( data : String ) : Xml;
	static function createProlog( data : String ) : Xml;
	static function createDocument() : Xml;

	property nodeType(default,null) : XmlType;

	// nodeName : only works for Node
	property nodeName(getNodeName,setNodeName) : String;
	private function getNodeName() : String;
	private function setNodeName( name : String ) : String;

	// nodeValue : only works for not Node and not Document
	property nodeValue(getNodeValue,setNodeValue) : String;
	private function getNodeValue() : String;
	private function setNodeValue( name : String ) : String;

	// attributes : only works for Node
	function get( att : String ) : String; // check case insensitivy
	function set( att : String, value : String ) : Void;
	function remove( att : String ) : Void;
	function exists( att : String ) : Bool;
	function attributes() : Iterator<String>;

	// children method : only works for Node and Document
	// exception if child is Document (can't add Documents together)
	function iterator() : Iterator<Xml>; // all children
	function nodes() : Iterator<Xml>; // only nodes
	function nodesNamed( name : String ) : Iterator<Xml>; // only nodes with this nodeName
	function firstChild() : Xml;
	function firstNode() : Xml;
	function addChild( x : Xml ) : Void;
	function removeChild( x : Xml ) : Bool;
	function insertChild( x : Xml, pos : Int ) : Void;

	function toString() : String;

	static function __init__() : Void untyped {
		Node = "node";
		PCData = "pcdata";
		CData = "cdata";
		Comment = "comment";
		Doctype = "doctype";
		Prolog = "prolog";
		Document = "document";
	}

}
