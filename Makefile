# Makefile generated by OCamake 
# http://tech.motion-twin.com
.SUFFIXES : .ml .mli .cmo .cmi .cmx .mll .mly

CFLAGS=-c -annot -cclib -fno-stack-protector  -pp camlp4o -I ocaml/extc -I ocaml/xml-light -I ocaml/swflib -I ocaml -I neko/libs/include/ocaml -I .
LIBS=
LFLAGS= -o bin/haxe -I ocaml/extc -I ocaml/xml-light -I ocaml/swflib -I ocaml -I neko/libs/include/ocaml -I .

all: bin/haxe

bin/haxe: ast.cmx neko/libs/include/ocaml/nast.cmx lexer.cmx type.cmx neko/libs/include/ocaml/binast.cmx neko/libs/include/ocaml/nxml.cmx common.cmx parser.cmx typecore.cmx genxml.cmx typeload.cmx optimizer.cmx codegen.cmx genneko.cmx genas3.cmx genjs.cmx genswf8.cmx genphp.cmx gencpp.cmx genswf9.cmx interp.cmx genswf.cmx typer.cmx main.cmx
	ocamlopt $(LFLAGS) $(LIBS) ast.cmx neko/libs/include/ocaml/nast.cmx lexer.cmx type.cmx neko/libs/include/ocaml/binast.cmx neko/libs/include/ocaml/nxml.cmx common.cmx parser.cmx typecore.cmx genxml.cmx typeload.cmx optimizer.cmx codegen.cmx genneko.cmx genas3.cmx genjs.cmx genswf8.cmx genphp.cmx gencpp.cmx genswf9.cmx interp.cmx genswf.cmx typer.cmx main.cmx

lexer.cmx: ast.cmx

type.cmx: ast.cmx

common.cmx: type.cmx ocaml/swflib/swf.cmx lexer.cmx ocaml/extc/extc.cmx ast.cmx ocaml/swflib/as3hl.cmi

parser.cmx: lexer.cmx common.cmx ast.cmx

typecore.cmx: type.cmx common.cmx ast.cmx

genxml.cmx: type.cmx lexer.cmx common.cmx ast.cmx

typeload.cmx: typecore.cmx type.cmx parser.cmx common.cmx ast.cmx

codegen.cmx: ocaml/xml-light/xml.cmi typeload.cmx typecore.cmx type.cmx genxml.cmx common.cmx ast.cmx

optimizer.cmx: typecore.cmx type.cmx parser.cmx common.cmx ast.cmx

neko/libs/include/ocaml/binast.cmx: neko/libs/include/ocaml/nast.cmx

neko/libs/include/ocaml/nxml.cmx: neko/libs/include/ocaml/nast.cmx

genneko.cmx: type.cmx neko/libs/include/ocaml/nxml.cmx neko/libs/include/ocaml/nast.cmx lexer.cmx common.cmx codegen.cmx neko/libs/include/ocaml/binast.cmx ast.cmx

genas3.cmx: type.cmx ocaml/swflib/swfParser.cmx ocaml/swflib/swf.cmx common.cmx codegen.cmx ast.cmx ocaml/swflib/as3parse.cmx ocaml/swflib/as3code.cmx ocaml/swflib/as3.cmi

genjs.cmx: type.cmx lexer.cmx common.cmx codegen.cmx ast.cmx

genswf8.cmx: type.cmx ocaml/swflib/swf.cmx lexer.cmx common.cmx codegen.cmx ast.cmx ocaml/swflib/actionScript.cmx

genswf9.cmx: type.cmx lexer.cmx genswf8.cmx common.cmx codegen.cmx ast.cmx ocaml/swflib/as3hlparse.cmx ocaml/swflib/as3hl.cmi ocaml/swflib/as3.cmi

genswf.cmx: ocaml/xml-light/xml.cmi type.cmx ocaml/swflib/swfParser.cmx ocaml/swflib/swf.cmx genswf9.cmx genswf8.cmx ocaml/extc/extc.cmx common.cmx ast.cmx ocaml/swflib/as3hlparse.cmx ocaml/swflib/as3hl.cmi ocaml/swflib/as3.cmi

genphp.cmx: type.cmx lexer.cmx common.cmx codegen.cmx ast.cmx

gencpp.cmx: type.cmx lexer.cmx common.cmx codegen.cmx ast.cmx

interp.cmx: type.cmx neko/libs/include/ocaml/nast.cmx lexer.cmx genneko.cmx common.cmx ast.cmx

typer.cmx: typeload.cmx typecore.cmx type.cmx parser.cmx optimizer.cmx lexer.cmx interp.cmx genneko.cmx common.cmx codegen.cmx ast.cmx

main.cmx: typer.cmx typeload.cmx typecore.cmx type.cmx parser.cmx optimizer.cmx lexer.cmx interp.cmx genxml.cmx genswf.cmx genphp.cmx genneko.cmx genjs.cmx gencpp.cmx genas3.cmx ocaml/extc/extc.cmx common.cmx codegen.cmx ast.cmx


clean:
	rm -f bin/haxe
	rm -f main.o main.cmx main.cmi typer.o typer.cmx typer.cmi interp.o interp.cmx interp.cmi gencpp.o gencpp.cmx gencpp.cmi genphp.o genphp.cmx genphp.cmi genswf.o genswf.cmx genswf.cmi genswf9.o genswf9.cmx genswf9.cmi genswf8.o genswf8.cmx genswf8.cmi genjs.o genjs.cmx genjs.cmi genas3.o genas3.cmx genas3.cmi genneko.o genneko.cmx genneko.cmi neko/libs/include/ocaml/nxml.o neko/libs/include/ocaml/nxml.cmx neko/libs/include/ocaml/nxml.cmi neko/libs/include/ocaml/binast.o neko/libs/include/ocaml/binast.cmx neko/libs/include/ocaml/binast.cmi neko/libs/include/ocaml/nast.o neko/libs/include/ocaml/nast.cmx neko/libs/include/ocaml/nast.cmi optimizer.o optimizer.cmx optimizer.cmi codegen.o codegen.cmx codegen.cmi typeload.o typeload.cmx typeload.cmi genxml.o genxml.cmx genxml.cmi typecore.o typecore.cmx typecore.cmi parser.o parser.cmx parser.cmi common.o common.cmx common.cmi type.o type.cmx type.cmi lexer.o lexer.cmx lexer.cmi ast.o ast.cmx ast.cmi

wclean:
	-@del bin/haxe 2>NUL
	-@del main.o main.cmx main.cmi typer.o typer.cmx typer.cmi interp.o interp.cmx interp.cmi gencpp.o gencpp.cmx gencpp.cmi genphp.o genphp.cmx genphp.cmi genswf.o genswf.cmx genswf.cmi genswf9.o genswf9.cmx genswf9.cmi genswf8.o genswf8.cmx genswf8.cmi genjs.o genjs.cmx genjs.cmi genas3.o genas3.cmx genas3.cmi genneko.o genneko.cmx genneko.cmi neko/libs/include/ocaml/nxml.o neko/libs/include/ocaml/nxml.cmx neko/libs/include/ocaml/nxml.cmi neko/libs/include/ocaml/binast.o neko/libs/include/ocaml/binast.cmx neko/libs/include/ocaml/binast.cmi neko/libs/include/ocaml/nast.o neko/libs/include/ocaml/nast.cmx neko/libs/include/ocaml/nast.cmi optimizer.o optimizer.cmx optimizer.cmi codegen.o codegen.cmx codegen.cmi typeload.o typeload.cmx typeload.cmi genxml.o genxml.cmx genxml.cmi typecore.o typecore.cmx typecore.cmi parser.o parser.cmx parser.cmi common.o common.cmx common.cmi type.o type.cmx type.cmi lexer.o lexer.cmx lexer.cmi ast.o ast.cmx ast.cmi 2>NUL

# SUFFIXES
.ml.cmo:
	ocamlc $(CFLAGS) -c $<

.ml.cmx:
	ocamlopt $(CFLAGS) -c $<

.mli.cmi:
	ocamlc $(CFLAGS) $<

.mll.ml:
	ocamllex $<

.mly.ml:
	ocamlyacc $<

ocaml_xml_light = ocaml/xml-light/xml_parser.cmx ocaml/xml-light/xml_lexer.cmx ocaml/xml-light/dtd.cmx ocaml/xml-light/xmlParser.cmx ocaml/xml-light/xml.cmx
ocaml_swf_lib = ocaml/swflib/swf.cmx  ocaml/swflib/actionScript.cmx ocaml/swflib/as3code.cmx ocaml/swflib/as3parse.cmx ocaml/swflib/as3hlparse.cmx ocaml/swflib/swfParser.cmx 
LIBS := $(LIBS) /nix/store/hq9rhjaawnvxh4x7cxw4xyjsfpldnkvq-zlib-1.2.5/lib/libz.a ocaml/extc/extc.cmxa unix.cmxa str.cmxa ./ocaml/extLib.cmxa ocaml/extc/extc.cmx $(ocaml_swf_lib) $(ocaml_xml_light) 
LFLAGS := $(LFLAGS)
