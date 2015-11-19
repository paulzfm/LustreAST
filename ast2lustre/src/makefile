COM = ocamlc
LEX = ocamllex
YACC = ocamlyacc

lc : lexer.cmo parser.cmo main.cmo tree.cmo
	$(COM) -o $@ lexer.cmo parser.cmo main.cmo

lexer.cmo : lexer.ml parser.cmi
	$(COM) -c $<

parser.cmo : parser.ml
	$(COM) -c $<

main.cmo : main.ml parser.cmi lexer.cmi
	$(COM) -c $<

tree.cmo : tree.ml
	$(COM) -c $<

parser.cmi : parser.mli
	$(COM) -c $<

lexer.ml : lexer.mll
	$(LEX) $<

parser.ml parser.mli : parser.mly tree.cmo
	$(YACC) $<

.PHONY clean :
	rm *.cmo *.cmi parser.mli parser.ml lexer.ml lc
