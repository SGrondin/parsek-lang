OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) 

# The list of object files for prog1
MAIN_OBJS=prints.cmo typegraph.cmo typing.cmo exp_parser.cmo exp_lexer.cmo  eval.cmo  parser.cmo lexer.cmo main.cmo 

ocil: .depend $(MAIN_OBJS)
	$(OCAMLC) -o k $(OCAMLFLAGS) $(MAIN_OBJS)

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc -v $<
.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

# Clean up
clean:
	rm -f main
	rm -f *~
	rm -f *.cm[iox]
	rm -f exp_parser.ml parser.ml parser.mli parser.output
	rm -f lexer.ml

# Dependencies - have to do the
# parser.* dependencies by hand
# since they're not seen by 
# make depend 

parser.cmo : parser.cmi
parser.mli : parser.mly
parser.ml : parser.mly

exp_parser.cmo : exp_parser.cmi
exp_parser.mli : exp_parser.mly
exp_parser.ml : exp_parser.mly


.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml *.mly *.mll > .depend

include .depend
