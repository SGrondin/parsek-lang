OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) 

# The list of object files for k
MAIN_OBJS=prints.cmo typegraph.cmo typing.cmo exp_parser.cmo exp_lexer.cmo  eval.cmo  parser.cmo lexer.cmo main.cmo 

# The list of object files for k
MAIN_OBJSOPT=prints.cmx typegraph.cmx typing.cmx exp_parser.cmx exp_lexer.cmx  eval.cmx  parser.cmx lexer.cmx main.cmx 

all: bytecode native

bytecode: .depend $(MAIN_OBJS)
	$(OCAMLC) -o k-bytecode $(OCAMLFLAGS) $(MAIN_OBJS)

native: .depend $(MAIN_OBJSOPT)
	$(OCAMLOPT) -o k-native $(OCAMLOPTFLAGS) $(MAIN_OBJSOPT)

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
	rm -f .depend*
	rm -f *~
	rm -f *.cm[iox] *.o
	rm -f exp_parser.ml parser.ml parser.mli parser.output
	rm -f lexer.ml

# Dependencies - have to do the
# parser.* dependencies by hand
# since they're not seen by 
# make depend 

parser.cmx : parser.cmi
parser.cmo : parser.cmi
parser.mli : parser.mly
parser.ml : parser.mly

exp_parser.cmx : exp_parser.cmi
exp_parser.cmo : exp_parser.cmi
exp_parser.mli : exp_parser.mly
exp_parser.ml : exp_parser.mly


.depend:
	$(OCAMLDEP) -native $(INCLUDES) *.mli *.ml *.mly *.mll > .depend

include .depend
