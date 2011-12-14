OCAMLOPT  = ocamlopt
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

all : lambdac

FUN_OBJS = \
        ast.cmx \
        eval.cmx \
        parse.cmx \
        lex.cmx \
        main.cmx 

clean : 
	$(RM) -f *.cmi *.cmx *.o *.cmo *~ lex.ml parse.ml parse.mli lambdac lambdac.exe

%.cmi: %.mli
	$(OCAMLOPT) -c $<

%.cmx: %.ml 
	$(OCAMLOPT) -c $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.ml: %.mll
	$(OCAMLLEX) $< 

lambdac: $(FUN_OBJS)
	$(OCAMLOPT) -o lambdac $(FUN_OBJS)

eval.cmx : eval.cmi
parse.cmx : parse.cmi parse.ml
main.cmx : eval.cmi eval.ml parse.cmi
