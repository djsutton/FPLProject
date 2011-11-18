# CSCI 5535 - Homework 3 Makefile
#
# Based on code by Wes Weimer.
# 
# if "make" or "gmake" doesn't work for you for some reason, you can
# compile manually like so:
#
# ocamlopt -o lambdac ast.cmx eval.cmx parse.cmx lex.cmx main.cmx 
#
# When you're feeling lucky, try "make test" 

OCAMLOPT  = ocamlopt
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

all : lambdac

hello: hello.cmx
	$(OCAMLOPT) -o hello hello.cmx
	./hello

FUN_OBJS = \
        ast.cmx \
        eval.cmx \
        parse.cmx \
        lex.cmx \
        main.cmx 

clean : 
	$(RM) -f *.cmi *.cmx *.o *.cmo *~ lex.ml parse.ml parse.mli lambdac lambdac.exe test-result test-answer hello hello.exe

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

.PHONY: test1 test2 test3 test4 test5 test6 test7

test: all test1 test2 test3 test4 test5 test6 test7

TEST1 = "'+(0,1)'" 
test1:
	@echo "1" > test-answer
	@echo "$(TEST1)" | ./lambdac --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test1 passed" ; else echo "*** test1 FAILED: $(TEST1)" ; fi 

TEST2 = "'=(0,0)'" 
test2:
	@echo "true" > test-answer
	@echo "$(TEST2)" | ./lambdac --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test2 passed" ; else echo "*** test2 FAILED: $(TEST2)" ; fi 

TEST3 = "'(lambda x . x) 42'" 
test3:
	@echo "42" > test-answer
	@echo "$(TEST3)" | ./lambdac --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test3 passed" ; else echo "*** test3 FAILED: $(TEST3)" ; fi 

TEST4 = "'letrec f = lambda x. (if (equal x 0) then 0 else f (pred x)) in f 10'" 
test4:
	@echo "0" > test-answer
	@echo "$(TEST4)" | ./lambdac --silent > test-result
	@if diff -b -w test-result test-answer ; then echo "*** test4 passed" ; else echo "*** test4 FAILED: $(TEST4)" ; fi 


# Your project will be graded using: 
#
#	./lambdac --silent < example.lambdac > test-result
#       diff -b -w test-result test-answer ; then echo "*** passed" ; else
#        echo "*** failed" ; fi 
#
# ... using the "example.lambdac"s submitted in by yourself and others. 
