#- global BINARYs

BUILDDIR = build

VPATH=.:grammar:$(BUILDDIR)

MODULES = lexer parser translater auxiliary satsolver

BINARY = satsolver

OCAMLC = ocamlc
CMOX = cmo

.PHONY: all test opt clean

#- arguments
compile: $(BINARY)

test:
	make
	./$(BINARY)

opt:
	make OCAMLC=ocamlopt CMOX=cmx BUILDDIR=buildopt BINARY=satsolver-opt

clean:
	rm -f lexer.ml parser.ml parser.mli
	rm -fr build buildopt
	rm -f satsolver satsolver-opt

$(BINARY): $(MODULES:%=$(BUILDDIR)/%.$(CMOX)) $(BUILDDIR)/main.$(CMOX)
	$(OCAMLC) -o $@ $^



#- dependency graph
.PRECIOUS: lexer.ml parser.mli

$(BUILDDIR)/lexer.$(CMOX): $(BUILDDIR)/parser.cmi
$(BUILDDIR)/translater.$(CMOX): $(BUILDDIR)/parser.cmi 
$(BUILDDIR)/satsolver.$(CMOX): $(BUILDDIR)/auxiliary.cmi
$(BUILDDIR)/main.$(CMOX): $(BUILDDIR)/translater.cmi
$(BUILDDIR)/main.$(CMOX): $(BUILDDIR)/satsolver.cmi 




#- generic rules

%.ml: %.mll 
	ocamllex -o $@ $<

%.mli %.ml: %.mly 
	ocamlyacc -b $* $<

$(BUILDDIR)/%.cmi: %.mli | $(BUILDDIR)
	$(OCAMLC) -o $@ -c $< -I $(BUILDDIR)

$(BUILDDIR)/%.$(CMOX) $(BUILDDIR)/%.cmi: %.ml | $(BUILDDIR)
	$(OCAMLC) -o $@ -c $< -I $(BUILDDIR)


$(BUILDDIR):
	mkdir -p $@
