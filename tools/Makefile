.PHONY: all clean

JOBS ?= -j 4

all:
	@cd crunch && ocamlbuild -quiet $(JOBS) crunch.native
	@cd mpl && ocamlbuild -quiet $(JOBS) mplc.native
	@cd ocp-pack && ocamlbuild -quiet $(JOBS) pack.native
	@$(MAKE) -C fs

clean:
	@cd crunch && ocamlbuild -clean
	@cd mpl && ocamlbuild -clean
	@cd ocp-pack && ocamlbuild -clean
	@cd fs && $(MAKE) clean
