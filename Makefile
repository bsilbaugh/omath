
BUILD = _build

omath: .FORCE
	ocamlbuild omath.cma

install:
	ocamlfind install omath $(BUILD)/*.cma $(BUILD)/*.cmi META

.FORCE:
