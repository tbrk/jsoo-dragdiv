
dragdiv.cmo: dragdiv.ml dragdiv.cmi
	ocamlfind ocamlc -package js_of_ocaml \
			 -package js_of_ocaml.ppx \
			 -linkpkg \
			 -c -o $@ $<

dragdiv.cmi: dragdiv.mli
	ocamlfind ocamlc -package js_of_ocaml $<

clean:
	-@rm -rf dragdiv.cmi dragdiv.cmo

