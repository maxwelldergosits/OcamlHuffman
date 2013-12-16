
all:
	ocamlc -o compress htree.mli htree.ml unix.cma compress.ml
	ocamlc -o decompress htree.mli htree.ml unix.cma decompress.ml

opt:
	ocamlopt -o compress htree.mli htree.ml unix.cmxa compress.ml
	ocamlopt -o decompress htree.mli htree.ml unix.cmxa decompress.ml

clean:
	rm compress
	rm decompress
	rm *.cmo *.cmi
	rm *.cmx
	rm *.o
