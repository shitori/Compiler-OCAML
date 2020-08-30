#!/bin/bash
ocamlc -c AST.ml
ocamllex lexeur.mll
ocamlyacc parseur.mly
ocamlc -c parseur.mli
ocamlc -c lexeur.ml
ocamlc -c parseur.ml
ocamlc -c interpreteur.ml
ocamlc -o start AST.cmo lexeur.cmo parseur.cmo interpreteur.cmo
