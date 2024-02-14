(lang dune 3.13)

(name CompilerConstruction)

(generate_opam_files true)

(source
 (github Ofsouzap/CompilerConstruction))

(authors "Ofsouzap")

(license MIT)

(package
 (name CompilerConstruction)
 (synopsis "Compiler construction code stuff")
 (depends ocaml dune alcotest nicelib fmt qcheck qcheck-alcotest))
