(lang dune 3.13)

(name stringx)

(package
 (name stringx)
 (synopsis
  "A collection of string utilities missing from OCaml's standard library")
 (description
  "stringx is a pure OCaml library that provides string manipulation functions commonly found in other languages, but absent from OCaml's standard library. Useful for text processing, formatting, and other everyday needs.")
 (depends "ocaml" "dune" "uutf" "alcotest")
 (license MIT)
 (authors "Naohiro CHIKAMATSU")
 (maintainers "Naohiro CHIKAMATSU <n.chika156@gmail.com>")
 (homepage "https://github.com/nao1215/stringx"))
