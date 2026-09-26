(lang dune 3.17)
(using melange 0.1)

(name datascript_ocaml)

(package
 (name datascript_ocaml)
 (depends
  (ocaml (>= 5.1.1))
  melange))

(package
 (name datascript-ocaml-native)
 (depends
  (ocaml (>= 5.1.1))
  (datascript_ocaml (= :version))
  lmdb
  sqlite3
  alcotest
  (melange-transit-native (= 0.1.2))
  yojson))

(package
 (name datascript-ocaml-native-lmdb))

(package
 (name datascript-ocaml-native-sqlite))

(package
 (name datascript-ocaml-melange)
 (depends
  (ocaml (>= 5.1.1))
  (datascript_ocaml (= :version))
  melange
  (melange-transit-melange (= 0.1.2))))

(package
 (name datascript-ocaml-melange-sqlite))

(package
 (name datascript-ocaml-lg)
 (synopsis "Typed LG application APIs for DataScript")
 (depends
  (ocaml (>= 5.1.1))
  datascript_ocaml
  lg))
