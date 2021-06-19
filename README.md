# ocamlJson

A toy JSON parser for Ocaml using dune, ocamlyacc and ocamllex.

### Usage

Json.parse takes a string and returns a JSON value. JSON values are a variant of Number, Bool, String, Null, Array, or Object, covering all possible JSON values. Not all string escape sequences, unicode escapes or exponent numbers are supported.

### Tests

`$ dune runtest`
