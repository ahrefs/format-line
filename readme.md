This is a formatter for OCaml that only format the inside of a line. It does not
change indentation or add line breaks. It is based on the OCaml lexer and
decides which lexing token should be separated by a space and which should not.

It is meant to be used alongside ocp-indent.

Usage:

For output on stdout:

```
dune exec format-line -- input.ml
```

For output in a file:

```
dune exec bin/main.exe input.ml -o output
```

For formatting the file inline:

```
dune exec bin/main.exe input.ml -i
```
