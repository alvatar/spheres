#!/usr/bin/env sh
if test -e syntax-case.o1; then rm syntax-case.o1; fi
gsi -f boot.scm
gsc -f -e "(parameterize ((current-readtable (readtable-sharing-allowed?-set (current-readtable) 'serialize))) (compile-file \"syntax-case\"))"
cp syntax-case.o1 ..
