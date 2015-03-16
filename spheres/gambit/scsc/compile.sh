#!/usr/bin/env sh
gsi -f boot.scm
gsc -f -e "(parameterize ((current-readtable (readtable-sharing-allowed?-set (current-readtable) 'serialize))) (compile-file \"syntax-case\"))"
cp syntax-case.o1 ..
