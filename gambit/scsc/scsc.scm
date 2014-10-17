(parameterize ((current-directory (getenv "scsc_home" "."))
               (current-readtable (readtable-sharing-allowed?-set (current-readtable) 'serialize)))
  (load "syntax-case")
  (load "module"))
