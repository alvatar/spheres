(define-library (spheres/string diff)
  (export diff:longest-common-subsequence
          diff:edits
          diff:edit-length)

  (include "diff.scm"))
