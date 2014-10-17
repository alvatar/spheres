;;;============================================================================

;;; Install the syntax-case expander.

(define c#expand-source
  (lambda (src)
    src))

(set! c#expand-source ;; setup compiler's expander
  (lambda (src)
    ((make-expander '(E) '(E)) src)))

(set! ##expand-source ;; setup interpreter's expander
  (lambda (src)
    (let ((expansion ((make-expander '(E) '(E)) src)))
      (if (syntax-case-debug)
          (pp (##desourcify expansion)))
      (unmark! expansion)
      expansion)))

;;;============================================================================
