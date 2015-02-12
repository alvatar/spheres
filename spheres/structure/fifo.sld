;;!!! FIFO
;; .author Marc Feeley. Copyright (c) 2005-2008 by Marc Feeley, All Rights Reserved.
;; .author Álvaro Castor-Castilla, 2015. Minor modifications and compatibility.

(define-library (spheres/structure fifo)
  (export macro-make-fifo
          macro-fifo-next
          macro-fifo-next-set!
          macro-fifo-tail
          macro-fifo-tail-set!
          macro-fifo-elem
          macro-fifo-elem-set!
          macro-fifo->list
          macro-fifo-remove-all!
          macro-fifo-remove-head!
          macro-fifo-insert-at-head!
          macro-fifo-insert-at-tail!
          macro-fifo-advance!
          fifo->u8vector
          fifo->string)

  (cond-expand
   (gambit
    (define-macro (macro-make-fifo)
      `(let ((fifo (##cons '() '())))
         (macro-fifo-tail-set! fifo fifo)
         fifo))

    (define-macro (macro-fifo-next fifo)
      `(##cdr ,fifo))

    (define-macro (macro-fifo-next-set! fifo x)
      `(##set-cdr! ,fifo ,x))

    (define-macro (macro-fifo-tail fifo)
      `(##car ,fifo))

    (define-macro (macro-fifo-tail-set! fifo x)
      `(##set-car! ,fifo ,x))

    (define-macro (macro-fifo-elem fifo)
      `(##car ,fifo))

    (define-macro (macro-fifo-elem-set! fifo x)
      `(##set-car! ,fifo ,x))

    (define-macro (macro-fifo->list fifo)
      `(macro-fifo-next ,fifo))

    (define-macro (macro-fifo-remove-all! fifo)
      `(let ((fifo ,fifo))
         (##declare (not interrupts-enabled))
         (let ((head (macro-fifo-next fifo)))
           (macro-fifo-tail-set! fifo fifo)
           (macro-fifo-next-set! fifo '())
           head)))

    (define-macro (macro-fifo-remove-head! fifo)
      `(let ((fifo ,fifo))
         (##declare (not interrupts-enabled))
         (let ((head (macro-fifo-next fifo)))
           (if (##pair? head)
               (let ((next (macro-fifo-next head)))
                 (if (##null? next)
                     (macro-fifo-tail-set! fifo fifo))
                 (macro-fifo-next-set! fifo next)
                 (macro-fifo-next-set! head '())))
           head)))

    (define-macro (macro-fifo-insert-at-tail! fifo elem)
      `(let ((fifo ,fifo) (elem ,elem))
         (let ((x (##cons elem '())))
           (##declare (not interrupts-enabled))
           (let ((tail (macro-fifo-tail fifo)))
             (macro-fifo-next-set! tail x)
             (macro-fifo-tail-set! fifo x)
             (##void)))))

    (define-macro (macro-fifo-insert-at-head! fifo elem)
      `(let ((fifo ,fifo) (elem ,elem))
         (let ((x (##cons elem '())))
           (##declare (not interrupts-enabled))
           ;; To obtain an atomic update of the fifo, we must force a
           ;; garbage-collection to occur right away if needed by the
           ;; ##cons, so that any finalization that might mutate this fifo
           ;; will be done before updating the fifo.
           (##check-heap-limit)
           (let ((head (macro-fifo-next fifo)))
             (if (##null? head)
                 (macro-fifo-tail-set! fifo x))
             (macro-fifo-next-set! fifo x)
             (macro-fifo-next-set! x head)
             (##void)))))

    (define-macro (macro-fifo-advance-to-tail! fifo)
      `(let ((fifo ,fifo))
         ;; It is assumed that the fifo contains at least one element
         ;; (i.e. the fifo's tail does not change).
         (let ((new-head (macro-fifo-tail fifo)))
           (macro-fifo-next-set! fifo new-head)
           (macro-fifo-elem new-head))))

    (define-macro (macro-fifo-advance! fifo)
      `(let ((fifo ,fifo))
         ;; It is assumed that the fifo contains at least two elements
         ;; (i.e. the fifo's tail does not change).
         (let* ((head (macro-fifo-next fifo))
                (new-head (macro-fifo-next head)))
           (macro-fifo-next-set! fifo new-head)
           (macro-fifo-elem new-head))))

    (define-macro (fifo->u8vector fifo start end)
      `(##fifo->u8vector ,fifo ,start ,end))

    (define-macro (fifo->string fifo start end)
      `(##fifo->string ,fifo ,start ,end)))
   
   (else
    (include "fifo.scm"))))
