(include "ffi-types-include.scm")
(include "test-macro.scm")

(c-declare #<<c-declare-end

struct point_s { int x; int y; };
union point_u { int x; int y; };
typedef struct { int x; int y; } point;

typedef struct {
    point p;
    point q;
} segment;

typedef struct {
    segment s;
    segment r;
} segment_pair;

typedef struct node {
   point *p;
   point *q;
} pointer_segment;

/* Utility for lifecycle debugging. */

int is_still_address(unsigned long address) {
    ___rc_header *start, *current;
    start = &___VMSTATE_FROM_PSTATE((___PSTATE))->mem.rc_head_;
    for (current=start->next; current != start; current=current->next) {
        if ((unsigned long)(current + 1) == address) {
            return 1;
        }
    }
    return (unsigned long)(start + 1) == address;
}

c-declare-end
)

; Helper functions for the tests.

(define (address-dead? a)
  (declare (not interrupts-enabled))
  (not ((c-lambda (unsigned-long) bool "is_still_address") a)))

(define (foreign-alive? serial)
  (table-ref ##serial-number-to-object-table serial #f))

; First gc kills the dependent's foreign object and clears its entry in the
; dependents table, which causes decref callbacks to run.  At second gc, the
; C object has no Scheme references and, in the case of an ___alloc_rc'd
; object, its refcount is 0, so it is finally reclaimed.
(define (gc-twice)
  (##gc)
  (##gc))

(define (remove l e)
  (cond
    ((null? l) '())
    ((eqv? (car l) e)
     (cdr l))
    (else
      (cons (car l) (remove (cdr l) e)))))

(define (mappend fn l)
  (if (null? l)
    '()
    (append (fn (car l)) (mappend fn (cdr l)))))

(define (permutations l)
  (cond ((null? l) '())
        ((null? (cdr l)) (list l))
        (else
          (mappend
            (lambda (e)
              (map (lambda (perm) (cons e perm))
                   (permutations (remove l e))))
            l))))


; BEGIN TESTS

(c-struct point_s
  (int x)
  (int y))

(c-union point_u
  (int x)
  (int y))

(c-type point
  (int x)
  (int y))

(c-type segment
  (type point p)
  (type point q))

(c-type segment_pair
  (type segment s)
  (type segment r))

(c-type pointer_segment
  (pointer type point p)
  (pointer type point q))


; Basic struct with primitive accessors and mutators.

(let* ((s (make-point_s))
       (a (foreign-address s)))
  (test-true (point_s? s))
  (test-false (point_u? s))
  (test-false (point? s))
  (test-false (segment? s))
  (test-false (segment_pair? s))
  (test-false (pointer_segment? s))
  (test-equal (foreign-tags s)
              '(|struct point_s| |struct point_s*|)
              "struct tags")
  (point_s-x-set! s 1)
  (point_s-y-set! s 2)
  (test-equal (list (point_s-x s) (point_s-y s)) '(1 2))
  (test-false (address-dead? a))
  (set! s #f)
  (gc-twice)
  (test-true (address-dead? a)))


; Basic union with primitive accessors and mutators.

(let* ((u (make-point_u))
       (a (foreign-address u)))
  (test-false (point_s? u))
  (test-true (point_u? u))
  (test-false (point? u))
  (test-false (segment? u))
  (test-false (segment_pair? u))
  (test-false (pointer_segment? u))
  (test-equal (foreign-tags u)
              '(|union point_u| |union point_u*|)
              "union tags")
  (point_u-x-set! u 3)
  (test-equal (point_u-y u) 3)
  (test-false (address-dead? a))
  (set! u #f)
  (gc-twice)
  (test-true (address-dead? a)))


; Basic struct exposed as opaque type, with primitive accessors and mutators.

(let* ((t (make-point))
       (a (foreign-address t)))
  (test-false (point_s? t))
  (test-false (point_u? t))
  (test-true (point? t))
  (test-false (segment? t))
  (test-false (segment_pair? t))
  (test-false (pointer_segment? t))
  (test-equal (foreign-tags t) '(|point| |point*|) "type tags")
  (point-x-set! t 4)
  (point-y-set! t 5)
  (test-equal (list (point-x t) (point-y t)) '(4 5))
  (test-false (address-dead? a))
  (set! t #f)
  (gc-twice)
  (test-true (address-dead? a)))


; Basic contained structure.

(let* ((s (make-segment))
       (a (foreign-address s))
       (p (segment-p s))
       (other-point (make-point))
       (oa (foreign-address other-point)))
  (test-false (point_s? s))
  (test-false (point_u? s))
  (test-false (point? s))
  (test-true (segment? s))
  (test-false (segment_pair? s))
  (test-false (pointer_segment? s))
  (test-true (point? p))
  (test-equal (foreign-tags p) '(|point| |point*|) "dependent accessor tags")
  (point-x-set! p 0)
  (point-x-set! other-point 6)
  (test-equal (point-x p) 0)
  (segment-p-set! s other-point)
  (test-equal (point-x p) 6 "copy by value")
  (point-x-set! other-point 7)
  (test-equal (point-x p) 6 "copy by value")
  (test-false (address-dead? a))
  (set! s #f)
  (gc-twice)
  (test-false (address-dead? a) "dependent reference keeps root alive")
  (set! p #f)
  (gc-twice)
  (test-true (address-dead? a)))


; Root with two direct dependents and one transitive one.  The only new thing
; to test here is lifecycle management.
;
; No matter in what order we release the direct or transitive references,
; only the last deletion gets the root reclaimed.

(let ((v (make-vector 4))
      (a #f))
  (for-each
    (lambda (permutation)
      (vector-set! v 0 (make-segment_pair))
      (set! a (foreign-address (vector-ref v 0)))
      (vector-set! v 1 (segment_pair-r (vector-ref v 0)))
      (vector-set! v 2 (segment_pair-s (vector-ref v 0)))
      (vector-set! v 3 (segment-q (vector-ref v 2)))
      (for-each
        (lambda (i)
          (gc-twice)
          (test-false (address-dead? a))
          (vector-set! v i #f))
        permutation)
      ; Since we have a chain of up to 3 references, it can take up to
      ; 4 collections to break all the links.
      (gc-twice)
      (gc-twice)
      (test-true (address-dead? a)))
    (permutations '(0 1 2 3))))


; User-defined dependencies; order of deletions doesn't really exercise them.

(let* ((d (make-point))
       (r (make-point))
       (rs (object->serial-number r))
       (ds (object->serial-number d))
       (ra (foreign-address r))
       (da (foreign-address d)))
  (ffi-types#register-dependency! d r)
  (test-true (foreign-alive? ds)
             "dependent foreign alive at beginning")
  (test-true (foreign-alive? rs)
             "root foreign alive at beginning")
  (test-false (address-dead? da)
              "dependent alive at beginning")
  (test-false (address-dead? ra)
              "root alive at beginning")
  (set! d #f)
  (gc-twice)
  (test-false (foreign-alive? ds)
              "dependent foreign dead after killing d")
  (test-true (foreign-alive? rs)
             "root foreign alive after killing d")
  (test-true (address-dead? da)
             "dependent dead after killing d")
  (test-false (address-dead? ra)
              "root alive after killing d")
  (set! r #f)
  (gc-twice)
  (test-false (foreign-alive? ds)
              "dependent foreign dead after killing both")
  (test-false (foreign-alive? rs)
              "root foreign dead after killing both")
  (test-true (address-dead? da)
             "dependent dead after killing both")
  (test-true (address-dead? ra)
             "root dead after killing both"))


; User-defined dependencies; order of deletions exercises them.

(let* ((d (make-point))
       (r (make-point))
       (rs (object->serial-number r))
       (ds (object->serial-number d))
       (ra (foreign-address r))
       (da (foreign-address d)))
  (ffi-types#register-dependency! d r)
  (test-true (foreign-alive? ds)
             "dependent foreign alive at beginning")
  (test-true (foreign-alive? rs)
             "root foreign alive at beginning")
  (test-false (address-dead? da)
              "dependent alive at beginning")
  (test-false (address-dead? ra)
              "root alive at beginning")
  (set! r #f)
  (gc-twice)
  (test-true (foreign-alive? ds)
             "dependent foreign alive after killing r")
  (test-false (foreign-alive? rs)
             "root foreign dead after killing r")
  (test-false (address-dead? da)
              "dependent alive after killing r")
  (test-false (address-dead? ra)
              "root alive after killing r")
  (set! d #f)
  (gc-twice)
  (test-false (foreign-alive? ds)
              "dependent foreign dead after killing both")
  (test-false (foreign-alive? rs)
              "root foreign dead after killing both")
  (test-true (address-dead? da)
             "dependent dead after killing both")
  (test-true (address-dead? ra)
             "root dead after killing both"))


; Pointer accessors and mutators.

(let ((s (make-pointer_segment))
      (p (make-point))
      (q (make-point)))
  (pointer_segment-p-set! s p)
  (pointer_segment-q-set! s q)
  ; We use pointers interchangeably with struct/union/type foreigns.
  (test-true (point? (pointer_segment-p s)))
  ; Mutate p and q _after_ assigning them to s, to test pointer semantics.
  (point-x-set! p 8)
  (point-y-set! p 9)
  (point-x-set! q 10)
  (point-y-set! q 11)
  (test-equal (point-x (pointer_segment-p s)) 8 "assign pointer")
  (test-equal (point-y (pointer_segment-p s)) 9 "assign pointer")
  (test-equal (point-x (pointer_segment-q s)) 10 "assign pointer")
  (test-equal (point-y (pointer_segment-q s)) 11 "assign pointer"))


; Cycle of user-defined dependencies.
(let* ((p (make-point))
       (q (make-point))
       (r (make-point))
       (pa (foreign-address p))
       (qa (foreign-address q))
       (ra (foreign-address r))
       (ps (object->serial-number p))
       (qs (object->serial-number q))
       (rs (object->serial-number r)))
  (ffi-types#register-dependency! p q)
  (ffi-types#register-dependency! q r)
  (ffi-types#register-dependency! r p)
  (set! p #f)
  (gc-twice)
  (test-false (foreign-alive? ps))
  (test-false (address-dead? pa))
  (test-false (address-dead? qa))
  (test-false (address-dead? ra))
  (set! q #f)
  (gc-twice)
  (test-false (foreign-alive? qs))
  (test-false (address-dead? pa))
  (test-false (address-dead? qa))
  (test-false (address-dead? ra))
  (set! r #f)
  (gc-twice)
  (test-false (foreign-alive? rs))
  (test-true (address-dead? pa) "cyclic dependencies get cleared")
  (test-true (address-dead? qa) "cyclic dependencies get cleared")
  (test-true (address-dead? ra) "cyclic dependencies get cleared"))
