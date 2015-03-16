(load "expand")
(load "test-lib")


(define (test)

  (test-equal
    (type-definition 'struct 'point)
    '(c-define-type point (struct "point" (|struct point| |struct point*|))))

  (test-equal
    (dependent-type-definition 'struct 'point)
   '(c-define-type dependent-point (struct "point" (|struct point| |struct point*|) "___release_pointer")))

  (test-equal
    (predicate 'struct 'point)
    '(define (point? x)
       (and (foreign? x)
            (memq (car (foreign-tags x)) '(|struct point| |struct point*|))
            #t)))

  (test-equal
    (allocator 'struct 'point)
    '(define (make-point)
       (let ((ret ((c-lambda () point
                     "___result_voidstar = ___EXT(___alloc_rc)(sizeof(struct point));"))))
         (ffi-types#register-rc-root! ret)
         ret)))

  (test-equal
    (primitive-accessor 'struct 'point 'int 'x)
    '(define point-x
       (c-lambda (point) int
         "___result = ((struct point*)___arg1_voidstar)->x;")))

  (test-equal
    (dependent-accessor 'struct 'point 'union 'coord 'x)
    '(define (point-x parent)
       (let ((ret ((c-lambda (point) dependent-coord
                     "___result_voidstar = &((struct point*)___arg1_voidstar)->x;") parent)))
         (ffi-types#register-dependency! ret parent)
         ret)))

  (test-equal
    (pointer-accessor 'struct 'point 'pointer 'union 'coord 'x)
    '(define (point-x parent)
       (let ((ret ((c-lambda (point) dependent-coord "___result_voidstar = ((struct point*)___arg1_voidstar)->x;") parent))) ret)))

  (test-equal
    (pointer-mutator 'struct 'point 'pointer 'union 'coord 'x)
    '(define point-x-set!
       (c-lambda (point coord) void
         "((struct point*)___arg1_voidstar)->x = (union coord*)___arg2_voidstar;"))

    (test-equal
      (primitive-mutator 'struct 'point 'int 'x)
      '(define point-x-set!
         (c-lambda (point int) void
                   "((struct point*)___arg1_voidstar)->x = ___arg2;"))))

  (test-equal
    (dependent-mutator 'struct 'point 'union 'coord 'x)
    '(define point-x-set!
       (c-lambda (point coord) void
         "((struct point*)___arg1_voidstar)->x = *(union coord*)___arg2_voidstar;"))))

(test)
