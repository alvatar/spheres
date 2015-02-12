;;!!! SRFI-51: Handling rest list
;; .author Joo ChurlSoo

(define-library (spheres/core rest-values)
  (export rest-values
          arg-and
          arg-ands
          arg-or
          arg-ors
          err-and
          err-ands
          err-or
          err-ors)

  (import (spheres/algorithm list))
  
  (define-syntax arg-and
    (syntax-rules()
      ((arg-and arg (a1 a2 ...) ...)
       (and (or (symbol? 'arg)
                (error "bad syntax" 'arg '(symbol? 'arg)
                       '(arg-and arg (a1 a2 ...) ...)))
            (or (a1 a2 ...)
                (error "incorrect argument" arg 'arg '(a1 a2 ...)))
            ...))
      ((arg-and caller arg (a1 a2 ...) ...)
       (and (or (symbol? 'arg)
                (error "bad syntax" 'arg '(symbol? 'arg)
                       '(arg-and caller arg (a1 a2 ...) ...)))
            (or (a1 a2 ...)
                (if (string? caller)
                    (error caller arg 'arg '(a1 a2 ...))
                    (error "incorrect argument" arg 'arg '(a1 a2 ...) caller)))
            ...))))

  ;; accessory macro for arg-ands
  (define-syntax %caller-arg-and
    (syntax-rules()
      ((_ caller arg (a1 a2 ...) ...)
       (and (or (symbol? 'arg)
                (error "bad syntax" 'arg '(symbol? 'arg)
                       '(%caller-arg-and caller arg (a1 a2 ...) ...)))
            (or (a1 a2 ...)
                (if (string? caller)
                    (error caller arg 'arg '(a1 a2 ...))
                    (error "incorrect argument" arg 'arg '(a1 a2 ...) caller)))
            ...))
      ((_ null caller arg (a1 a2 ...) ...)
       (and (or (symbol? 'arg)
                (error "bad syntax" 'arg '(symbol? 'arg)
                       '(%caller-arg-and caller arg (a1 a2 ...) ...)))
            (or (a1 a2 ...)
                (if (string? caller)
                    (error caller arg 'arg '(a1 a2 ...))
                    (error "incorrect argument" arg 'arg '(a1 a2 ...) caller)))
            ...))))

  (define-syntax arg-ands
    (syntax-rules (common)
      ((arg-ands (a1 a2 ...) ...)
       (and (arg-and a1 a2 ...) ...))
      ((arg-ands common caller (a1 a2 ...) ...)
       (and (%caller-arg-and caller a1 a2 ...) ...))))

  (define-syntax arg-or
    (syntax-rules()
      ((arg-or arg (a1 a2 ...) ...)
       (or (and (not (symbol? 'arg))
                (error "bad syntax" 'arg '(symbol? 'arg)
                       '(arg-or arg (a1 a2 ...) ...)))
           (and (a1 a2 ...)
                (error "incorrect argument" arg 'arg '(a1 a2 ...)))
           ...))
      ((arg-or caller arg (a1 a2 ...) ...)
       (or (and (not (symbol? 'arg))
                (error "bad syntax" 'arg '(symbol? 'arg)
                       '(arg-or caller arg (a1 a2 ...) ...)))
           (and (a1 a2 ...)
                (if (string? caller)
                    (error caller arg 'arg '(a1 a2 ...))
                    (error "incorrect argument" arg 'arg '(a1 a2 ...) caller)))
           ...))))

  ;; accessory macro for arg-ors
  (define-syntax %caller-arg-or
    (syntax-rules()
      ((_ caller arg (a1 a2 ...) ...)
       (or (and (not (symbol? 'arg))
                (error "bad syntax" 'arg '(symbol? 'arg)
                       '(%caller-arg-or caller arg (a1 a2 ...) ...)))
           (and (a1 a2 ...)
                (if (string? caller)
                    (error caller arg 'arg '(a1 a2 ...))
                    (error "incorrect argument" arg 'arg '(a1 a2 ...) caller)))
           ...))
      ((_ null caller arg (a1 a2 ...) ...)
       (or (and (not (symbol? 'arg))
                (error "bad syntax" 'arg '(symbol? 'arg)
                       '(%caller-arg-or caller arg (a1 a2 ...) ...)))
           (and (a1 a2 ...)
                (if (string? caller)
                    (error caller arg 'arg '(a1 a2 ...))
                    (error "incorrect argument" arg 'arg '(a1 a2 ...) caller)))
           ...))))

  (define-syntax arg-ors
    (syntax-rules (common)
      ((arg-ors (a1 a2 ...) ...)
       (or (arg-or a1 a2 ...) ...))
      ((arg-ors common caller (a1 a2 ...) ...)
       (or (%caller-arg-or caller a1 a2 ...) ...))))

  (define-syntax err-and
    (syntax-rules ()
      ((err-and err expression ...)
       (and (or expression
                (if (string? err)
                    (error err 'expression)
                    (error "false expression" 'expression err)))
            ...))))

  (define-syntax err-ands
    (syntax-rules ()
      ((err-ands (err expression ...)  ...)
       (and (err-and err expression ...)
            ...))))

  (define-syntax err-or
    (syntax-rules ()
      ((err-or err expression ...)
       (or (and expression
                (if (string? err)
                    (error err 'expression)
                    (error "true expression" 'expression err)))
           ...))))

  (define-syntax err-ors
    (syntax-rules ()
      ((err-ors (err expression ...) ...)
       (or (err-or err expression ...)
           ...))))

  (include "rest-values.scm"))
