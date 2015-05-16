(import scheme chicken)
(use srfi-1 srfi-13 posix test)
(parameterize ((current-directory ".."))
  (load "rope.scm")
  (load "rope-chicken.scm")
  (import rope))

(define t rope->tree)
(define s rope->string)
(define r
  (lambda (o)
    (if (list? o) 
        (tree->rope o)
        (string->rope o))))

(test-group "rope"
  (test-error (rope 1))
  (test #t (rope? (rope "yes")))
  (test 9  (rope-length (rope "yes" "yes" "yes"))))

(test-group "rope?"
  (test #f (rope? "no"))
  (test #t (rope? (r "yes")))
  (test #t (rope? (r '("yes" "yes")))))

(test-group "rope-depth"
  (test 0 (rope-depth (r "abc")))
  (test 1 (rope-depth (r '("a" "b"))))
  (test 2 (rope-depth (r '(("a" "b") ("c" "d"))))))

(test-group "rope-length"
  (test #t (rope-null? (r "")))
  (test 0  (rope-length (r "")))
  (test 3  (rope-length (r "abc")))
  (test 6  (rope-length (r '("abc" "def")))))

(test-group "read-rope"
  (test #t (rope? (with-input-from-string "abc" read-rope)))
  (test 3  (rope-length (call-with-input-string "abc" read-rope)))
  (test 1  (rope-length (call-with-input-string "abc"
                          (lambda (p) (read-rope p 1))))))

(test-group "rope=?"
  (test #t (rope=? empty-rope (r "")))
  (test #t (rope=? (r "abc") (r "abc")))
  (test #f (rope=? (r "abc") (r "def")))
  (test #t (rope=? (r "abc") (r '(("a" "b") "c"))))
  (test #f (rope=? (r "abc") (r '(("c" "b") "a"))))
  (test #t (rope=? (with-input-from-file "run.scm" read-rope)
                   (with-input-from-file "run.scm" read-rope))))

(test-group "rope-append"
  (test ""     (s (rope-append)))
  (test "a"    (s (rope-append (r "a"))))
  (test "ab"   (s (rope-append (r "a") (r "b"))))
  (test "abc"  (s (rope-append (r "a") (r "b") (r "c"))))
  (test "abcd" (s (rope-append (r '("a" "b")) (r '("c" "d")))))
  (test "ab"        (t (rope-append (r "a") (r "b"))))
  (test '("a" "bc") (t (rope-append (r '("a" "b")) (r "c")))))

(test-group "rope-concatenate"
  (test ""     (s (rope-concatenate '())))
  (test "a"    (s (rope-concatenate (list (r "a")))))
  (test "ab"   (s (rope-concatenate (list (r "a") (r "b")))))
  (test "abc"  (s (rope-concatenate (list (r "a") (r "b") (r "c")))))
  (test "abcd" (s (rope-concatenate (list (r '("a" "b")) (r '("c" "d")))))))

(test-group "rope-ref"
  (test #\a   (rope-ref (r "abc") 0))
  (test #\e   (rope-ref (r '(("a" "b") ("c" ("d" "e")))) 4))
  (test-error (rope-ref (r "abc") -1))
  (test-error (rope-ref (r "abc") 3)))

(test-group "subrope"
  (test "a"   (s (subrope (r "abc") 0 1)))
  (test "b"   (s (subrope (r "abc") 1 2)))
  (test "bc"  (s (subrope (r "abc") 1 3)))
  (test "bc"  (s (subrope (r '("a" ("b" "c"))) 1 3)))
  (test "bc"  (s (subrope (r '(("a" "b") ("c" "d"))) 1 3)))
  (test-error (s (subrope (r "abc") -1)))
  (test-error (s (subrope (r "abc") 1 4)))
  (test-error (s (subrope (r '("a" ("b" "c"))) 1 4))))

(test-group "rope-reverse"
  (test "cba" (s (rope-reverse (r "abc"))))
  (test "cba" (s (rope-reverse (r '("a" ("b" "c")))))))

(test-group "rope-fold"
  (test (reverse '(#\a #\b #\c))
        (rope-fold cons '() (r '("a" ("b" "c"))))))

(test-group "rope-for-each"
  (test "abc" (with-output-to-string
                (lambda ()
                  (rope-for-each display (r "abc"))))))

(test-group "rope-balanced?"
  (test #t (rope-balanced? (r "abc")))
  (test #t (rope-balanced? (r '("a" ("b" "c")))))
  (test #f (rope-balanced? (r '("a" ("b" ("c" "d"))))))
  (test #t (rope-balanced? (with-input-from-file "run.scm" read-rope))))

(test-group "rope-balance"
  (test "" (t (rope-balance (r ""))))
  (test "abc" (t (rope-balance (r "abc"))))
  (test '("ab" "c") (t (rope-balance (r '("ab" "c")))))
  (test '("abc" ("d" "e")) (t (rope-balance (r '(("abc" "d") "e")))))
  (test '(("a" "bc") ("d" "ef")) (t (rope-balance (r '("a" ("bc" ("d" "ef")))))))
  (test '((("a" "b") ("c" "d")) (("e" "f") "g"))
        (t (rope-balance (r '((("a" ("b" "c")) "d") ("e" ("f" "g"))))))))

(test-group "make-rope-iterator"
  (let ((next (make-rope-iterator (r '("a" ("b" "c"))))))
    (test #\a   (next))
    (test #\b   (next))
    (test #\c   (next))
    (test #!eof (next))))

(test-group "open-input-rope"
  (with-input-from-port (open-input-rope (r '("a" ("b" "c"))))
    (lambda ()
      (test #\a   (read-char))
      (test "bc"  (read-string))
      (test #!eof (read-char)))))

(test-group "open-output-rope"
  (let ((output-rope
          (with-output-to-port (open-output-rope)
            (lambda ()
              (display "abc")
              (get-output-rope)))))
    (test #t    (rope? output-rope))
    (test "abc" (s output-rope))))

(test-exit)
