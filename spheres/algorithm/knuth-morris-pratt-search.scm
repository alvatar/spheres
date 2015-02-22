;;!!! Knuth-Morris-Pratt Fixed-Pattern Search Algorithm
;;
;; This code is written by Taylor R. Campbell and placed in the Public
;; Domain.  All warranties are disclaimed.

(define-record-type <kmp-cache>
  (make-kmp-cache pattern pattern-start table)
  kmp-cache?
  (pattern kmp-cache/pattern)
  (pattern-start kmp-cache/pattern-start)
  (table kmp-cache/table))

(define (u8vector-forward-search-cache/kmp pattern start end)
  (make-kmp-cache pattern start
                  (compute-kmp-prefix-table pattern start end)))

(define (u8vector-backward-search-cache/kmp pattern start end)
  (make-kmp-cache pattern start
                  (compute-kmp-suffix-table pattern start end)))

(define (u8vector-search-forward/kmp instance instance-start instance-end
                                      cache)
  (kmp-search-forward instance instance-start instance-end
                      (kmp-cache/pattern cache)
                      (kmp-cache/pattern-start cache)
                      (kmp-cache/table cache)))

(define (u8vector-search-backward/kmp instance instance-start instance-end
                                       cache)
  (kmp-search-backward instance instance-start instance-end
                       (kmp-cache/pattern cache)
                       (kmp-cache/pattern-start cache)
                       (kmp-cache/table cache)))

(define (u8vector-search-forward*/kmp instance instance-start instance-end
                                       cache)
  (kmp-search-forward* instance instance-start instance-end
                       (kmp-cache/pattern cache)
                       (kmp-cache/pattern-start cache)
                       (kmp-cache/table cache)))

(define (u8vector-search-backward*/kmp instance instance-start instance-end
                                        cache)
  (kmp-search-backward* instance instance-start instance-end
                        (kmp-cache/pattern cache)
                        (kmp-cache/pattern-start cache)
                        (kmp-cache/table cache)))


;;-------------------------------------------------------------------------------
;;!! KMP Search Utilities

(define (kmp-search-forward instance instance-start instance-end
                            pattern pattern-start
                            prefix-table)
  (and (< instance-start instance-end)
       (let ((partial-result
              (kmp-partial-search-forward instance instance-start instance-end
                                          pattern pattern-start pattern-start
                                          prefix-table)))
         (and (negative? partial-result)
              (- 0
                 (+ (vector-length prefix-table) partial-result))))))

(define (kmp-search-backward instance instance-start instance-end
                             pattern pattern-start
                             suffix-table)
  (and (< instance-start instance-end)
       (let ((partial-result
              (kmp-partial-search-backward instance instance-start instance-end
                                           pattern pattern-start
                                           (+ (vector-length suffix-table)
                                              pattern-start)
                                           suffix-table)))
         (and (not (positive? partial-result))
              (- 0 partial-result)))))

(define (kmp-search-forward* instance instance-start instance-end
                             pattern pattern-start
                             prefix-table)
  (let loop ((instance-start instance-start)
             (occurrences '()))
    (cond ((kmp-search-forward instance instance-start instance-end
                               pattern pattern-start
                               prefix-table)
           => (lambda (occurrence)
                (loop (+ occurrence 1)
                      (cons occurrence occurrences))))
          (else
           (reverse! occurrences)))))

(define (kmp-search-backward* instance instance-start instance-end
                              pattern pattern-start
                              suffix-table)
  (let loop ((instance-end instance-end)
             (occurrences '()))
    (cond ((kmp-search-backward instance instance-start instance-end
                                pattern pattern-start
                                suffix-table)
           => (lambda (occurrence)
                (loop (- (+ occurrence (vector-length suffix-table))
                         1)
                      (cons occurrence occurrences))))
          (else
           (reverse! occurrences)))))


;;-------------------------------------------------------------------------------
;;!! KMP Search Algorithm

;; The basic idea of this algorithm is to step through each unit of
;; the instance, and if we find a mismatch to backtrack in the
;; pattern only as far as we need to, based on the longest prefix
;; (suffix) of the pattern we might have just matched.  See the next
;; page for details of the table and how we compute it.

(define (kmp-partial-search-forward instance instance-start instance-end
                                    pattern pattern-start pattern-index
                                    prefix-table)
  (let* ((pattern-length (vector-length prefix-table))
         (pattern-end (+ pattern-start pattern-length)))
    (let loop ((instance-index instance-start)
               (pattern-index pattern-index))
      (cond ((= pattern-index pattern-end)
             (- 0 instance-index))
            ((= instance-index instance-end)
             pattern-index)
            (else
             (loop (+ instance-index 1)
                   (kmp-step-forward pattern
                                     pattern-start
                                     pattern-index
                                     (u8vector-ref instance instance-index)
                                     prefix-table)))))))

(define (kmp-partial-search-backward instance instance-start instance-end
                                     pattern pattern-start pattern-index
                                     suffix-table)
  (let loop ((instance-index instance-end)
             (pattern-index pattern-index))
    (cond ((= pattern-index pattern-start)
           (- 0 instance-index))
          ((= instance-index instance-start)
           pattern-index)
          (else
           (loop (- instance-index 1)
                 (kmp-step-backward pattern
                                    pattern-start
                                    pattern-index
                                    (u8vector-ref instance
                                                   (- instance-index 1))
                                    suffix-table))))))

(define (kmp-step-forward pattern pattern-start pattern-index octet prefix-table)
  (let loop ((pattern-index pattern-index))
    (cond ((= octet (u8vector-ref pattern pattern-index))
           (+ pattern-index 1))
          ((vector-ref prefix-table (- pattern-index pattern-start))
           => loop)
          (else
           pattern-start))))

(define (kmp-step-backward pattern pattern-start pattern-index octet suffix-table)
  (let loop ((pattern-index pattern-index))
    (let ((pattern-index* (- pattern-index 1)))
      (cond ((= octet (u8vector-ref pattern pattern-index*))
             pattern-index*)
            ((vector-ref suffix-table (- pattern-index* pattern-start))
             => loop)
            (else
             (+ pattern-start (vector-length suffix-table)))))))


;;-------------------------------------------------------------------------------
;;!! Computing the KMP Prefix (Suffix) Table

;; The prefix table maps offsets from the pattern start to the index
;; of the longest prefix of the pattern that we have already matched
;; from that offset in the pattern.  So if we have a pattern
;; `abcabd', if we get to the `d' and we find that the instance does
;; not have a `d', we have already matched `ab', and can avoid trying
;; to match it again.  If what we get, then, is `c', we can try to
;; move on.  False in the prefix table means that we must start over
;; in the pattern from the beginning.
;;
;; SCAN-PATTERN scans through the whole pattern unit by unit;
;; FIND-PREFIX finds the longest prefix so far, and either fills in
;; the prefix table if the suffix of what we have scanned still
;; matches a prefix of the pattern, or finds the next most recent
;; occurrence of the same prefix in the prefix table and repeats.

(define (compute-kmp-prefix-table pattern start end)
  (let ((prefix-table (make-vector (- end start) #f)))
    (define (store-prefix! index prefix-index)
      (vector-set! prefix-table (- index start) prefix-index))
    (define (fetch-prefix index)
      (vector-ref prefix-table (- index start)))
    (let scan-pattern ((index start) (prefix-index #f))
      (if (< index (- end 1))
          (let find-prefix ((prefix-index prefix-index))
            (cond ((not prefix-index)
                   (let ((index* (+ index 1)))
                     (if (not (= (u8vector-ref pattern index*)
                                 (u8vector-ref pattern start)))
                         (store-prefix! index* start))
                     (scan-pattern index* start)))
                  ((= (u8vector-ref pattern index)
                      (u8vector-ref pattern prefix-index))
                   (let ((index* (+ index 1))
                         (prefix-index* (+ prefix-index 1)))
                     (store-prefix! index* prefix-index*)
                     (scan-pattern index* prefix-index*)))
                  (else
                   (find-prefix (fetch-prefix prefix-index)))))))
    prefix-table))

(define (compute-kmp-suffix-table pattern start end)
  (let ((suffix-table (make-vector (- end start) #f)))
    (define (store-suffix! index suffix-index)
      (vector-set! suffix-table (- (- index 1) start) suffix-index))
    (define (fetch-suffix index)
      (vector-ref suffix-table (- (- index 1) start)))
    (let scan-pattern ((index end) (suffix-index #f))
      (if (> index (+ start 1))
          (let find-suffix ((suffix-index suffix-index))
            (if (not suffix-index)
                (let ((index (- index 1)))
                  (if (not (= (u8vector-ref pattern (- index 1))
                              (u8vector-ref pattern (- end 1))))
                      (store-suffix! index end))
                  (scan-pattern index end))
                (if (= (u8vector-ref pattern (- index 1))
                       (u8vector-ref pattern (- suffix-index 1)))
                    (let ((index* (- index 1))
                          (suffix-index* (- suffix-index 1)))
                      (store-suffix! index* suffix-index*)
                      (scan-pattern index* suffix-index*))
                    (find-suffix (fetch-suffix suffix-index)))))))
    suffix-table))
