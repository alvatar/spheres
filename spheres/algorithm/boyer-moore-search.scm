;;!!! Boyer-Moore Fixed-Pattern Search Algorithm
;; .author Taylor R. Campbell
;; .author Alvaro Castro-Castilla, 2015
;;
;; This code is written by Taylor R. Campbell and placed in the Public
;; Domain.  All warranties are disclaimed.

(define-record-type <bm-cache>
  (make-bm-cache pattern
                 pattern-start
                 pattern-end
                 last-occurrence-table
                 good-suffix-table
                 max-suffix)
  bm-cache?
  (pattern bm-cache/pattern)
  (pattern-start bm-cache/pattern-start)
  (pattern-end bm-cache/pattern-end)
  (last-occurrence-table bm-cache/last-occurrence-table)
  (good-suffix-table bm-cache/good-suffix-table)
  (max-suffix bm-cache/max-suffix))

(define (u8vector-forward-search-cache/bm pattern start end)
  (let ((max-suffix (compute-bm-max-suffix pattern start end)))
    (make-bm-cache
     pattern start end
     (compute-bm-last-occurrence-table pattern start end)
     (compute-bm-good-suffix-table
      (- end start)
      max-suffix
      (compute-bm-prefix-table (subu8vector-reverse pattern start end)
                               0
                               (- end start)))
     max-suffix)))

(define (u8vector-backward-search-cache/bm pattern start end)
  (let ((nrettap (subu8vector-reverse pattern start end))
        (pattern-length (- end start)))
    (let ((max-suffix (compute-bm-max-suffix nrettap 0 pattern-length)))
      (make-bm-cache
       pattern start end
       (compute-bm-last-occurrence-table nrettap 0 pattern-length)
       (compute-bm-good-suffix-table
        pattern-length
        max-suffix
        (compute-bm-prefix-table pattern start end))
       max-suffix))))

(define (u8vector-search-forward/bm instance instance-start instance-end
                                     cache)
  (bm-search-forward instance instance-start instance-end
                     (bm-cache/pattern cache)
                     (bm-cache/pattern-start cache)
                     (bm-cache/pattern-end cache)
                     (bm-cache/last-occurrence-table cache)
                     (bm-cache/good-suffix-table cache)))

(define (u8vector-search-backward/bm instance instance-start instance-end
                                      cache)
  (bm-search-backward instance instance-start instance-end
                      (bm-cache/pattern cache)
                      (bm-cache/pattern-start cache)
                      (bm-cache/pattern-end cache)
                      (bm-cache/last-occurrence-table cache)
                      (bm-cache/good-suffix-table cache)))

(define (u8vector-search-forward*/bm instance instance-start instance-end
                                      cache)
  (bm-search-forward* instance instance-start instance-end
                      (bm-cache/pattern cache)
                      (bm-cache/pattern-start cache)
                      (bm-cache/pattern-end cache)
                      (bm-cache/last-occurrence-table cache)
                      (bm-cache/good-suffix-table cache)
                      (bm-cache/max-suffix cache)))

(define (u8vector-search-backward*/bm instance instance-start instance-end
                                       cache)
  (bm-search-backward* instance instance-start instance-end
                       (bm-cache/pattern cache)
                       (bm-cache/pattern-start cache)
                       (bm-cache/pattern-end cache)
                       (bm-cache/last-occurrence-table cache)
                       (bm-cache/good-suffix-table cache)
                       (bm-cache/max-suffix cache)))

;;; Some clarifications on the code of the following pages that
;;; implements the Boyer-Moore search algorithm:
;;;
;;; - /Offsets/ are differences between two positions in memory; an
;;;   offset is /from/ a lower position in memory.  Often, offsets are
;;;   from the starts of patterns, which are not the same as the
;;;   starts of the vectors underlying the patterns.
;;; - /Indices/ are offsets from the starts of vectors.
;;; - /Shifts/ are differences in offsets.
;;; - /Tables/ are mappings from subsets of the natural numbers.
;;; - The /pattern/ is the sequence of octets that we are searching
;;;   for; the /instance/ is that through which we are searching.
;;;
;;; The basic idea of this algorithm when searching forward is to
;;; match increasing *suffixes* of the pattern, rather than prefixes
;;; as algorithms like the naive one and the Knuth-Morris-Pratt one.
;;; Then, if we find a mismatch, we skip ahead as far as we can
;;; knowing what we have already found in the suffix we matched.  We
;;; skip using the maximum of the entries in these two tables:
;;; (1) the /last occurrence table/, which tells us where in the
;;;   pattern the last occurrence of any octet is (or tells us the
;;;   length of the pattern, if the octet does not occur in it at
;;;   all); and
;;; (2) the /good suffix table/, which tells us how far we can safely
;;;   skip before we have to match the last reoccurrence of the suffix
;;;   we already matched against the same position in the pattern.

;;;; Search Algorithm

(define (bm-search-forward instance instance-start instance-end
                           pattern pattern-start pattern-end
                           last-occurrence-table
                           good-suffix-table)
  (let* ((pattern-length (- pattern-end pattern-start))
         (scan-limit (- instance-end pattern-length)))
    (let scan ((scan-index instance-start))
      (if (> scan-index scan-limit)
          #f
          (let match ((match-index (+ scan-index pattern-length))
                      (pattern-index pattern-end))
            (let ((match-index (- match-index 1))
                  (pattern-index (- pattern-index 1)))
              (let ((match-octet (u8vector-ref instance match-index))
                    (pattern-octet (u8vector-ref pattern pattern-index)))
                (if (= match-octet pattern-octet)
                    (if (= match-index scan-index)
                        scan-index
                        (match match-index pattern-index))
                    (scan (+ scan-index
                             (bm-skip-forward pattern-start
                                              pattern-index
                                              match-octet
                                              last-occurrence-table
                                              good-suffix-table)))))))))))

(define (bm-search-backward instance instance-start instance-end
                            pattern pattern-start pattern-end
                            last-occurrence-table
                            good-suffix-table)
  (let* ((pattern-length (- pattern-end pattern-start))
         (scan-limit (+ instance-start pattern-length)))
    (let scan ((scan-index instance-end))
      (if (< scan-index scan-limit)
          #f
          (let match ((match-index (- scan-index pattern-length))
                      (pattern-index pattern-start))
            (let ((match-octet (u8vector-ref instance match-index))
                  (pattern-octet (u8vector-ref pattern pattern-index)))
              (if (= match-octet pattern-octet)
                  (if (= pattern-index (- pattern-end 1))
                      (- scan-index pattern-length)
                      (match (+ match-index 1)
                             (+ pattern-index 1)))
                  (scan (- scan-index
                           (bm-skip-backward pattern-end
                                             pattern-index
                                             match-octet
                                             last-occurrence-table
                                             good-suffix-table))))))))))

;;;;; Multiple-Occurrence Search

(define (bm-search-forward* instance instance-start instance-end
                            pattern pattern-start pattern-end
                            last-occurrence-table
                            good-suffix-table
                            max-suffix)
  (let* ((pattern-length (- pattern-end pattern-start))
         (scan-limit (- instance-end pattern-length)))
    (let scan ((scan-index instance-start)
               (occurrences '()))
      (if (> scan-index scan-limit)
          (reverse! occurrences)
          (let match ((match-index (+ scan-index pattern-length))
                      (pattern-index pattern-end))
            (let ((match-index (- match-index 1))
                  (pattern-index (- pattern-index 1)))
              (let ((match-octet (u8vector-ref instance match-index))
                    (pattern-octet (u8vector-ref pattern pattern-index)))
                (if (= match-octet pattern-octet)
                    (if (= match-index scan-index)
                        (scan (+ scan-index max-suffix)
                              (cons scan-index occurrences))
                        (match match-index pattern-index))
                    (scan (+ scan-index
                             (bm-skip-forward pattern-start
                                              pattern-index
                                              match-octet
                                              last-occurrence-table
                                              good-suffix-table))
                          occurrences)))))))))

(define (bm-search-backward* instance instance-start instance-end
                             pattern pattern-start pattern-end
                             last-occurrence-table
                             good-suffix-table
                             max-suffix)
  (let* ((pattern-length (- pattern-end pattern-start))
         (scan-limit (+ instance-start pattern-length)))
    (let scan ((scan-index instance-end)
               (occurrences '()))
      (if (< scan-index scan-limit)
          (reverse! occurrences)
          (let match ((match-index (- scan-index pattern-length))
                      (pattern-index pattern-start))
            (let ((match-octet (u8vector-ref instance match-index))
                  (pattern-octet (u8vector-ref pattern pattern-index)))
              (if (= match-octet pattern-octet)
                  (if (= pattern-index (- pattern-end 1))
                      (scan (- scan-index max-suffix)
                            (cons (- scan-index pattern-length)
                                  occurrences))
                      (match (+ match-index 1)
                             (+ pattern-index 1)))
                  (scan (- scan-index
                           (bm-skip-backward pattern-end
                                             pattern-index
                                             match-octet
                                             last-occurrence-table
                                             good-suffix-table))
                        occurrences))))))))

;;;;; Boyer-Moore Tables

;;; This computes a table of the offset of the last occurrence in the
;;; pattern of each possible octet.  This is the easiest part of the
;;; algorithm: as we scan the pattern backward, if we find a mismatch,
;;; we can skip forward by the offset in this table for the octet we
;;; found in the instance and not in the pattern.  By default, we can
;;; just skip the length of the pattern if we found an octet that was
;;; not in it at all.

(define (compute-bm-last-occurrence-table pattern start end)
  (let ((last-occurrence-table (make-vector #x100 (- end start))))
    (do ((index start (+ index 1)))
        ((= index end))
      (vector-set! last-occurrence-table
                   (u8vector-ref pattern index)
                   (+ 1 (- index start))))
    last-occurrence-table))

;;; This is the hard part of the algorithm.  It yields a table mapping
;;; offsets in the pattern to the maximum amount by which we can shift
;;; the pattern's index if we have found a mismatch, based only on the
;;; position in the pattern, and consequently irrespective of the
;;; mismatch octet.  (Only the last occurrence table, above, supplies
;;; information regarding the octet found in the instance.)
;;;
;;; We do this using the length of the longest suffix of the pattern
;;; matching no prefix of the pattern, and a prefix table of the
;;; *reversed* pattern.  (See the next page for the computation of
;;; those data.)  This gives us the amount of the *suffix* matched by
;;; matching backward to any point in the pattern.  For example, for
;;; the pattern "abcdabcab", we reverse the pattern to get a reversed
;;; prefix table of #(0 0 0 1 2 0 0 1 2).  The maximum suffix length
;;;                   b a c b a d c b a
;;; of "abcdabcab" is 7, for "cdabcab", which is the longest suffix
;;; that shares no prefix of the pattern.  So the good suffix table is
;;; #(7 7 7 7 7 7 3 3 1).  The `1' in the last position means that if
;;;   a b c d a b c a b
;;; we find no `b' in the position in the instance corresponding with
;;; the end of the pattern, we can safely skip forward only by one
;;; position, because that next position in the pattern could be a
;;; `b'.  But if we find a `b' in the instance, and the preceding
;;; octet is not an `a', then we can skip forward by three positions,
;;; because that is where the previous `b' in the pattern will occur:
;;;                          abcdabcab
;;;                             abcdabcab
;;; The same goes for the `c', because the unit preceding `b' is `a'
;;; in both of the occurrences of `b'.  (By contrast, had we the
;;; pattern "abcdxbcab", it would be 7.)  But then when we find a
;;; mismatch for the `b' before the `c', we can safely skip over the
;;; entire maximum suffix, because nowhere else does "cab" occur, and
;;; there's no point in trying to match it up again.  However, the
;;; suffix "ab" of *that* suffix which we already matched *does* occur
;;; in the pattern, so we can't just skip over the *whole* pattern.

(define (compute-bm-good-suffix-table pattern-length max-suffix prefix-table)
  (let ((good-suffix-table (make-vector pattern-length max-suffix)))
    (do ((offset 0 (+ offset 1)))
        ((= offset pattern-length))
      (let ((prefix-length (vector-ref prefix-table offset)))
        (let ((offset (- (- pattern-length 1) prefix-length))
              (shift (- (+ offset 1) prefix-length)))
          (if (< shift (vector-ref good-suffix-table offset))
              (vector-set! good-suffix-table offset shift)))))
    good-suffix-table))

;;; This returns the length of the longest suffix of the pattern
;;; matching no prefix of the pattern, so for a pattern "abcdabcab"
;;; this will yield 7 (for "cdabcab").  We find the longest prefix
;;; matched at the last octet of the pattern, using the prefix table
;;; computed below, and subtract its length from the length of the
;;; pattern, because that will be the longest prefix that could
;;; possibly be matched by any suffix of the string (which must
;;; include the last octet).

(define (compute-bm-max-suffix pattern start end)
  (let ((length (- end start)))
    (- length
       (vector-ref (compute-bm-prefix-table pattern start end)
                   (- length 1)))))

;;; This returns a table mapping each offset in the pattern to the
;;; offset of the longest prefix of the pattern that precedes it.
;;; E.g., the pattern "abcdabcab" will yield #(0 0 0 0 1 2 3 1 2).
;;;                                            a b c d a b c a b

(define (compute-bm-prefix-table pattern start end)
  (let* ((pattern-length (- end start))
         (prefix-table (make-vector pattern-length 0)))
    (let scan-pattern ((offset 1) (prefix-length 0))
      (if (< offset pattern-length)
          ((lambda (prefix-length)
             (vector-set! prefix-table offset prefix-length)
             (scan-pattern (+ offset 1) prefix-length))
           (let ((octet (u8vector-ref pattern (+ start offset))))
             (let find-prefix ((prefix-length prefix-length))
               (cond ((= octet
                         (u8vector-ref pattern
                                        (+ start prefix-length)))
                      (+ prefix-length 1))
                     ((= prefix-length 0) 0)
                     (else
                      (find-prefix (vector-ref prefix-table
                                               (- prefix-length 1))))))))))
    prefix-table))

;;; Once we have computed the above tables, we can use these utilities
;;; to skip forward in the actual search routines.

(define (bm-skip-forward pattern-start pattern-index match-octet
                         last-occurrence-table
                         good-suffix-table)
  (let ((pattern-offset (- pattern-index pattern-start)))
    (max (+ 1
            (- pattern-offset
               (vector-ref last-occurrence-table match-octet)))
         (vector-ref good-suffix-table pattern-offset))))

(define (bm-skip-backward pattern-end pattern-index match-octet
                          last-occurrence-table
                          good-suffix-table)
  (let ((pattern-offset (- pattern-end pattern-index)))
    (max (- pattern-offset
            (vector-ref last-occurrence-table match-octet))
         (vector-ref good-suffix-table (- pattern-offset 1)))))

