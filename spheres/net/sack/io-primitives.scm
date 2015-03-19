;; Sack IO primitives module
;; Copyright (C) 2012, 2013 Mikael More
;;
;; This module defines the IO primitives and their handling procedures, for the Sack web server.
;;
;; The purpose is to provide "standard" primitives for ordinary HTTP access, while opening up for
;; special purpose primitives as for special delivery such as via HTTPS.
;;
;; The specific objectives with it are to provide an IO facility:
;;
;;  * that abstracts (so that SSL can be implemented transparently)
;;
;;  * with primitives that return #f on failure (EOF etc.).
;;    This way allowing the boolean logics primitives |or| |if| |and| etc. for primitive access.
;;
;;  * that does not throw exceptions, ever, but returns #f for that.
;;
;; A set of io-primitives is specific for a TCP connection. It's a set of closures for performing
;; the different IO operations such as read line with ISO-8859-1 encoding, write u8vector, etc.
;;
;; The routines are:
;;
;;      Name                      Arguments & Result                  Behavior
;;      read-subu8vector          (u8v start end #!optional (need 1)) Read u8vector data
;;                                => bytes read (presuming that at least |need| bytes were read), or #f
;;      read-substring            (str start end #!optional (need 1)) Read string data as ISO-8859-1
;;                                => characters(=bytes now) read (presuming at least |need| characters(=bytes for now) were read), or #f
;;      read-u8                   () => byte or #f                    Read a byte
;;      write-subu8vector         (u8v start end)                     Write u8vector data
;;                                => #t = succeeded as in written, #f = failed as attempt to write failed                                 ENSURE ACCORDANCE (passes #!eof now???)   Should return bytes written??
;;      display                   (v)                                 Write string, number or symbol as ISO-8859-1
;;                                => #t = succeeded as in written, #f = failed as attempt to write failed                                 ENSURE ACCORDANCE (passes #!eof now???)
;;      force-output              ()                                  Flush any write buffer to network
;;                                => #t = succeeded as in done, #f = failed as in port broken                                             ENSURE ACCORDANCE (passes #!eof now???)
;;      close-port                ()                                  Close port
;;                                => #t = succeeded as in done, #f = failed as in was already closed (incl. by other party?)              ENSURE ACCORDANCE (passes #!eof now???)
;;      make-read-line-until-crlf (#!optional return-partial-string?) Read line until reaching crlf. Permissive behavior.
;;                                => procedure taking no args         The procedure returned may be used in a single thread only and thus
;;                                => string or #f, see desc           reuse the same buffers across invocations.
;;                                                                    If return-partial-string? is set then on EOF, string reading is just
;;                                                                    ended and result is returned. If not, #f is returned in case of EOF.
;;
;; The current format for the io-primitives structure is a vector. This is done as to provide the highest speed of access.
;; (The vector access can be completely inlined in beneficiary code. Re |define-type| accessors I believe those remain
;; non-inlined procedure calls always, leading to lower potential performance. Because of the paramount need of performance
;; in this function, together with the very limited scope in which io-primitives structures are passed around, vectors are
;; very suitable.)
;;
;; ## History
;; 2013-09-15: Threadsafe io-primitives behavior means that only one outgoing and ingoing operation
;;             respectively can take place at the same time. Updated the code to follow this
;;             definition, until now threadsafe meant an io-primitives -global mutex was locked on
;;             each operation, meaning twoway communication was impaired.
;;

(declare (block)
         (standard-bindings)
         (extended-bindings)
         (mostly-fixnum))

(define (reverse-list->string clist)
  (let* ((len (length clist))
         (s (make-string len)))
    (do ((i (- len 1) (- i 1))   (clist clist (cdr clist)))
        ((not (pair? clist)))
      (string-set! s i (car clist)))
    s))

(define io-primitives-verbose? #f)
(set! io-primitives-verbose? #f)

(define console-output-port (current-output-port))

(define-macro (io-primitives-dbg . a)
  `(if io-primitives-verbose?
       (begin
         (print port: console-output-port "io-primitives: " ,@a "\n")
         (force-output console-output-port 1)))
  #!void)

;; (define-macro (io-primitives-status-add! v status) `(vector-set! ,v 8 (cons ,status (vector-ref ,v 8))))

;; Make an io-primitives structure for use by Sack, given thunks for the respective primitive
(define* (make-io-primitives read-subu8vector read-substring read-u8 write-subu8vector display force-output close-port(read-line-until-crlf #f))
  (vector read-subu8vector read-substring read-u8 write-subu8vector display force-output close-port read-line-until-crlf
                                        ; '(active) ; status. It's a chain as to be able to follow the changes.
          (current-thread) ; For lowlevel debugging: Include a reference to the current thread in the io-primitives structure.
          ))

;; ## Constructor (for using Gambit's IO system)
;; Make an io-primitives structure that delivers Gambit's IO system's primitives, given a gambit-port
;; on which to do the IO operations
(define* (with-standard-primitives gambit-port timeout-seconds thunk (threadsafe? #t))
  (let* ((io-primitives (make-standard-primitives gambit-port timeout-seconds threadsafe?))
         (r (thunk io-primitives)))
    ((io-primitives-close-port io-primitives))
    r))

;; For debug tracking
(define standard-io-primitives-all (make-table weak-keys: #t))
(define standard-io-primitives-count 0)

(define (make-standard-primitives gambit-port timeout-seconds #!optional (threadsafe? #t))
  (define not-closed? #t)
  (define closed?     #f)
  (define eof-object  #f)
  (define mutex-out   (and threadsafe? (make-mutex))) ; (No need to allocate it if we won't use it)
  (define mutex-in    (and threadsafe? (make-mutex)))
  (define mutex-close (and threadsafe? (make-mutex)))
  (define display-buffer-size 1024)
  (define display-buffer #f)
  (define-macro (perform direction . code)
    (define mutex (case direction
                    ((out  ) 'mutex-out)
                    ((in   ) 'mutex-in )
                    ((close) 'mutex-close)
                    ))
    `(begin
       (if threadsafe?
           (begin
             ;; It can happen that we get an IO primitive invocation while already in an IO primitive.
             ;; This would be if an IO primitive would throw an exception - generally that shouldn't happen
             ;; but in the current implementation (Gambit pre 4.6.8) it happens if a write fails - and then <- re 4.6.8 outdated
             ;; the exception handler does another IO primitive invocation.                                 <- is this outdated? in all cases it's good we check for it and handle like this.
             ;;
             ;; We respond to this with raising an exception
             (if (eq? (mutex-state ,mutex) (current-thread))
                 (error "io-primitives is not reentrant; the fact that this was attempted indicates a [correct] error state."))
             (mutex-lock! ,mutex)))
       (let ((r (begin ,@code)))
         (if threadsafe? (mutex-unlock! ,mutex))
         r)))
  ;; => closed-now? boolean
  ;; We return this only for the caller to be able to differentiate its behavior. Not sure if this ultimately ever is relevant.
  (define (close)
    (perform close
             (if not-closed?
                 (begin
                   ;; We have experienced that running Gambit's close-port leads to invocation of a routine inside
                   ;; ##make-device-port which leads to ##byte-wbuf-drain-no-reset which in turn, somehow, leads
                   ;; to a call to this procedure.
                   (set! not-closed? #f)
                   (set! closed?     #t)
                   ;; (close-port/noexception gambit-port)
                   (close-port gambit-port)
                   #t
                   )
                 (begin
                   ;; This is a supported usecase so no need to report it.
                   ;; (io-primitives-dbg "Had a (close) call on already closed Gambit port " gambit-port
                   ;;                    " (primitives #" (object->serial-number io-primitives) ").")
                   ;; A recursive call to (close) is not an error.
                   #f))))
  (define io-primitives #f)
  ;; Make the Gambit IO primitives return #f on IO error.
  (port-io-exception-handler-set! gambit-port (lambda (e) #f))
  (input-port-timeout-set! gambit-port
                           timeout-seconds
                           (lambda ()
                             ;; (io-primitives-status-add! io-primitives 'closed/input-timeout)
                             (close)
                             #f)) ; We return #f as to signal to Gambit that it should end the current IO operation. The
  ;; manual says that this leads to #!eof return for read ops and exception for write ops -
  ;; our port-io-exception-handler above should wrap at least the latter to a #f return value.
                                        ;
  ;; Obsolete: ; #t signals to Gambit that it should not cancel the IO operation that was ongoing on the
  ;; timeout. We don't signal that because for output operations that would raise an exception.
  ;; Instead, we hope Gambit identifies that the port is now closed and ends the respective
  ;; operation without throwing an exception.
  (output-port-timeout-set! gambit-port
                            timeout-seconds
                            (lambda ()
                              ;; (io-primitives-status-add! io-primitives 'closed/output-timeout)
                              (close)
                              ;; We return #f as to signal to Gambit that it should end the current IO operation. The
                              ;; manual says that this leads to #!eof return for read ops and exception for write ops -
                              ;; our port-io-exception-handler above should wrap at least the latter to a #f return value.
                              ;; Obsolete: ; #t signals to Gambit that it should not cancel the IO operation that was ongoing on the
                              ;; timeout. We don't signal that because for output operations that would raise an exception.
                              ;; Instead, we hope Gambit identifies that the port is now closed and ends the respective
                              ;; operation without throwing an exception.
                              #f))
  (let ((io-primitives*
         (make-io-primitives
                                        ; read-subu8vector :
          (lambda (u8v start end #!optional (need 1))
            (perform in
                     (if closed?
                         eof-object
                         (begin
                           (io-primitives-dbg "Making call to Gambit's read-subu8vector with start " start " end " end " need " need ".")
                           ;; (read-subu8vector/noexception u8v start end gambit-port need) ; TODO: Guarantee #!eof return value on EOF
                           (let ((r (read-subu8vector u8v start end gambit-port need))) ; TODO: Guarantee #!eof return value on EOF
                             (io-primitives-dbg "Gambit's read-subu8vector returned " r ".")
                             (if (or
                                  (eq? #!eof r)
                                  ;; Gambit's read-subu8vector may return with fewer bytes than as specified by the |need| argument!
                                  (and (fixnum? r)
                                       (fx> need r)))
                                 #f
                                 r))))))
          ;; read-substring :
          (lambda (str start end #!optional (need 1))
            (perform in
                     (if closed?
                         eof-object
                         ;; (read-substring/noexception str start end gambit-port need) ; TODO: Guarantee #!eof return value on EOF
                         (let ((r (read-substring str start end gambit-port need))) ; TODO: Guarantee #!eof return value on EOF
                           (if (eq? r #!eof)
                               #f
                               r)))))
          ;; read-u8 :
          (lambda ()
            (perform in
                     (if closed?
                         eof-object
                                        ; (let ((r (read-u8/noexception gambit-port)))
                         (let ((r (read-u8 gambit-port)))
                           (if (eq? r #!eof) ; Possible improvement would be that the read-u8 never returned #!eof in the first place.
                               #f            ;
                               r)))))        ;
          ;; write-subu8vector :
          (lambda (u8v start end)
            (perform out
                     (if closed?
                         eof-object
                         (write-subu8vector u8v start end gambit-port))))
          ;; display
          (lambda (v)
            (perform out
                     (if closed?
                         eof-object
                         (begin
                           ;; (display v gambit-port)
                           ;;
                           ;; There seems to be some issues with display:
                           ;;      0  ##mutex-lock-out-of-line!
                           ;;      1  ##make-device-port
                           ;;      2  ##write-generic-to-character-port
                           ;;      3  ##display
                           ;;      4  io-primitives#with-standard-primitives         "lib/sack/src/io-primitives.scm"@50:1          (display 8#v 1#gambit-port)
                           ;;
                           ;; Now, we only need a very basic display implementation anyhow, namely one that works exactly like this:
                           ;;
                           ;; Implementation that has large memory turnover:
                           ;; (let* ((v (cond ((symbol? v) (symbol->string v))
                           ;;                 ((number? v) (number->string v))
                           ;;                 (else        v                 )))
                           ;;        (l (string-length v))
                           ;;        (u8v (make-u8vector l)))
                           ;;   (let loop ((at 0))
                           ;;     (if (not (eq? at l))
                           ;;         (begin
                           ;;           (u8vector-set! u8v at (char->integer (string-ref v at)))
                           ;;           (loop (fx+ at 1)))))
                           ;;   (write-subu8vector u8v 0 l gambit-port)
                           ;;   )
                           ;;
                           ;; Implementation with less memory turnover:
                           (if (not display-buffer) (set! display-buffer (make-u8vector display-buffer-size)))
                           (let* ((v (cond ((symbol? v) (symbol->string v))
                                           ((number? v) (number->string v))
                                           (else        v                 )))
                                  (l (string-length v)))
                             (define (write-chars! string-start-idx count)
                               (io-primitives-dbg "(display): (write-chars!): Displaying " string-start-idx " and to " count ".")
                               (let loop ((at string-start-idx) (buffer-at 0))
                                 (if (not (eq? buffer-at count))
                                     (begin
                                       (u8vector-set! display-buffer buffer-at (char->integer (string-ref v at)))
                                       ;; (dbg "(display): (write-chars!): Copying " at " to " buffer-at ".")
                                       (loop (fx+ at 1) (fx+ buffer-at 1)))))
                               (io-primitives-dbg "(display): (write-chars!): Writing buffer 0 to " count ".")
                               (write-subu8vector display-buffer 0 count gambit-port))
                             (let loop ((start-at 0))
                               (let ((end-at (fx+ start-at display-buffer-size)))
                                 (if (fx> end-at l)
                                     (begin
                                       ;; (dbg "(display): Am at last segment, " start-at " and " (fx- l start-at) " onward.")
                                       (write-chars! start-at (fx- l start-at)))
                                     (if (eq? #!eof (write-chars! start-at display-buffer-size))
                                         #!eof
                                         (begin
                                           ;; (dbg "(display): Am at a segment, " start-at " and " display-buffer-size " onward.")
                                           (loop (fx+ start-at display-buffer-size))))))))))))
          ;; force-output :
          (lambda ()
            (perform out
                     (if closed?
                         eof-object
                         (begin
                           (io-primitives-dbg "forcing output.")
                                        ; (force-output/noexception gambit-port 1)
                           (let ((r (force-output gambit-port 1)))
                             (io-primitives-dbg "forced output, Gambit primitive returned " r ".")
                             r
                             ;; We wrap #!void to #t.
                             (and r #t))))))
          ;; close-port :
          (lambda ()
            ;; (perform close - this logic is inside (close).
            (if closed?
                eof-object
                (begin
                  ;; (io-primitives-status-add! io-primitives 'closed)
                  (if (close)
                      #t
                      ;; If another close call was made between our closed? check above and that (close)'s logics started.
                      eof-object))))
          ;; make-read-line-until-crlf :
          (lambda* ((return-partial-string? #f))
              ;; read-line-until-crlf :
              ;; Reads one line from port, using the byte reading functions and not
              ;; read-line. Safe to US ASCII only.
              (lambda ()
                (perform in
                         (if closed?
                             eof-object
                             (let loop ((lst '()))
                               (let ((x (if closed?
                                            eof-object
                                            ;; (read-u8/noexception gambit-port) ; #!eof response handled below.
                                            (begin
                                              (io-primitives-dbg "read-line-until-crlf: Reading a byte...")
                                              (let ((b (read-u8 gambit-port))) ; #!eof response handled below.
                                                (io-primitives-dbg "read-line-until-crlf: Got byte: " b " " (and (fixnum? b) (integer->char b))) ; fixnum check so it's not #!eof
                                                ;; - ** This changes when Gambit ports' have configurable eof-object
                                                b)))))
                                 (cond
                                  ((and (eq? x 10)
                                        (pair? lst)
                                        (eq? (car lst) #\return))
                                   (reverse-list->string (cdr lst)))
                                  ((or (eq? #!eof x)
                                       (eq? #f    x) ; = IO error.
                                       (eq? eof-object x)) ; = Closed. (For now this is always #f; we may want to update this logic too.)
                                   (if return-partial-string?
                                       (reverse-list->string lst)
                                       #f))
                                  (else
                                   (loop (cons (integer->char x) lst)))))))))))))
    (set! io-primitives io-primitives*)
    ;; Debug tracking
    (table-set! standard-io-primitives-all io-primitives #t)
    (let () (declare (not interrupts-enabled)) (set! standard-io-primitives-count (+ standard-io-primitives-count 1)))
    io-primitives))
