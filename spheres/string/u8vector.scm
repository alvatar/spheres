;;!!! U8vector String utilities

;; .author Mikael More. Copyright (C) 2008, 2011, 2012, 2013, 2014
;; .author Álvaro Castro-Castilla, 2015 - check-arg macros

;;
;; ## Exports
;; # U8vector -> string conversion
;; (utf8-u8vector->string u8v) => string
;; Highly optimized routine for converting an UTF8-encoded U8vector to string.
;;  XX Please note that this procedure works around a freeze bug that is in Gambit 4.6.3 and has been
;;  XX there for at least 2-3 years.
;; (This current implementation does not acknowledge any byte sequences as reflecting broken encoding.
;; Perhaps that kind of acknowledgmeent&/error is something that with benefit could be implemented?)
;;
;; (u8vector->string u8v #!key (char-encoding 'UTF-8) (ignore-errors-in-encoding #f)) => string
;; Currently this procedure is a bit double in function:
;;  * If the char-encoding is 'UTF-8 (default), the call is passed on to utf8-u8vector->string
;;    and the ignore-errors-in-encoding argument is *ignored*. This gives high speed processing.
;;  * If char-encoding is not 'UTF-8, the source port is read on a character per character basis
;;    and the result is output on a character per character basis, meaning a total processing speed of
;;    250KB/sec or so. The low processing speed is to get exact error reporting in case
;;    ignore-errors-in-encoding is set, and perhaps to work around some bug in Gambit's decoder???
;;
;; (utf8-u8vector-port->string-reader port #!optional (buffer-size 1024))
;; => a procedure taking the arguments (target-string start end need) => characters-read
;; Highly optimized reader of the u8vector port |port| delivering its UTF8-decoded representation in
;; string form. Allocates an internal buffer of buffer-size bytes for readahead caching.
;; target-string is the string into which to read the string output data. start and end are the
;; character offsets in target-string into where the data should be read. need is the number of characters
;; that is required to be read in total before this procedure returns.
;;
;; (u8vector->ISO-8859-1-string u8vect)
;; Takes input u8vector u8vector and maps it into a string where the byte to character conversion is made on
;; a one-to-one basis.
;;
;; (subu8vector->ISO-8859-1-string u8vect start end)
;; Same as u8vector->ISO-8859-1-string but only for u8vect's byte offsets start to end.
;;
;; # String -> u8vector conversion
;; (string->u8vector s #!optional (char-encoding 'UTF-8))
;; Takes string s and returns it in byte-encoded form as an u8vector, using the char-encoding char-encoding.
;;
;; (ISO-8859-1-string->u8vector str)
;; Byte-encodes string str into an u8vector which is return value. Uses ISO8859 encoding i.e. presumes input
;; string only has character codes in the interval 0-255 and outputs corresponding bytes.
;;
;; (ISO-8859-1-substring->u8vector str start end)
;; Same as ISO-8859-1-string->u8vector but only for the interval from character offset start to end in str.
;;
;; # U8vector <-> hexadecimal string routines
;; (u8vector->hex-string u8vect #!optional (use-capitals? #t) (separate-by-spaces? #t)) => hex-string
;; Converts all u8vect's contents into string notation e.g.
;; (u8vector->hex-string '#u8(1 5 10 16 32 64)) => "01050a102040"
;;
;; (subu8vector->hex-string u8vect start end #!optional (use-capitals? #t) (separate-by-spaces? #t)) => hex-string
;; Same as u8vector->hex-string but only for u8vect's byte offset interval start .. end.
;;
;; (hex-string->u8vector s #!optional (separated-by-spaces? #t)) => u8vector
;; Converts hex string s into its u8vector representation, which is the result value. Accepts both
;; upper and lower cased input.
;;
;; # Other u8vector I/O
;; (u8vector->file u8vector filename) => #!void
;; Writes u8vector into file filename.
;;
;; (file->u8vector filename) => u8vector
;; Reads filename and returns its contents as an u8vector.
;;
;; (write-u8vector vec #!optional port)
;; Wrapper for |write-subu8vector| that outputs the entire string. If port is not specified, current-output-port
;; is used.
;;
;; (dump-u8vector-port source-port #!optional target-port) => #!void
;; Reads out u8vector data from source-port and outputs it into target-port, all until source-port signals
;; end of file. Uses a 50KB buffer.
;;
;; ## History
;; 2013-03-13: Made u8vector->string and string->u8vector use 'lf for eol-encoding, as noticed that
;;             the other two ones ('cr-lf , which was the one used til now, and 'cr) is information-
;;             destructive on serialization work, to either "\r", "\n" or "\r\n" sequences.
;;             Thus thanks to this update now, both these two routines keep data unmodified now.
;;
;;             Implemented an internal |string->utf8-u8vector| for speed and convenience, ought to
;;             use less internal buffering and alike. Optimized |utf8-u8vector->string| a bit.
;;
;;             Made both |string->utf8-u8vector| and |utf8-u8vector->string| (declare (not safe)).
;;
;;             They score at 126ms, compared with ~4200ms when (safe) and 300ms which is the time
;;             Gambit's coder takes to do the same.
;; 2013-04-01: Added capitalization and spacing options to u8vector <-> hex string procedures, both on by
;;             default, as to increase their usefulness in debugging. This is what they're used for
;;             anyhow.
;; 2014-01-04: Added |substring->utf8-u8vector| and |u8vector-pad-to-length|.
;;
;; ## TODO
;; * XXX NOTE that in the current state, |string->u8vector| replaces every \n with \r\n!!!
;;   XXX This is because of a funny Gambit behavior that should be addressed.
;;


(define (hex-char->integer c)
  (let ((i (char->integer c)))
    (cond
     ((fx<= 48 i 57)                    ; #\0 .. #\9
      (fx- i 48))
     ((fx<= 97 i 102)                   ; #\a .. #\f
      (fx+ (fx- i 97) 10))
     ((fx<= 65 i 70)                    ; #\A .. #\F
      (fx+ (fx- i 65) 10))
     (else
      (error "Character not 0-9, A-F, a-f." c)))))

;; Based on http://www.webtoolkit.info/javascript-utf8.html
;; (This current implementation does not acknowledge any byte sequences as reflecting broken encoding.
;; Perhaps that kind of acknowledgmeent&/error is something that with benefit could be implemented?)
;; Benchmark, for a 4MB string:
;;  * Gambit's builtin decoder                                          : (like 10 seconds, though this is in a bug workaround mode I believe! Fix and reevaluate.)
;;  * This procedure, with (declare (safe))                             : 3937ms
;;  * This procedure, with (declare (not safe))                         : 126ms
;;  * This procedure, with (declare (not safe) (not interrupts-enabled)): 123ms
;; Therefore, given that we know for sure that no risks are introduced with (not safe), then it is
;; good design to go with it.
(define (utf8-u8vector->string u8v)
  (check-arg u8vector? u8v utf-u8vector->string)
  (let* ((u8v-len (u8vector-length u8v))
         (len u8v-len))
    (let ()
      (cond-expand (optimize (declare (not safe))) (else (void)))
      (let loop ((i 0))
        (if (##fx< i u8v-len)
            (let ((b (##u8vector-ref u8v i)))
              (loop (##fx+ i (if (##fx>= b 128)
                                 (let ((add-more (if (##fx< 191 b 224) 1 2)))
                                   ;; (print "UTF-8 encoded char at u8vector-ref. " i ", means -"
                                   ;;        add-more " chars string length.\n")
                                   (set! len (##fx- len add-more))
                                   (##fx+ add-more 1))
                                 1)))))))
    ;; It could be that we get a negative length because the string was very short and ended
    ;; with only the first 1-2 bytes of a 2-3 byte multibyte UTF-8 character.
    (check-arg (lambda (len) (fx>= len 0)) len 'utf-u8vector->string "The input u8vector is not proper UTF-8.")
    ;; (print "output string length = " len " (u8vector length=" u8v-len ")\n")
    ;; Compiled with (declare (safe))
    (let ((s (make-string len)))
      (cond-expand (optimize (declare (not safe))) (else (void)))
      (let loop ((u8v-ref 0) (s-ref 0))
        (if (##fx< u8v-ref u8v-len)
            (let ((b (##u8vector-ref u8v u8v-ref)))
              (loop (##fx+ u8v-ref
                           (if (##fx< b 128)
                               (begin
                                 (##string-set! s s-ref (integer->char b))
                                 1)
                               (let* ((u8v-ref-b2 (##fx+ u8v-ref 1))
                                      (b2 (##u8vector-ref u8v u8v-ref-b2)))
                                 (if (##fx< 191 b 224)
                                     (begin
                                       (##string-set! s s-ref
                                                      (integer->char
                                                       (##fxior (##fxarithmetic-shift-left (##fxand b 31) 6)
                                                                (##fxand b2 63))))
                                       2)
                                     (let ((b3 (##u8vector-ref u8v (##fx+ u8v-ref-b2 1))))
                                       (##string-set! s s-ref
                                                      (integer->char
                                                       (##fxior (##fxarithmetic-shift-left (##fxand b  15) 12)
                                                                (##fxarithmetic-shift-left (##fxand b2 31) 6 )
                                                                (##fxand b3 63))))
                                       3)))))
                    (##fx+ s-ref 1)))))
      s)))

;;! This procedure has been implemented for the purpose of doing faster string to utf8-u8vector serialization.
;; It was not implemented to work around a Gambit bug or alike.
;;
;; Benchmark, for a 4M-char string:
;;  * Gambit's builtin encoder                                          : 310ms
;;  * This procedure, with (declare (safe))                             : 4300ms
;;  * This procedure, with (declare (not safe))                         : 97ms
;;  * This procedure, with (declare (not safe) (not interrupts-enabled)): 90ms
;;
;; Therefore, given that we know for sure that no risks are introduced with (not safe), then it is
;; good design to go with it.
;;
;; Based on http://www.webtoolkit.info/javascript-utf8.html
(define (string->utf8-u8vector s)
  (check-arg string? s string->utf8-u8vector)
  (substring->utf8-u8vector s 0 (string-length s)))

(define (substring->utf8-u8vector s start end)
  (check-arg string? s string->utf8-u8vector)
  (check-arg (lambda (x) (and (fixnum? x) (fx<= 0 x end)))
             start string->utf8-u8vector "argument 2 must be a fixnum in the interval 0 to end")
  (check-arg (lambda (x) (and (fixnum? x) (fx<= 0 x (string-length s))))
             end string->utf8-u8vector "argument 3 must be a fixnum in the interval 0 to string-length of s")
  ;; Determine length of u8vector to be generated.
  (let* ((u8v-len (##fx- end start)))
    (let ()
      (cond-expand (optimize (declare (not safe))) (else (void)))
      (let loop ((s-at start))
        (if (##fx< s-at end)
            (let ((c (char->integer (##string-ref s s-at)))) ; There's no ##char->integer apparently.
              (if (##fx>= c 128)
                  (set! u8v-len (##fx+ u8v-len (if (##fx< c 2048) 1 2))))
              (loop (##fx+ s-at 1))))))

    ;; Produce the output u8vector
    (let ((u8v (make-u8vector u8v-len))) ; This one is compiled with (declare (safe)).
      (cond-expand (optimize (declare (not safe))) (else (void)))
      (let loop ((s-at start) (u8v-at 0))
        (if (##fx< s-at end)
            (let ((c (char->integer (##string-ref s s-at)))) ; "
              (loop (##fx+ s-at 1)
                    (##fx+ u8v-at (cond ((##fx< c 128)
                                         (##u8vector-set! u8v u8v-at c)
                                         1)
                                        ((##fx< c 2048)
                                         (##u8vector-set! u8v u8v-at           (##fxior (##fxarithmetic-shift-right c 6) 192))
                                         (##u8vector-set! u8v (##fx+ u8v-at 1) (##fxior (##fxand c 63) 128))
                                         2)
                                        (else
                                         (##u8vector-set! u8v u8v-at           (##fxior (##fxarithmetic-shift-right c 12) 224))
                                         (##u8vector-set! u8v (##fx+ u8v-at 1) (##fxior (##fxand (##fxarithmetic-shift-right c 6) 63) 128))
                                         (##u8vector-set! u8v (##fx+ u8v-at 2) (##fxior (##fxand c 63) 128))
                                         3)))))
            u8v)))))

;;! This procedure provides a fast reading mechanism using which to read UTF-8 data from any u8vector port,
;; into a target string. It should be basically as fast as can be done.
;;
;; It does some read-ahead buffering, so the port must be completely devoted to reading with this reader.
;;
;; Should be easily wrappable into a macro for making readers for more encodings.
;;
;; Example use:
;; (define p (open-u8vector))
;; (import (std misc/u8v)) (define x (utf8-u8vector-port->string-reader p))
;; (define u '#u8(229 159 188 231 156 140 233 131 168 59 32 72 105 33))
;; (write-subu8vector u 0 (u8vector-length u) p)
;; (define s (make-string 100))
;; (x s 0 100 3)
(define* (utf8-u8vector-port->string-reader port (buffer-size 1024))
  (define-macro (dbg . args) #!void)
  (define-macro (input:read-byte)
    `(begin
       (set! input:read-byte-idx (##fx+ input:read-byte-idx 1))
       (let ((r (if (##fx> input:read-byte-idx we-have-data-up-to-idx)
                    (begin
                      (dbg "(input:read-byte) goes to (input:read-new-data-to-buffer!).")
                      (input:read-new-data-to-buffer!)
                      (##u8vector-ref read-buffer 0))
                    (##u8vector-ref read-buffer input:read-byte-idx))))
         (dbg "(input:read-byte) returns " r ", is at read buffer idx " input:read-byte-idx ".")
         r)))
  (check-arg port? port utf8-u8vector-port->string-reader)
  (check-arg fixnum? buffer-size utf8-u8vector-port->string-reader)
  ;; Reader-thunk, when invoked, continues its operation forever.
  ;;
  ;; Prior to the first output:get-new-read-operation callback, at least one byte will have been
  ;; read from port.
  ;;
  ;; output:target-string output:start output:end output:need-chars represent the first read operation
  ;; to be made. output:get-new-read-operation is invoked as soon as this read operation is finished,
  ;; with the number of bytes read as only argument. Its return value should be a (values) with
  ;; exactly the same thing as passed as arguments to this procedure in the first place.
  (let* ((reader-thunk
          (lambda (output:target-string output:start output:end output:need-chars
                                   output:get-new-read-operation)
            (let* ((buffer-size buffer-size) ; 1024)
                   (read-buffer (make-u8vector buffer-size))
                   (input:read-byte-idx 0)
                   (we-have-data-up-to-idx 0))
              ;; Here's a descriptive example of our reading strategy:
              ;; If we get a read operation to a string, where 200 chars are needed, start is 0 and end is 400,
              ;; then when we read-subu8vector , we make a read for the entire buffer size and with need
              ;; set to 200.
              ;;
              ;; The read returns 150. Those 150 bytes are now read into the string as 150 chars. On the first
              ;; (input:read-byte) where the buffer is depleted, need will be 50. Those 50 bytes are now read
              ;; into the string as 49 chars. On the first (input:read-byte) after the buffer is depleted,
              ;; need will be 1. On the read, 10 bytes are read though - and the 3 bytes needed for that
              ;; UTF-8 char have all been read, and another 7 bytes creating 7 more chars. They're processed,
              ;; and 207 is returned to the caller.
              (define (input:read-new-data-to-buffer!)
                (let read-again-loop ()
                  (let* ((output:need-chars-still (fx- output:need-chars (fx- output:write-idx output:start))) ; May be negative
                         ;; This is the minimum case of one byte per UTF-8 char. Always read at least one byte,
                         ;; except for if we're reading beyond the chars needed to be read, in this case we
                         ;; need to read nothing (this is just a check before returning to caller anyhow).
                         (we-know-we-need-at-least-bytes (if (fx> output:need-chars-still 0)
                                                             output:need-chars-still
                                                             0))
                         (read-bytes (let ((need (fxmin we-know-we-need-at-least-bytes buffer-size)))
                                       (dbg "Going into read-subu8vector. need = " need)
                                       (read-subu8vector read-buffer 0 buffer-size port need))))
                    (dbg "read-subu8vector read " read-bytes " bytes.")
                    (if (zero? read-bytes)
                        (begin
                          (dbg "Read nothing. The only case in which this can happen, is when we have already read all "
                               "needed characters and now we're just checking if there's more to read anyhow.")
                          ;; Internal debug assertion. Comment out when we doublechecked code performs intended task correctly.
                          (let ((output:need-to-read-up-to-and-incl-idx (fx+ output:start output:need-chars -1)))
                            (if (not (fx> output:write-idx output:need-to-read-up-to-and-incl-idx))
                                (error "Internal inconsistency")))
                          (output:return-result-to-caller&get-read-job)
                          (read-again-loop))
                        (begin
                          (set! we-have-data-up-to-idx (fx- read-bytes 1))
                          (set! input:read-byte-idx 0))))))
              (define output:write-idx 0)
              (define (output:give-char char-no)
                ;; For every target-string we're given to write to, the caller has assured there's at least one
                ;; character to be read to it, thus it's safe for us to assume there's space for one char
                ;; to be set here. (This layout gives the simplest code layout.)
                (##string-set! output:target-string output:write-idx (integer->char char-no))
                (dbg "Put read char " (integer->char char-no) " (" char-no ") in output string #"
                     (object->serial-number output:target-string) " at idx " output:write-idx
                     " (start " output:start " end " output:end " need-chars " output:need-chars ")")
                (set! output:write-idx (##fx+ output:write-idx 1))
                (if (fx>= output:write-idx output:end)
                                        ; If the writing of the string has been completed, we return immediately, prior to
                                        ; getting into any more reading from the input port.
                    (output:return-result-to-caller&get-read-job)))
              (define (output:return-result-to-caller&get-read-job)
                (let ((read-chars (fx- output:write-idx output:start)))
                  (dbg "Done with filling up this string #" (object->serial-number output:target-string) ", read "
                       read-chars " chars, returning to caller."
                       " (could take " (- output:end output:start) ", needed " output:need-chars ")"
                       " str: " (substring output:target-string output:start output:write-idx) " /str")
                  (receive
                   (target-string start end need-chars get-new-read-operation)
                   (output:get-new-read-operation read-chars)
                   (set! output:target-string target-string)
                   (set! output:start start)
                   (set! output:end end)
                   (set! output:need-chars need-chars)
                   (set! output:write-idx start)
                   (set! output:get-new-read-operation get-new-read-operation)
                   (dbg "Got into new read job; target-string is #" (object->serial-number output:target-string)
                        " start " output:start " end " output:end " need-chars " output:need-chars "."))))
              ;; Technically what we do here is to read forever.
              ;; (This continuation is GC:ed at end of use of this procedure.)
              (dbg "Read loop starts.")
              (let loop ()
                (let ((char-read-no
                       (let ((b (input:read-byte)))
                         (if (##fx< b 128)
                             b
                             (let ((b2 (input:read-byte)))
                               (if (##fx< 191 b 224)
                                   (##fxior (##fxarithmetic-shift-left (##fxand b 31) 6)
                                            (##fxand b2 63))
                                   (let ((b3 (input:read-byte)))
                                     (##fxior (##fxarithmetic-shift-left (##fxand b  15) 12)
                                              (##fxarithmetic-shift-left (##fxand b2 31) 6 )
                                              (##fxand b3 63)))))))))
                  (dbg "Read char: " (integer->char char-read-no) " (" char-read-no ")")
                  (output:give-char char-read-no)
                  (loop))))))
         (reader-thunk-continuation reader-thunk)
         (read-chars
          (lambda (target-string start end need-chars)
            (call/cc
             (lambda (return)
               (reader-thunk-continuation target-string start end need-chars
                                          ;; output:get-new-read-operation:
                                          (lambda (read-bytes)
                                            (call/cc
                                             (lambda (continue)
                                               (set! reader-thunk-continuation continue)
                                               (return read-bytes))))))))))
    (lambda (target-string start end need)
      (dbg "Called for target string #" (object->serial-number target-string) " start " start " end " end " need " need)
      (let* ((reach-for (fx- end start))
             (need (fxmin reach-for need)))
        (if (or (fx> 0 start) (fx> 0 end) (fx> 0 reach-for)) (error "Must want at least 0 bytes."))
        (let ((sl (string-length target-string)))
          (if (or (fx> (fx+ start need) sl) ; If length is 5, start is 2 and needed is 3, then pos 2 3 4 will be read = ok
                  (fx< end (fx+ start need))) ; Same calculation as in previous step but with end instead of length.
              (error "Aimed at reading out of range of target-string")))
        (if (zero? reach-for)
            ;; Success - zero characters were read.
            0
            ;; From here we know that the reading is to valid positions in string, so we can go with ##string-set! .
            ;; Furthermore, we know at least one character can be read into the string.
            (read-chars target-string start end need))))))


;;!! Transform an encoded u8vector into a string
(define* (u8vector->string u8v
                           (char-encoding: 'UTF-8)
                           (ignore-errors-in-encoding: #f))
  (check-arg u8vector? u8v u8vector->string)
  (if (eq? char-encoding 'UTF-8)
      (utf8-u8vector->string u8v)
      (call-with-input-u8vector
       `(char-encoding: ,char-encoding
                        ;; The 'lf eol-encoding is the only one that does not make changes
                        ;; to serialization of "\r", "\n" or "\r\n".
                        eol-encoding: lf
                        init: ,u8v)
       (lambda (input-port)
         (call-with-output-string
          '()
          (lambda (output-port)
            (let loop ()
              (let ((c
                     (with-exception-catcher
                      (lambda (e)
                        (if ignore-errors-in-encoding
                            #f
                            (raise
                             (error "Failed to read u8vector, broken characters? Data: "
                                    u8v))))
                      (lambda ()
                        (read-char input-port)))))
                ;; (dbg "Had " c)
                (if (not (eq? c #!eof))
                    (begin
                      (if c (write-char c output-port))
                      (loop)))))))))))

;;! Transform a string into an encoded u8vector
;; XXX NOTE that in the current state, this procedure replaces every \n with \r\n!!!
;; XXX This is a funny Gambit behavior that should be addressed.
(define* (string->u8vector s (char-encoding: 'UTF-8))
  (check-arg string? s string->u8vector)
  (if (eq? char-encoding 'UTF-8)
      (string->utf8-u8vector s)
      (call-with-output-u8vector
       ;; The 'lf eol-encoding is the only one that does not make changes
       ;; to serialization of "\r", "\n" or "\r\n".
       `(char-encoding: ,char-encoding eol-encoding: lf)
       (lambda (p) (write-substring s 0 (string-length s) p)))))

(define (ISO-8859-1-substring->u8vector str start end)
  (check-arg string? str ISO-8859-1-substring->u8vector)
  (let* ((len (fx- end start))
         (u8vect (make-u8vector len)))
    (let loop ((i 0))
      (if (fx< i len)
          (begin
            (u8vector-set! u8vect
                           i
                           (char->integer (string-ref str (fx+ start i))))
            (loop (fx+ i 1)))
          u8vect))))

(define (ISO-8859-1-string->u8vector str)
  (check-arg string? str ISO-8859-1-string->u8vector)
  (ISO-8859-1-substring->u8vector
   str
   0
   (string-length str)))

(define (subu8vector->ISO-8859-1-string u8vect start end)
  (check-arg u8vector? u8vect subu8vector->ISO-8859-1-string)
  (let* ((len (- end start))
         (str (make-string len)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (string-set!
             str
             i
             (integer->char (u8vector-ref u8vect (+ start i))))
            (loop (+ i 1)))
          str))))

(define (u8vector->ISO-8859-1-string u8vect)
  (check-arg u8vector? u8vect u8vector->ISO-8859-1-string)
  (subu8vector->ISO-8859-1-string u8vect
                                  0
                                  (u8vector-length u8vect)))

(define* (subu8vector->hex-string u8vect start end (use-capitals? #t) (separate-by-spaces? #t))
  (check-arg u8vector? u8vect subu8vector->hex-string)
  (let* ((hex-chars (if use-capitals? "0123456789ABCDEF" "0123456789abcdef"))
         (digit->char
          (lambda (d)
            (##string-ref hex-chars d)))
         (len (fx- end start))
         (n (if separate-by-spaces?
                (fxmax 0 (fx- (fx* len 3) 1)) ; No need for an ending space. However if empty u8v make this 0.
                (fx* len 2)))
         (str (make-string n #\space))
         (increment-size (if separate-by-spaces? 3 2)))
    (let loop ((i 0) (j (- len 1)) (k 0))
      (if (>= j 0)
          (let ((x (u8vector-ref u8vect k)))
            (string-set! str i       (digit->char (fxquotient  x 16)))
            (string-set! str (+ i 1) (digit->char (fxremainder x 16))) ; = modulo
            (loop (+ i increment-size) (- j 1) (+ k 1)))
          str))))

(define* (u8vector->hex-string u8vect (use-capitals? #t) (separate-by-spaces? #t))
  (check-arg u8vector? u8vect u8vector->hex-string)
  (subu8vector->hex-string u8vect
                           0
                           (u8vector-length u8vect)
                           use-capitals? separate-by-spaces?))

(define* (hex-string->u8vector s (separated-by-spaces? #t))
  (check-arg string? s hex-string->u8vector)
  (let* ((l (string-length s)))
    (if separated-by-spaces?
        (if (not (zero? (fxremainder (fx+ l 1) 3))) (error "String length +1 not multiple of three" s))
        (if (not (zero? (fxremainder l 2))) (error "String length not multiple of two" s))) ; = modulo
    (let* ((c (if separated-by-spaces?
                  (fxquotient (fx+ l 1) 3)
                  (fxquotient l 2)))
           (v (make-u8vector c))
           (increment-size (if separated-by-spaces? 3 2)))
      (let loop ((i 0))
        (let* ((m (fx* i increment-size))
               (a (hex-char->integer (string-ref s m)))
               (b (hex-char->integer (string-ref s (+ m 1))))
               (n (fx+ (* a 16) b)))
          (u8vector-set! v i n)
          (let ((i (+ i 1)))
            (if (not (eq? i c))
                (loop i)))))
      v)))

;;! This procedure normalizes every CR, LF and CR LF to the specified encoding.
;; This is quite neat as it effectively autodetects the line encoding of the input.
;; This stuff is needed for instance for properly "formatting"/"escaping" the DATA block in the SMTP
;; protocol, where sporadic CR:s lead to data corruption.
;; Please note that if no mangling was applied, the original object reference is passed back.
;; to-encoding = 'cr 'lf 'cr-lf
;; This procedure is mirrored by string-normalize-eol:s of the string/util module.
;; Tests:
;; These tests are intendedly made so that |all-src-eol:s-of-target-encoding-already?| is switched to #f
;; and thus the entire 
(define* (u8vector-normalize-eol:s u8v (to-encoding 'cr-lf))
  (check-arg u8vector? u8v u8vector-normalize-eol:s)
  ;; CR = ASCII 13, LF = ASCII 10
  (let* ((src-length (u8vector-length u8v))
         (src-length-1 (fx- src-length 1))
         (to-eol-length (case to-encoding
                          ((cr   ) 1)
                          ((lf   ) 1)
                          ((cr-lf) 2)
                          (else    (error "Invalid to-encoding" to-encoding))))
         (all-src-eol:s-of-target-encoding-already? #t))
    (let loop ((src-at 0)
               ;; Counting this one as src-length + number of linebreaks would not be so neat, as
               ;; we may have linebreaks of any length in the source data.
               (dst-length 0)
               (iterating-from-src-at 0)
               (intervals '()))
      (define (intervals-add)
        (cons (cons iterating-from-src-at src-at)
              intervals))
      (if (fx< src-at src-length)
          ;; Continue
          (let* ((b (u8vector-ref u8v src-at))
                 (src-at+1 (fx+ src-at 1)))
            (case b
              ((13)
               ;; Are we at the last position in the source?
               (if (eq? src-at src-length-1)
                   ;; Yes - this means the next character cannot be a LF, so we should conclude we detected a break here.
                   (begin
                     (if (not (eq? to-encoding 'cr)) (set! all-src-eol:s-of-target-encoding-already? #f))
                     (loop src-at+1
                           (fx+ dst-length to-eol-length)
                           src-at+1
                           (intervals-add)))
                   ;; No - this means we should check if the next character is a LF, and have differentiated behavior based on that.
                   (let* ((b2 (u8vector-ref u8v src-at+1)))
                     (if (eq? b2 10)
                         ;; We did hit a CRLF sequence - register.
                         (begin
                           (if (not (eq? to-encoding 'cr-lf)) (set! all-src-eol:s-of-target-encoding-already? #f))
                           (let ((src-at+2 (fx+ src-at+1 1)))
                             (loop src-at+2
                                   (fx+ dst-length to-eol-length)
                                   src-at+2
                                   (intervals-add))))
                         ;; We hit a sporadic CR - register.
                         (begin
                           (if (not (eq? to-encoding 'cr)) (set! all-src-eol:s-of-target-encoding-already? #f))
                           (loop src-at+1
                                 (fx+ dst-length to-eol-length)
                                 src-at+1
                                 (intervals-add)))))))
              ((10)
               ;; We hit a sporadic LF - register.
               (if (not (eq? to-encoding 'lf)) (set! all-src-eol:s-of-target-encoding-already? #f))
               (loop src-at+1
                     (fx+ dst-length to-eol-length)
                     src-at+1
                     (intervals-add)))
              (else
               (loop src-at+1
                     (fx+ dst-length 1)
                     iterating-from-src-at
                     intervals))))
          ;; Done processing
          (if all-src-eol:s-of-target-encoding-already? ; No need to add (or ... (null? r)) here as that is implicit to the logic.
              u8v
              ;; We now have the |r| structure with all the intervals in the source for the sequences between lines,
              ;; and we have length of the to be made result in |dst-length|.
              (let* ((intervals
                      (intervals-add))
                     ;; We add the finishing interval here. The point with this is that if there was no interval up to now,
                     ;; then we need to create an interval to cover all the content, and in all cases all intervals made up
                     ;; to this point were only up to the last CRLF anyhow.
                     ;;      And last, if the u8v ended with an EOL then we may have a zero-length interval here now however
                     ;; that is cool as the point then with this interval is to make that EOL be inserted into the result.
                     ;; For simplicity we do the result generation in start to end order,
                     ;; and that requires us to reverse the order of the intervals here.
                     (intervals (reverse intervals))
                     (result (make-u8vector dst-length))
                     (first-interval      (car intervals))
                     (remaining-intervals (cdr intervals))
                     (first-interval-from (car first-interval))
                     (first-interval-to   (cdr first-interval)))
                ;; Copy the first interval.
                (subu8vector-move! u8v
                                   first-interval-from ; = 0
                                   first-interval-to
                                   result
                                   0)
                ;; Copy the next intervals, and for each such interval, insert the EOL before it.
                (let loop ((result-at first-interval-to)
                           (intervals remaining-intervals))
                  (if (not (null? intervals))
                      (begin
                        (u8vector-set! result result-at (if (eq? to-encoding 'lf) 10 13))
                        (set! result-at (fx+ result-at 1))
                        (if (eq? to-encoding 'cr-lf)
                            (begin
                              (u8vector-set! result result-at 10)
                              (set! result-at (fx+ result-at 1))))
                        (let* ((interval (car intervals))
                               (remaining-intervals (cdr intervals))
                               (interval-from (car interval))
                               (interval-to   (cdr interval)))
                          (subu8vector-move! u8v
                                             interval-from
                                             interval-to
                                             result
                                             result-at)
                          (loop (fx+ result-at (fx- interval-to interval-from))
                                remaining-intervals)))
                      ;; Done.
                      result))))))))

