;; Universal HTTP client library
;; Copyright (C) 2013 Mikael More
;; MIT license
;;
;; This module is intended to provide HTTP client functionality for any purpose, including ability
;; to run HTTP over custom data channels such as SSL thus providing HTTPS support. It is inspired by
;; XmlHttpRequest's calling convention, and by Rack's lowlevel approach to delivering the protocol.
;;
;; Emphasis is on programmatic control over the ongoing HTTP activity on the one hand, and on
;; protocol support on the other hand.
;;
;; This module does currently not regard timeouts as those are currently handled by the
;; IO facility of IO primitives.
;;
;; ## HTTP version support note: 1.1 and 1.0 only.
;; Per inspiration from libCurl, we do not support making HTTP 0.9 requests. Maintaining support for
;; it would be easy, but, it's old since 1.0's release in 1996 and nobody uses it and it would add
;; another design specification of which we have sufficient numbers anyhow.
;;
;; ## Related documents
;; # Related standards documents
;; HTTP 1.0: https://tools.ietf.org/html/rfc1945
;; HTTP 1.1: https://tools.ietf.org/html/rfc2616
;;
;; W3C XmlHttpRequest specification: http://www.w3.org/TR/XMLHttpRequest2/
;;
;; # Other HTTP client implementations
;; Common Lisp HTTP client Drakma at https://github.com/edicl/drakma .
;;
;; LibCurl's HTTP handling is at in the Curl_Http procedure at
;; https://github.com/bagder/curl/blob/master/lib/http.c - too heavy read though.
;;
;; Equally Google's Java HTTP client library is too heavy read,
;; https://code.google.com/p/google-http-java-client/source/browse/#hg%2Fgoogle-http-client%2Fsrc%2Fmain%2Fjava%2Fcom%2Fgoogle%2Fapi%2Fclient%2Fhttp
;;
;; ## Method of sending HTTP request body contents
;; We rely only on passing a Content-Length or Transfer-Encoding: chunked header for the server to
;; understand how long the passed body contents are.
;;
;; Theoretically we could go with Connection: close and close the *outbound* TCP connection channel
;; after having sent the body as for the server to receive the EOF understanding it reached the end
;; of the data, and then sending the HTTP response on the channel for the other direction which is
;; still open.
;;      It ought to be probable that even while this is completely in the TCP spec, there should
;; be servers that don't comply with this but perceive the HTTP connection as failed if it receives
;; an EOF, so therefore we do not rely on this method.
;;
;; ## Development background
;; This module replaces a http-client module previously bundled with Sack. It had the issues that
;;
;;  * Requests could not be aborted once initiated and there were no introspection features
;;  * It tended to block and any error in requests made, including |thread-terminate!| on any involved
;;    thread, tended to end up blocking any more requests to at least the domain they belonged to
;;  * Pipelining was always on, meaning it was not suitable for 'sensitive' HTTP requests
;;  * The code was hard to read and a general sense of absence of guidance in case of problems,
;;    for instance because the HTTP protocol handling and interaction code was mixed together completely
;;    with the connection pool/thread/work coordination code.
;;  * The 'root' coordinating structure was not replacable so only one 'sphere' in total and any
;;    error with it would thus affect all http requests made globally
;;
;; This module is intended to include resolution of these issues.
;;
;; While this module is completely new as a structure, it does reuse shorter and longer fragments of
;; code from the earlier http-client module.
;;
;; One design aspect is that the old http-client relied on the Gambit IO model where IO error led
;; to exception, while this module relies on IO primitives returning #f in case of error. This
;; provides basis for more fine grained error monitornig with more concise and slightly faster code.
;;
;; ## Exports


;; (http-client-sphere-make #!key
;;                          (req-in-current-thread?             #t) ; Boolean, #t = by default perform the HTTP request in the caller's thread.
;;                          (uri-schemes-inherit-from-sphere   http-client-dfl-sphere)
;;                          (max-connections                   http-client-default-max-connections)
;;                          (max-connections-per-host          http-client-default-max-connections-per-host)
;;                          (max-pipelined-reqs-per-connection http-client-default-max-pipelined-reqs-per-connection)
;;                          (protocol                          'HTTP/1.1) ; Default HTTP protocol setting. Can be either of 'HTTP/1.1 'HTTP/1.0
;;                          (pipeline?                         #f) ; Boolean, #t = do pipeline HTTP requests
;;                          (follow-redirects?                 #t) ; Boolean, #t = follow redirects, #f = do not follow redirects
;;                          (follow-redirects-limit            15) ; Follow up to 15 redirects to reach the target content
;;                          ; This is implicit functionality in every HTTP protocol, let's not bother making an option of it.
;;                          ; (generate-host-header              #t) ; Boolean, #t = automatically add a Host: header for the hostname specified in the request URI.
;;                          ;                                        ; (This is required by the HTTP/1.1 specification.)
;;                          (record-response                   http-client-default-record-response) ; This is only a conveniency mechanism complementing the on-data callback,
;;                                                                                                  ; that receives all received HTTP response body data.
;;                                                                                                  ;
;;                                                                                                  ; #f or 0 = Mechanism disabled.
;;                                                                                                  ; fixnum = Record up to this many bytes of response in the http-client-request object.
;;                          (keepalive                         http-client-default-keepalive) ; Keep HTTP connections open for up to 15 seconds after the most
;;                                                                                            ; recent HTTP request response was completely received.
;;                          )

;; => http-client-sphere, which is a closure that takes an argument and in response provides..

;; 'add-uri-scheme! => lambda (scheme make-io-primitives) => #!void

;; 'close! => lambda () => #!void

;; (http-client-request-make uri
;;                           #!key

;;                           ; Settings
;;                           (sphere                 http-client-dfl-sphere) ; The http-client-sphere within which to make the request
;;                           (req-in-current-thread? 'inherit)
;;                           (protocol               'inherit)
;;                           (pipeline?              'inherit)
;;                           (follow-redirects?      'inherit)
;;                           (follow-redirects-limit 'inherit)
;;                           ; (generate-host-header   'inherit), commented out for the same reason it's commented out above.
;;                           (keepalive              'inherit) ; If request is HTTP/1.1, then, this value specifies how much time
;;                                                             ; the HTTP connection will be kept alive after this request is over.
;;                           (record-response        'inherit)

;;                           ; Request contents
;;                           (method                 "GET") ; HTTP method used, typically one of "GET" "POST" "HEAD" "PUT" "DELETE" "OPTIONS"
;;                           http-username
;;                           http-password
;;                           (headers                '()  ) ; Alist of (key . value) where both key and value are string.
;;                           request-body                   ; May be either of
;;                                                          ;  * u8vector
;;                                                          ;  * string (if so gets UTF-8-encoded)
;;                                                          ;  * alist in which case it gets x-www-form-urlencoded and UTF-8-encoded. This is to provide
;;                                                          ;    a simple way to submit HTML-style HTTP POST forms.
;;                                                          ;  * #f = use |request-body-feed| to get the request body instead.
;;                           request-body-feed              ; Procedure that gets invoked to provide request body data, if the request-body value is #f.
;;                                                          ; The purpose is to provide a way to send large request bodies, without needing to keep their
;;                                                          ; content in RAM at once.
;;                                                          ;
;;                                                          ; Takes no arguments. May return either of:
;;                                                          ;  * u8vector                = Feed this u8vector's contents in full.
;;                                                          ;  * string                  = Feed this string's contents in full, in UTF-8 encoded form.
;;                                                          ;  * (values u8v u8v-length) = Feed the first u8v-length bytes of u8v's contents.
;;                                                          ;  * #f                      = The end of the request body has been reached and this procedure
;;                                                          ;                              should not be invoked again.
;;                                                          ;
;;                                                          ; When a subsequent call to request-body-feed is done, http-client will not access data provided
;;                                                          ; by earlier calls anymore.

;;                           ; Response handlers
;;                           on-result-or-stage-change      ; Invoked when result or stage change. Takes args (result stage).

;;                           on-load-start                  ; Invoked at the start of request processing, no args

;;                           on-http-status&headers         ; When got the HTTP response status code and headers were received. Takes the args (status-code headers).

;;                           on-data                        ; Invoked at the response of a chunk of HTTP request response body data. Takes args
;;                                                          ; (u8v u8v-length last-call-within-block?) where the u8v contains the data passed and u8v-length
;;                                                          ; describes how many bytes of data in u8v that are used. If copying? = #t, all of u8v's contents
;;                                                          ; is data received now and u8v-length is equal to (u8vector-length u8v).
;;                                                          ;
;;                                                          ; on-data invocations are done in sequences on a per block basis. If we receive the response body in chunked
;;                                                          ; encoding, then the chunk is the block, otherwise the entire response body is the block. On the last
;;                                                          ; on-data invocation within a block, last-call-within-block? is #t, otherwise #f.

;;                           on-load                        ; Invoked when HTTP request response retrieval completed successfully, no args
;;                           on-error                       ; Invoked when HTTP request failed, no args
;;                           on-abort                       ; Invoked when HTTP request was aborted, no args
;;                           on-timeout                     ; Invoked when HTTP request failed because of timeout, no args

;;                           on-load-end                    ; Invoked at the end of request processing, independent of if failed/aborted/successful, no args
;;                           )
;; => request

;; Note that an XHR-style status value ('opened (1), 'headers-received (2), 'loading (3), 'done (4))
;; can be generated from result and stage. Note that statuses 2-4 are only for when the final
;; destination of the redirect chain has been reached.

;; request is a closure that takes an argument and in response provides...

;; 'result => lambda () = Returns the result of the HTTP request attempt.
;;                      => #f = Request unfinished, see 'stage for in which phase it is.
;;                         #t = HTTP request was successfully performed (and stage is 'done)

;;                         Neither #t nor #f means error, and the stage at which the error happened is in stage:

;;                         'aborted = HTTP request was aborted using the 'abort! call or 'close! on the parent sphere.
;;                         'timeout = HTTP request timed out
;;                         'error   = Other error made completion of HTTP request response fail.

;; 'stage => lambda () = The stage of performing the HTTP request reached.

;;                        Please note that if following a redirect chain, the entire process from 'acquiring-slot
;;                        to 'done will be repeated until reaching the final destination, or failing.

;;                     => 'created         = Request created.
;;                        'acquiring-slot  = Acquiring a HTTP connection slot for performing the HTTP request on.
;;                                           (There are limits on how many HTTP connections can be done in total and per
;;                                           host concurrently, so this can take time.)
;;                                           Please note that this step regards the acquisition of a slot for doing HTTP
;;                                           communication, and not actually doing any communication, this is done at first
;;                                           in subsequent steps.
;;                        'connect         = Connect to remote host if not connected already, and successfully send the HTTP
;;                                           method as to ensure that the connection works. If this was a re-use of a present
;;                                           HTTP connection and sending the HTTP method did not work, then a re-connect
;;                                           is done. (Slot acquired.)
;;                        'send-headers    = Sending the URL, protocol version and headers of the HTTP request. (Connected.)
;;                        'send-body       = Sending the HTTP request body. (Headers sent.)
;;                        'receive-headers = Receiving the status code and headers of the HTTP response. (Body sent.)
;;                        'receive-body    = Receiving the HTTP response body. (Headers received.)
;;                        'done            = HTTP request performed completely.

;; 'reached-uri => lambda () => uri = Returns the URI that the last HTTP request in following the redirect chain was
;;                                    made to. If result = #t, then it's the URI finally directed to in the chain.

;; 'status-code => lambda () => #f = not HTTP status code returned yet, or integer HTTP status code returned e.g. 200

;; ; Not needed: 'status-text => lambda () => #f = no HTTP status text returned yet, or string HTTP status text returned e.g. "OK"

;; 'headers => lambda () => #f = no HTTP headers received yet, or alist where key and value are string.

;; 'header => lambda (name) => #f = no HTTP header by that name was found, or string = the value of that HTTP response header.

;; 'response => lambda (#!optional (as-text? 'auto)) = Procedure that picks up response content depending on the record-response conveniency setting.
;;                                                     => #f = no response received yet, or string or u8vector = the response.

;; 'abort! => lambda () = Procedure that aborts the HTTP request with immediate effect.
;;                        => #!void

;; 'wait-for-result => lambda () = Waits for the HTTP request to reach non-#f result, and returns result.
;;                                 => same result as of the 'result command.

;; 'wait-for-stage => lambda (#!optional (for-stage 'done)) => #!void

;; 'all-response => lambda () => list (reached-uri result stage status-code headers get-header response)
;;                               where the respective field corresponds to the result of the closure with the respective name above.

;; ; Procedures exported but that should not be needed to be used:
;; (http-connect host port with-io-primitives
;;               #!key
;;               (threadsafe?          #f)
;;               (protocol             'HTTP/1.1)
;;               (pipeline?            #f)
;;               ; (generate-host-header 'inherit), commented out for the same reason it's commented out above.
;;               )

;; => http-connection.

;; |http-connect| provides the most basic access to the HTTP protocol: On invocation, it opens a
;; TCP connection to the specified host, and when it gets a 'request command it performs a
;; HTTP request on the connection.

;; Part of the purpose here is that the entire 'request call is performed within the current
;; thread only, meaning that responsivity is kept good as no roundtrips through the thread
;; scheduler are needed.

;; At the same time, pipelining is supported through that the same http-connect object becomes usable
;; for other threads when it not needs to be locked for the current thread. In pipelining mode,
;; there is a 'write' and a 'read' lock and first the write lock is locked, the request is sent, the
;; lock is unlocked, the read lock is locked, the response is read, and the lock is unlocked.


;; http-connection is a closure that takes an argument and in response provides...

;; 'request => lambda (uri
;;                     #!key
;;                     ; Settings

;;                     ; Note, no req-in-current-thread? option here as it's implicit that the 'perform
;;                     ; closure performs its job in the current thread.

;;                     (copying?               #t   ) ; #t = Data passed to on-data should be unique u8vectors
;;                                                    ; #f = Data passed to on-data will be the same u8vector between calls but with other content

;;                     (protocol               'HTTP/1.1)
;;                     (keepalive              http-client-default-keepalive      ) ; If request is HTTP/1.1, then, this value specifies how much time
;;                                                                                 ; the HTTP connection will be kept alive after this request is over.
;;                     (record-response        http-client-default-record-response)

;;                     ; Request contents
;;                     (method                 "GET") ; HTTP method used, typically one of "GET" "POST" "HEAD" "PUT" "DELETE" "OPTIONS"
;;                     http-username
;;                     http-password
;;                     (headers                '()  )
;;                     request-body                   ; May be either of
;;                                                    ;  * u8vector
;;                                                    ;  * string (if so gets UTF-8-encoded)
;;                                                    ;  * alist in which case it gets x-www-form-urlencoded and UTF-8-encoded. This is to provide
;;                                                    ;    a simple way to submit HTML-style HTTP POST forms.
;;                                                    ;  * #f = use |request-body-feed| to get the request body instead.
;;                     request-body-feed              ; Procedure that gets invoked to provide request body data, if the request-body value is #f.
;;                                                    ; The purpose is to provide a way to send large request bodies, without needing to keep their
;;                                                    ; content in RAM at once.
;;                                                    ;
;;                                                    ; Takes no arguments. May return either of:
;;                                                    ;  * u8vector                = Feed this u8vector's contents in full.
;;                                                    ;  * string                  = Feed this string's contents in full, in UTF-8 encoded form.
;;                                                    ;  * (values u8v u8v-length) = Feed the first u8v-length bytes of u8v's contents.
;;                                                    ;  * #f                      = The end of the request body has been reached and this procedure
;;                                                    ;                              should not be invoked again.
;;                                                    ;
;;                                                    ; When a subsequent call to request-body-feed is done, http-client will not access data provided
;;                                                    ; by earlier calls anymore.

;;                     ; Response handlers
;;                     on-result-or-stage-change      ; Invoked when result or stage change. Takes args (result stage).

;;                     on-load-start                  ; Invoked at the start of request processing, no args

;;                     on-http-status&headers         ; When got the HTTP response status code and headers were received. Takes the args (status-code headers).

;;                     on-data                        ; Invoked at the response of a chunk of HTTP request response body data. Takes args
;;                                                    ; (u8v u8v-length last-call-within-block?) where the u8v contains the data passed and u8v-length
;;                                                    ; describes how many bytes of data in u8v that are used. If copying? = #t, all of u8v's contents
;;                                                    ; is data received now and u8v-length is equal to (u8vector-length u8v).
;;                                                    ;
;;                                                    ; on-data invocations are done in sequences on a per block basis. If we receive the response body in chunked
;;                                                    ; encoding, then the chunk is the block, otherwise the entire response body is the block. On the last
;;                                                    ; on-data invocation within a block, last-call-within-block? is #t, otherwise #f.

;;                     on-load                        ; Invoked when HTTP request response retrieval completed successfully, no args
;;                     on-error                       ; Invoked when HTTP request failed, no args
;;                     on-abort                       ; Invoked when HTTP request was aborted, no args
;;                     on-timeout                     ; Invoked when HTTP request failed because of timeout, no args

;;                     on-load-end                    ; Invoked at the end of request processing, independent of if failed/aborted/successful, no args
;;                     )
;;             => request

;;             request is a closure that takes an argument and in response provides the closures described in the http-request section above,
;;             and in addition to them:

;;             'perform => lambda () = Performs the HTTP request, and returns when it has been fully performed.
;;                                     => same return value as 'all-response returns.

;; 'close => lambda () = Closes the HTTP connection with immediate effect.




;;
;; ## TODO
;;  * ** The |abort!| procedures do currently not abort HTTP requests currently being made, but only
;;    cancelling nextfollowing requests. This is as we're waiting for learning to know a best
;;    practice for making interrupts to Gambit IO primitives (read/write).
;;  * Possible development is to add a GC argument that specificies if request/-s should GC
;;    as usual or if a request should be hindered from GC:ing while active just as is the case for
;;    threads.
;;  * On the fly Gzip coding support.
;;  * SOCKS 5 proxy support
;;  * When uri-scheme is "http" and HTTP connection is at the first request, instead of sending
;;    method we can just do ##wait-for-output-io.
;;  * On all relevant places, check that the HTTP request was received in full correctly:
;;    If content-length is set then check based on that, and if chunked encoding is used
;;    then check that the last chunk was indeed zero-length. (Combine both checks if both are set?)
;;



;;-------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------




(declare (standard-bindings) (extended-bindings) (fixnum))


;; ## Configuration

(define http-client-verbose?                                  #f)

;; We keep this one not too high
(define http-client-default-max-connections                   32)

(define http-client-default-max-connections-per-host          8)

(define http-client-default-max-pipelined-reqs-per-connection 10)

(define http-client-default-record-response                   10485760)

(define http-client-default-keepalive                         15)

;; ## Constants


;; ## Utils

(define-macro (with-http-client-all-response all-response . code)
  `(apply (lambda (reached-uri result stage status-code headers get-header response) ,@code) ,all-response))

;; (There's a macro-based variant of this one at hand by the name |with-list-maker|. Let's keep
;; this module as free from macro use as can be done though, for as to keep general compatibility.)
(define make-list-maker
  (let ((unspecified (make-uninterned-symbol "")))
    (lambda ()
      (let* ((list '()) (last-element #f)
             (cons! (lambda (v) (let ((c (cons v '())))
                             (if last-element
                                 (set-cdr! last-element c)
                                 (set! list c))
                             (set! last-element c)))))
        (lambda* ((v unspecified))
          (if (eq? v unspecified)
              list
              (cons! v)))))))

;; Coincides with version-table of the server module.
(define http-response-version-table (make-token-table
                                     ("HTTP/1.0" 'HTTP/1.0)
                                     ("HTTP/1.1" 'HTTP/1.1)))

;; Example input: "HTTP/1.1 200 OK"
;;
;; => #f = invalid input, or
;;    values (http-version status-code)
(define (parse-http-response-status-line str)
  (let* ((len (string-length str))
         (hit-space-at #f)
         (start
          (let loop ((start 0))
            (if (char=? (string-ref str start) #\space)
                (set! hit-space-at start)
                #f)
            (cond
             ((and hit-space-at
                   (char-numeric? (string-ref str start)))
              start)
             ((< start len)
              (loop (+ 1 start)))
             (else
              start))))
         (http-version (and hit-space-at
                            (token-table-lookup-substring http-response-version-table
                                                          str
                                                          0 hit-space-at))))
    (and http-version ; http-version being set implies that hit-space-at is set
         (let ((status-code (string->number (substring str start (min (+ start 3) len)))))
           (values (vector-ref http-response-version-table (+ http-version 1)) ; This reflects how the server module does the same.
                   status-code)))))

;; ## Global scheme registration
;; (Spheres' schemes override this one.)
(define http-client-schemes `(("http" . ,make-standard-primitives)))

(define (http-client-scheme-add! scheme make-io-primitives)
  (set! http-client-schemes `(,@http-client-schemes
                              (,scheme . ,make-io-primitives))))

;; => make-io-primitives, or #f = scheme not found
(define (http-scheme-resolve scheme)
  (al-get http-client-schemes scheme #f))

;; ## |http-connect|
;; Please note that |http-connect| is not aware what scheme URI:s will have, and thus requires any
;; |http-scheme-resolve| call to be made by the user.
(define* (http-connect host port
                       (make-io-primitives: #f)
                       (threadsafe?: #f)
                       (protocol: 'HTTP/1.1)
                       (pipeline?: #f))
  (define-macro (dbg . a) #!void)
  (define-macro (dbg* . a) `(if http-client-verbose? (dbg "http-connection: " ,@a)))
  (define-macro (receive from-code args . code) `(call-with-values (lambda () ,from-code) (lambda ,args ,@code)))
  (let* ((make-io-primitives (or make-io-primitives make-standard-primitives))
         (gambit-port (open-tcp-client (string-append host ":" (number->string port)) ))
         (io-primitives (make-io-primitives gambit-port http-client-default-keepalive))
         (gambit-write-subu8vector write-subu8vector)
         (display           (io-primitives-display           io-primitives))
         (write-subu8vector (io-primitives-write-subu8vector io-primitives))
         (read-subu8vector  (io-primitives-read-subu8vector  io-primitives))
         (force-output      (io-primitives-force-output      io-primitives))
         (close-port        (io-primitives-close-port        io-primitives))
         ;; When IO primitive results are speced to return #f only on failure, change these routines accordingly.
         (display           (lambda (s) (not (memq (display s) '(#f #!eof)))))
         (read-subu8vector  (lambda* (u8v start end (need 1)) (not (memq (read-subu8vector u8v start end need) '(#f #!eof)))))
         (write-subu8vector (lambda (u8v start end) (not (memq (write-subu8vector u8v start end) '(#f #!eof)))))
         (force-output      (lambda () (not (memq (force-output) '(#f #!eof)))))
         (close-port        (lambda () (not (memq (close-port) '(#f #!eof))))))
    (lambda (command)
      (case command
        ((request)
         (lambda* (uri
              ;; Settings
              ;; Note, no req-in-current-thread? option here as it's implicit that the 'perform
              ;; closure performs its job in the current thread.
              (copying?: #t) ; #t = Data passed to on-data should be unique u8vectors
              ;; #f = Data passed to on-data will be the same u8vector between calls but with other content
              (protocol: 'HTTP/1.1)
              ;; If request is HTTP/1.1, then, this value specifies how much time
              ;; the HTTP connection will be kept alive after this request is over.
              (keepalive: http-client-default-keepalive)
              (record-response: http-client-default-record-response)
              ;; Request contents
              (method: "GET") ; HTTP method used, typically one of "GET" "POST" "HEAD" "PUT" "DELETE" "OPTIONS"
              (http-username: #f)
              (http-password: #f)
              (headers: '() )
              ;; May be either of
              ;;  * u8vector
              ;;  * string (if so gets UTF-8-encoded)
              ;;  * alist in which case it gets x-www-form-urlencoded and UTF-8-encoded. This is to provide
              ;;    a simple way to submit HTML-style HTTP POST forms.
              ;;  * #f = use |request-body-feed| to get the request body instead.
              (request-body: #f)
              ;; Procedure that gets invoked to provide request body data, if the request-body value is #f.
              ;; The purpose is to provide a way to send large request bodies, without needing to keep their
              ;; content in RAM at once.
              ;;
              ;; Takes no arguments. May return either of:
              ;;  * u8vector                = Feed this u8vector's contents in full.
              ;;  * string                  = Feed this string's contents in full, in UTF-8 encoded form.
              ;;  * (values u8v u8v-length) = Feed the first u8v-length bytes of u8v's contents.
              ;;  * #f                      = The end of the request body has been reached and this procedure
              ;;                              should not be invoked again.
              ;;
              ;; When a subsequent call to request-body-feed is done, http-client will not access data provided
              ;; by earlier calls anymore.
              ;;
              ;; Response handlers
              (request-body-feed: #f)
              ;; Invoked when result or stage change. Takes args (result stage).
              (on-result-or-stage-change: #f)
              ;; Invoked at the start of request processing, no args
              (on-load-start: #f)
              ;; When got the HTTP response status code and headers were received. Takes the args (status-code headers).
              (on-http-status&headers: #f)
              ;; Invoked at the response of a chunk of HTTP request response body data. Takes args
              ;; (u8v u8v-length last-call-within-block?) where the u8v contains the data passed and u8v-length
              ;; describes how many bytes of data in u8v that are used. If copying? = #t, all of u8v's contents
              ;; is data received now and u8v-length is equal to (u8vector-length u8v).
              ;;
              ;; on-data invocations are done in sequences on a per block basis. If we receive the response body in chunked
              ;; encoding, then the chunk is the block, otherwise the entire response body is the block. On the last
              ;; on-data invocation within a block, last-call-within-block? is #t, otherwise #f.
              (on-data: #f)
              ;; Invoked when HTTP request response retrieval completed successfully, no args
              (on-load: #f)
              ;; Invoked when HTTP request failed, no args
              (on-error: #f)
              ;; Invoked when HTTP request was aborted, no args
              (on-abort: #f)
              ;; Invoked when HTTP request failed because of timeout, no args
              (on-timeout: #f)
              ;; Invoked at the end of request processing, independent of if failed/aborted/successful, no args
              (on-load-end: #f))
             (define result #f)
             (define stage  #f)
             (define (result&stage! result* stage*)
               (set! result result*)
               (set! stage  stage*)
               (if on-result-or-stage-change (on-result-or-stage-change result stage)))
             (define (result! result*)
               (set! result result*)
               (if on-result-or-stage-change (on-result-or-stage-change result stage)))
             (define (stage! stage*)
               (set! stage  stage*)
               (if on-result-or-stage-change (on-result-or-stage-change result stage)))
             (define status-code #f)
             (define response-headers #f)
             (define (get-header name)
               (and headers (al-get response-headers name #f)))
             (define recorded-response #f)
             (define recorded-response-bytes 0)
             (define* (response (as-text? 'auto))
               (let ((u8v (and recorded-response (get-output-u8vector recorded-response))))
                 ;; Refine to check content type for if binary or text content.
                 (and u8v (case as-text?
                            ((auto) (utf8-u8vector->string u8v)) ; Current behavior, we may very well refine it.
                            ((#t  ) (utf8-u8vector->string u8v))
                            ((#f  ) u8v)
                            (else (error "Unknown response as-text? setting" as-text?))))))
             (define* (fail (to-result 'error))
               (dbg* "HTTP request failed.")
               (result! to-result))
             ;; Possibility: To set the port's timeout to timeout here.
             (if (not (uri? uri)) (set! uri (string->uri uri)))
             (lambda (command)
               (case command
                 ((perform)
                  (lambda ()
                    (dbg* "perform invoked.")
                    ;; When invoked by http-request, these three should be omitted.
                    (if on-load-start (on-load-start))
                    (result&stage! #f 'created)
                    (stage! 'acquiring-slot)
                    (result&stage! #f 'connect)
                    (dbg* "Displaying method: " method)
                    ;; To keep things as basic as possible we send only the method here and not the
                    ;; preceding space. We could even just have sent the first letter of the method.
                    ;;
                    ;; The reason we want to send anything at all, if because Unix IO tends to require
                    ;; IO to actually be done (a write or read) in order to signal EOF.
                    (if (not (and (display method)
                                  ;; XXX TODO: Doing only force-output level 0 here is enough. We just want the IO
                                  ;; system and OS to clarify to us that the connection is alive. We don't actually
                                  ;; need it to be submitted as a separate ethernet packet to the other party.
                                  (force-output)))
                        (fail)
                        (begin
                          (dbg* "Success. Processing headers.")
                          ;; We understand that the HTTP connection has been proven to work.
                          ;; Now is our time to send the HTTP request headers.
                          (stage! 'send-headers)
                          ;; These though, are also a prelude for the submission of HTTP request body data. For this reason, we need
                          ;; to know as much as is needed about the request body data as to be able to provide headers that properly
                          ;; prepare the server for its reception.
                          ;;
                          ;; We can send the request body as a solid block of data given that we provided a Content-Length header
                          ;; telling how many bytes long it is. Alternatively, presuming the request is in the HTTP/1.1 protocol
                          ;; (which is generally the case), we would not need to know the length of the content if we go with a
                          ;; Transfer-Encoding: chunked header. Though, use of such a header would mean that the receiving
                          ;; server cannot track how many % into the reception it is (wanting to do this is extremely unusual though),
                          ;; though to keep any involved logics as easy as possible, let's not use chunked encoding unless needed.
                          ;;
                          ;; Also, the caller may specify a Content-Length or Transfer-Encoding already, which if passed, we will
                          ;; respect and adapt the format we send the request body in to.
                          ;;
                          ;; (For some additional notes, see the "Method of sending HTTP request body contents" section in the header
                          ;; comments.)
                          ;;
                          ;; So, our job now here is to determine Content-Length and Transfer-Encoding header contents.
                          (receive
                           (let* ((content-length-header          (al-getq headers 'content-length    #f))
                                  (transfer-encoding-header       (al-getq headers 'transfer-encoding #f))
                                  (send-body-in-chunked-encoding? (equal? transfer-encoding-header "chunked"))
                                  (request-body-u8v
                                   (cond ((not       request-body) #f)
                                         ((u8vector? request-body) request-body)
                                         ((string?   request-body) (string->utf8-u8vector request-body))
                                         ((pair?     request-body) (string->utf8-u8vector
                                                                    (encode-x-www-form-urlencoded/UTF-8 request-body)))
                                         (else                     (error "request-body has unknown format" request-body))))
                                  ;; In case we have no request-body-u8v content, then attempt to make a read from |request-body-feed|
                                  ;; as to check if this HTTP request has a request body at all. The purpose with knowing if it has,
                                  ;; is that if it hasn't, we need to bother neither about passing a Content-Length nor about passing
                                  ;; Transfer-Encoding: chunked .
                                  (request-body-feed/first-result (and (not request-body-u8v)
                                                                       request-body-feed
                                                                       (request-body-feed)))
                                  ;; Actually, if we're in HTTP/1.0 then there's no Transfer-Encoding: chunked support, so we
                                  )
                             (cond
                              ;; If the user has passed a Content-Length or/and a Transfer-Encoding header, then we simply understand they're
                              ;; correct and proceed with them, and apply the user-provided Transfer-Encoding setting (whether on or off).
                              ((or content-length-header transfer-encoding-header)
                               (values headers
                                       request-body-u8v
                                       request-body-feed/first-result
                                       send-body-in-chunked-encoding?))
                              ;; We get here in the case that the user not has provided any Content-Length or/and Transfer-Encoding header.
                              ;;
                              ;; If no HTTP request body is being submitted in this HTTP request, indeed absence of a Content-Length and
                              ;; Transfer-Encoding header is exactly as it should be, so if that is how it is let's just proceed also.
                              ((and (not request-body-u8v) (not request-body-feed/first-result))
                               (values headers
                                       request-body-u8v ; (=#f.)
                                       request-body-feed/first-result ; (=#f.)
                                       send-body-in-chunked-encoding?))  ; (=#f.)
                              ;; We get here when the user has not provided any Content-Length or/and Transfer-Encoding header, but,
                              ;; we do have content to submit.
                              ;;
                              ;; First, in case the request is done in HTTP/1.0 then there's no Transfer-Encoding option available,
                              ;; so let's read all the request body into request-body-u8v and describe the body length to the server
                              ;; using a Content-Length header, which is the only way to do that in HTTP/1.0 .
                              ((eq? protocol 'HTTP/1.0)
                               (let ((request-body-u8v (or request-body-u8v
                                                           (error "Wrapping of request-body feed to request-body-u8v not implemented!") ; TODO
                                                           )))
                                 (values `((content-length . ,(number->string (u8vector-length request-body-u8v)))
                                           . ,headers)
                                         request-body-u8v
                                         #f ; request-body-feed/first-result
                                         #f)))  ; send-body-in-chunked-encoding?
                              ;; We get here when the user has not provided any Content-Length or/and Transfer-Encoding header, but,
                              ;; we do have content to submit, and the HTTP request is done in HTTP/1.1 so we are at the liberty of
                              ;; choosing whether to go with Content-Length or Transfer-Encoding: chunked .
                              ;;
                              ;; What we do is that if the request body content is provided to us through the request-body parameter
                              ;; (here wrapped to request-body-u8v), then we go with Content-Length , and if it's provided through
                              ;; the request-body-feed parameter, we go with Transfer-Encoding: chunked .
                              (request-body-u8v
                               (values `((content-length . ,(number->string (u8vector-length request-body-u8v)))
                                         . ,headers)
                                       request-body-u8v
                                       #f ; request-body-feed/first-result
                                       #f)) ; send-body-in-chunked-encoding?
                              (else
                               (values `((transfer-encoding . "chunked")
                                         . ,headers)
                                       #f ; = request-body-u8v
                                       request-body-feed/first-result
                                       #t))))  ; = send-body-in-chunked-encoding?
                           (headers ; headers including any update with inclusion of content-length or transfer-encoding tag.
                            request-body-u8v
                            request-body-feed/first-result
                            send-body-in-chunked-encoding?) ; boolean, #f = send verbatim
                           ;; Do finishing round of processing of the headers.
                           (let* ((headers
                                   ;; Add the headers listed here, if they were not already present in headers.
                                   (headers-join headers
                                                 `(
                                                   ;; HTTP/1.1 standard, ordinarily included by HTTP library so that's what we do.
                                                   (host . ,(let* ((host (uri-host uri))
                                                                   (scheme (uri-scheme uri))
                                                                   (port (uri-port uri)))
                                                              ;; port = #f means use default port, which we communicate
                                                              ;; into the Header tag by only providing the hostname.
                                                              (if (and port
                                                                       (or (and (equal? scheme "http" ) (not (eq? 80  port)))
                                                                           (and (equal? scheme "https") (not (eq? 443 port)))))
                                                                  (string-append host ":" (number->string port))
                                                                  host)))
                                                   ;; General stuff.
                                                   ;; For example Wikipedia blocks clients that don't provide an User-Agent header.
                                                   (user-agent . "curl/7.18.2 (i486-pc-linux-gnu) libcurl/7.18.2")
                                                   (accept-language . "en")
                                                   ;; (accept . "text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5")
                                                   ;; What charsets we accept responses in.
                                                   ;; Telling them UTF-8 primarily and ISO-8859-1 secondarily makes great sense as default choice,
                                                   ;; as Gambit natively supports a limited number of charset coders only and these ought to be said
                                                   ;; to be the most central ones.
                                                   (accept-charset . "utf-8;q=0.7,ISO-8859-1") ; "utf-8;q=0.7,ISO-8859-1;q=0.6,*;q=0.5")
                                                   ;; Currently no support for GZip, deflate, etc. - require uncompressed response.
                                                   (accept-encoding . "identity") ; Identity means send it uncompressed
                                                   ;; We do not implement any caching primitives so let's be clear to servers that we do not accept
                                                   ;; cache references as response. (Is this ever needed?)
                                                   ;; (cache-control . "max-age=0")
                                                   (pragma . "no-cache")
                                                   (cache-control . "no-cache"))))
                                  (req-s (append-strings
                                          `(" " ,(uri-path&query->string uri) " " ,(symbol->string protocol) ,crlf-s
                                            ;; HTTP request headers
                                            .,(let ((list-maker (make-list-maker)))
                                                (for-each
                                                 (lambda (header)
                                                   (let ((key (car header)) (value (cdr header)))
                                                     (list-maker (header-key->string key))
                                                     (list-maker ": ")
                                                     (list-maker value)
                                                     (list-maker crlf-s)))
                                                 headers)
                                                ;; After all header key-value rows have been sent, send an empty row
                                                ;; to declare that the headers have been sent.
                                                (list-maker crlf-s)
                                                (list-maker))))))
                             ;; Soo, getting here, we have done any mending of headers needed, and we're ready to send the
                             ;; request row and headers.
                             (dbg* "Sending request: " req-s)
                             ;; Send request row and headers.
                             (if (not (and (display req-s)
                                           (force-output)))
                                 ;; Send request row and headers failed, fail connection.
                                 (fail)
                                 ;; Send request row and headers succeeded.
                                 (begin
                                   (dbg* "Sending body.")
                                   ;; Send request body, if applicable.
                                   (stage! 'send-body)
                                   ;; Any body to send?
                                   (if (not (if (or request-body-u8v request-body-feed/first-result)
                                                ;; Yep, send.
                                                (if send-body-in-chunked-encoding?
                                                    ;; Send in chunked encoding.
                                                    (http-write-with-chunked-encoding io-primitives
                                                                                      request-body-feed/first-result ; = first-chunk
                                                                                      get-chunk)
                                                    ;; Send verbatim.
                                                    (and (write-subu8vector request-body-u8v 0 (u8vector-length request-body-u8v))
                                                         ;; (It's important we flush the output here, as that needs to reach the other end in order to
                                                         ;; send us a response in return, which is what we will want to get directly now.)
                                                         (force-output)))
                                                ;; (No body to send, so report success in sending)
                                                #t))
                                       ;; Send body was applicable and failed
                                       (fail)
                                       ;; Send body, if applicable, succeeded.
                                       (begin
                                         (dbg* "Receiving headers.")
                                         ;; Time to read the HTTP response.
                                         (stage! 'receive-headers)
                                         ;; Read the HTTP response status line
                                         (receive
                                          (let read-status-line-loop ()
                                            (let* ((status-line (read-line-until-crlf/skip-empty-line/-s io-primitives)))
                                              (dbg* "Read status-line: " status-line)
                                              (call-with-values
                                                  (lambda ()
                                                    (or (and status-line (parse-http-response-status-line status-line))
                                                        (values #f #f)))
                                                (lambda (http-version status-code)
                                                  (if (eq? status-code 100)
                                                      ;; HTTP status 100 is an interim status that we're supposed to skip over,
                                                      ;; so if we run into it, just continue with reading the status line anew.
                                                      ;; http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
                                                      ;; http://www.jmarshall.com/easy/http/#http1.1s5
                                                      (read-status-line-loop)
                                                      (values status-line http-version status-code))))))
                                          (status-line http-version status-code*)
                                          (set! status-code status-code*)
                                          (dbg* "Got status-line " status-line ", response-status-code " status-code)
                                          (if (not status-code) ; This includes (not status-line)
                                              (fail)
                                              (let* ((has-body?
                                                      ;; Determine whether to read a body at all:
                                                      ;;
                                                      ;; The general behavior is to read a response body.
                                                      (not (or
                                                            ;; However, if either of the following holds true, we don't:
                                                            ;; 204 No Content does not include a message-body
                                                            ;; (https://github.com/bagder/curl/blob/acafe9c160e6c299d48d72ab93fdb7abfcbf3ed2/lib/http.c row 3172
                                                            ;; of May 2013 reflects this behavior)
                                                            (eq? 204 status-code)
                                                            ;; 304 Not Modified does not include a message-body
                                                            ;; (https://github.com/bagder/curl/blob/acafe9c160e6c299d48d72ab93fdb7abfcbf3ed2/lib/http.c row 3179
                                                            ;; of May 2013 reflects this behavior)
                                                            (eq? 304 status-code)
                                                            ;; 1xx responses don't include a message-body
                                                            ;; (e.g. 101 Switching Protocols.)
                                                            (eq? 1 (fxquotient status-code 100)) ; = (floor (/ code 100)))
                                                            ;; Correct?
                                                            ;; ; If the request was "HEAD", then the response doesn't include a message-body.
                                                            ;; (equal? method "HEAD")
                                                            ))))
                                                ;; Read the HTTP response headers
                                                (set! response-headers (read-headers io-primitives))
                                                (dbg* "Received headers: " response-headers ", has-body? " has-body?)
                                                (if (not response-headers)
                                                    ;; Failed to read HTTP response headers (because reached EOF or invalid header line)
                                                    (fail)
                                                    ;; Succeeded with reading HTTP response headers.
                                                    (begin
                                                      ;; Read HTTP response body.
                                                      (stage! 'receive-body)
                                                      ;; Extract Content-Length number if any and check for Transfer-Encoding: chunked .
                                                      (let* ((response-content-length (let ((val (al-get response-headers "content-length" #f)))
                                                                                        (and val (string->number val))))
                                                             (response-body-sent-in-chunked-encoding?
                                                              (find (lambda (x)
                                                                      (and (equal? "transfer-encoding" (car x))
                                                                           (string-prefix? "chunked" (string-downcase (cdr x)))))
                                                                    response-headers))
                                                             ;; Determine if connection mode is close or keep-alive.
                                                             ;;
                                                             ;; Our logics are motivated as follows:
                                                             ;;
                                                             ;; 1) If Connection: keep-alive is set, then treat connection as keep-alive.
                                                             ;;    This is only relevant for HTTP 1.0 connections; for HTTP 1.1 this is the default behavior.
                                                             ;;    This is per RFC 2068, section 19.7.1.
                                                             ;;
                                                             ;; 2) If Connection: close is set, then treat connection as close.
                                                             ;;    Per RFC 2616, section 8.1.2.1.
                                                             ;;
                                                             ;; 3) If Connection: is not set, then if HTTP 1.1 presume keep-alive and if 1.0 presume close.
                                                             ;;
                                                             ;; 4) If neither chunked encoding nor content-length are set, then presume close.
                                                             ;;    This is per RFC 2616 section 4.4 point 5.
                                                             ;;
                                                             ;; This is the same behavior as libCurl has per
                                                             ;; https://github.com/bagder/curl/blob/acafe9c160e6c299d48d72ab93fdb7abfcbf3ed2/lib/http.c
                                                             ;; (of May 2013): 1) at row 3279, 2) at row 3290, 3) at row 3148 and 4) at row 2881.
                                                             (connection-close? ; #t = close, #f = keepalive.
                                                              (or (and (not response-body-sent-in-chunked-encoding?)
                                                                       (not response-content-length))
                                                                  (let ((connection-header (al-get response-headers "connection" #f)))
                                                                    (if connection-header
                                                                        (equal? connection-header "close")
                                                                        (if (eq? http-version 'HTTP/1.1)
                                                                            #f ; keep-alive is default for HTTP 1.1
                                                                            #t)))))  ; close is default for HTTP 1.0
                                                             (read-subu8vector (io-primitives-read-subu8vector io-primitives))
                                                             ;; bytes-left = integer = Read this many bytes. Failure to read this many bytes is considered read failure.
                                                             ;;              #f = Read until EOF. EOF is considered read completion.
                                                             ;;
                                                             ;; buf-u8v-len must be >0.
                                                             ;;
                                                             ;; => boolean, #f = read failure.
                                                             (read-data-block-and-pass-to-user
                                                              (lambda (bytes-left buf-u8v buf-u8v-len)
                                                                (define read-til-eof? (not bytes-left))
                                                                (dbg* "Reading " (if read-til-eof?
                                                                                     "til EOF."
                                                                                     (string-append (number->string bytes-left) " bytes.")))
                                                                (let loop ((bytes-left bytes-left))
                                                                  (let* ((bytes-to-read (if read-til-eof?
                                                                                            buf-u8v-len
                                                                                            (min bytes-left buf-u8v-len))))
                                                                    (if (zero? bytes-to-read)
                                                                        #t
                                                                        (let ((read-bytes (read-subu8vector
                                                                                           buf-u8v 0 bytes-to-read
                                                                                           (if read-til-eof?
                                                                                               1
                                                                                               ;; Note that we say to read-subu8vector here that we need |bytes-to-read|
                                                                                               ;; bytes, so it will not return with less unless in case of failure.
                                                                                               bytes-to-read))))
                                                                          ;; TODO When Gambit ports get the |io-error| param clarify this one
                                                                          (if (memq read-bytes '(#f #!eof 0))
                                                                              (if read-til-eof?
                                                                                  ;; Reached EOF:
                                                                                  ;;
                                                                                  ;; Success.
                                                                                  #t
                                                                                  ;; Failed to read the specified number of bytes:
                                                                                  ;;
                                                                                  ;; Failure
                                                                                  #f)
                                                                              ;; If read-til-eof? #f: Successfully read data block, as in, bytes-to-read bytes.
                                                                              ;;
                                                                              (begin
                                                                                (dbg* "Read " read-bytes " bytes.")
                                                                                ;; If record-response is set and not full yet, pass on data to it.
                                                                                (if (and record-response (< recorded-response-bytes record-response))
                                                                                    (let* ((open-for-more-bytes (- record-response recorded-response-bytes))
                                                                                           (write-bytes (min open-for-more-bytes read-bytes)))
                                                                                      (define-macro (+! value to-add)
                                                                                        `(let ((n (+ ,value ,to-add)))
                                                                                           (set! ,value n)
                                                                                           n))
                                                                                      (if (not recorded-response) (set! recorded-response (open-output-u8vector)))
                                                                                      (gambit-write-subu8vector buf-u8v 0 write-bytes recorded-response)
                                                                                      (+! recorded-response-bytes write-bytes)))
                                                                                ;; Pass on the data block to the user, if the user wants it.
                                                                                (if on-data
                                                                                    (let ((last-call-within-block? (eq? bytes-to-read bytes-left))
                                                                                          (u8v (if copying?
                                                                                                   (subu8vector buf-u8v 0 bytes-to-read) ; Copy
                                                                                                   buf-u8v))) ; Pass buf-u8v
                                                                                      ;; (Ignore return value from on-data)
                                                                                      (on-data u8v bytes-to-read last-call-within-block?)))
                                                                                ;; Really should be fit within a (and bytes-left .. )?
                                                                                (loop (and bytes-left (- bytes-left read-bytes)))))))))))
                                                             (read-body-successfully?
                                                              (cond
                                                               ;; If the response body is sent in chunked encoding, read accordingly.
                                                               (response-body-sent-in-chunked-encoding?
                                                                (let* ((buf-u8v-len (* 10 1024))
                                                                       (buf-u8v (make-u8vector buf-u8v-len))
                                                                       (read-line ((io-primitives-read-line-until-crlf io-primitives) #f))) ; return-partial-string? = #f
                                                                  (let loop ()
                                                                    (let* ((len-str (read-line))
                                                                           (len (and (not (memq len-str '(#f #!eof))) ; TODO When Gambit ports get |io-error| slot and we use it, update this.
                                                                                     (chunked-coding-read-hex len-str))))
                                                                      (cond
                                                                       ;; Chunk header line was either not read because of EOF, or had broken contents. Fail.
                                                                       ((not len)
                                                                        #f)
                                                                       ;; We got a chunk with data in it. Read it and recur.
                                                                       ((not (zero? len))
                                                                        (begin
                                                                          (if (not (and (read-data-block-and-pass-to-user len buf-u8v buf-u8v-len)
                                                                                        ;; As to read the \r\n that's the end of each chunk:
                                                                                        ;; TODO When Gambit ports get |io-error| slot and we use it, update this.
                                                                                        (equal? (read-line) "")))
                                                                              ;; Failure to read, fail.
                                                                              #f
                                                                              (loop))))
                                                                       ;; Got a zero-size chunk. This is how the last chunk is signaled.
                                                                       (else
                                                                        ;; We're reading the last empty line that ends the chunk, right?     XXX Doublecheck, TODO
                                                                        (equal? (read-line) "")))))))
                                                               ;; If the response body is fixed size, read accordingly.
                                                               (response-content-length
                                                                (let* ((buf-u8v-len (* 10 1024))
                                                                       (buf-u8v (make-u8vector buf-u8v-len)))
                                                                  (read-data-block-and-pass-to-user response-content-length buf-u8v buf-u8v-len)))
                                                               ;; If it's a non-keepalive connection, then read until the connection closes.
                                                               (connection-close?
                                                                (let* ((buf-u8v-len (* 10 1024))
                                                                       (buf-u8v (make-u8vector buf-u8v-len)))
                                                                  (read-data-block-and-pass-to-user #f ; = Read til EOF.
                                                                                                    buf-u8v buf-u8v-len)))
                                                               ;; If neither chunked encoding nor Content-Length nor Connection: Close were set, it means
                                                               ;; the response has no body. So, report success for the body reading.
                                                               (else
                                                                #t))))
                                                        (if (not read-body-successfully?)
                                                            ;; Read response body failed.
                                                            (fail)
                                                            ;; Read response body succeeded.
                                                            (begin
                                                              (dbg* "HTTP request performed!")
                                                              ;; The performance of this HTTP request is done!
                                                              ;; Report success.
                                                              (result&stage! #t 'done)
                                                              (let* ((->boolean (lambda (x) (if x #t #f)))
                                                                     ;; (connection:close? (->boolean (member '("connection" . "close") response-headers)))
                                                                     (connection:keep-alive? (->boolean (member '("connection" . "keep-alive") response-headers))))
                                                                (void))))))))))))))))))))
                 ((result)
                  (lambda ()
                    '..))
                 ((stage)
                  (lambda ()
                    '..))
                 ((reached-uri)
                  (lambda ()
                    '..))
                 ((status-code)
                  (lambda ()
                    status-code))
                 ;; ((status-text)
                 ;;  (lambda ()
                 ;;    ..))
                 ((headers)
                  (lambda ()
                    response-headers))
                 ((header)
                  get-header)
                 ((response)
                  response)
                 ((abort!)
                  (lambda ()
                    '..))
                 ((wait-for-result)
                  (lambda ()
                    '..))
                 ((wait-for-stage)
                  (lambda* ((for-stage 'done))
                    '..))
                 ((all-response)
                  (lambda* ((response-as-text? 'auto))
                    (list uri ; reached-uri - http-connect makes no forwarding
                          result stage
                          status-code
                          response-headers get-header
                          (response response-as-text?))))))))
        ((close)
         (lambda ()
           (close-port)))))))

(define* (simple-raw-http-request uri
                                  (request-body: #f)
                                  (headers: '())
                                  (nonexistant-scheme->result: #f)
                                  (port-unknown->result: #f)
                                  (response-as-text?: 'auto))
  (let* ((method (if request-body "POST" "GET"))
         (uri (if (uri? uri) uri (string->uri uri)))
         (scheme (uri-scheme uri))
         (make-io-primitives (http-scheme-resolve scheme)))
    (if (not make-io-primitives)
        (or nonexistant-scheme->result
            (error "URI scheme is unknown" scheme uri))
        (let* ((port (or (uri-port uri) (let ((scheme (uri-scheme uri)))
                                          (cond
                                           ((equal? scheme "http" ) 80 )
                                           ((equal? scheme "https") 443)
                                           (else #f))))))
          (if (not port)
              (or port-unknown->result
                  (error "port not provided and could not be autodetected for scheme" scheme uri))
              (let* ((conn (http-connect (uri-host uri) port make-io-primitives: make-io-primitives))
                     (req ((conn 'request) uri method: method request-body: request-body headers: headers)))
                ((req 'perform))
                (let ((resp ((req 'all-response) response-as-text?)))
                  ((conn 'close))
                  resp)))))))

(define (simple-raw-http-request-resp . a)
  (with-http-client-all-response
   (apply simple-raw-http-request a)
   response))

(define (http-client-response-if-successful r)
  (with-http-client-all-response
   r
   (and (eq? 200 status-code) response)))


(println "(spheres/net sack-client) -- This library is incomplete!")
