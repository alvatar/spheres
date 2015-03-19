;; Sack HTTP server middleware
;;
;; Copyright (C) 2008-2009 Per Eckerdal, 2010-2013 Mikael More, 2005-2007 Marc Feeley.
;; All Rights Reserved.
;; MIT license.
;;
;; Fundementally Sack delivers core HTTP server functionality only, without also providing any
;; abstractions that the HTTP protocol does not imply. Thus for example, Sack does not provide
;; functionality to read static files from a harddrive as to deliver in a path on the HTTP server.
;;
;; Sack is inspired by Ruby's Rack, http://rack.github.io/ as introduced on
;; http://chneukirchen.org/blog/archive/2007/02/introducing-rack.html , which in turn was inspired
;; by PEP333: Python Web Server Gateway Interface, http://www.python.org/dev/peps/pep-0333/ .
;;
;; ## General description of use
;; The basic use goes like this:
;;
;;  * A Sack server is started using |sack-start!|. It takes the Sack app thunk as first argument,
;;    which is the procedure that Sack invokes for each HTTP request received. Remaining arguments
;;    are configuration options for what TCP port to use etc.
;;
;;  * The Sack app thunk takes one argument, being |env|. |env| is a procedure that contains the
;;    Sack app's "environment", as in, information about URI, headers, HTTP request body contents
;;    etc.
;;
;;    For the default |env| provided by Sack, see |make-environment|.
;;
;;  * Sack provides extensibility through easy implementation of "adapters", that form a layer
;;    between Sack and the Sack app, and that overlaps the |env| procedure as to provide additional
;;    functionality.
;;
;;    An example of such an extension is cookies support, provided by the cookies module.
;;
;;  * The Sack app procedure should return a structure (values http-code headers data-thunk) .
;;
;;    http-code  = HTTP response statatus code as an integer e.g. 200
;;    headers    = HTTP response headers as an a-list e.g.
;;                 '(("Content-Type" . "text/html; charset=utf-8"))
;;    data-thunk = HTTP response body data provider thunk. Procedure taking no arguments and that
;;                 returns either an u8vector containing the next chunk of body data to be sent to
;;                 the client, or #f meaning that all data to be output has been provided and the
;;                 provision of HTTP response thus is done now.
;;
;;  * On return of the sack-app, Sack sends to the client the HTTP response status and headers, and
;;    then invokes data-thunk all until it returns #f.
;;
;; ## Simple use example
;; Opens a HTTP server on port 8000, available to connect to from any peer. For each HTTP request,
;; responds with HTTP status 200 (= HTTP OK), no headers, and with the text "Hello world!\n\nURI: "
;; followed by a copy of the particular URI this visit is for, with domain name, path and query i.e.
;; HTTP GET arguments.
;;
;; (sack-start! (lambda (env)
;;                (define (r) (with-output-to-u8vector '() (lambda ()
;;                                                           (print "Hello world!\n"
;;                                                                  "\n"
;;                                                                  "URI: " (env 'sack:uri)))))
;;                (values 200 '() (let ((f #t)) (lambda () (and f (begin (set! f #f) (r)))))))
;;              server-address: "*" port-number: 8000)
;;
;; ## Error handling
;; Sack needs to have error handling that is automatic and transparent on the one hand, and that is
;; configurable and hookable on the other hand, as to satisfy any general development and error
;; reporting use case.
;;
;; # Connection error handling
;; Connections that shut down before having received HTTP request line and HTTP headers, are
;; disconnected transparently.
;;
;; Disconnection while reading the HTTP request body (through the 'sack:body environment parameter)
;; or while sending the HTTP response body (through
;;
;; # sack-app exception handling
;; By default on sack-app exception, Sack catches the exception and current continuation and prints
;; them out fully.
;;
;; While this works well as a default behavior, in a particular use of Sack, the user may want:
;;
;;  * Exceptions not to be caught.
;;    Also it would be imaginable that the user would want only the exception to be caught.
;;    And,
;;
;;  * Printouts not to be made, or made but of the exception only and not of the continuation.
;;
;; Adjusting these is done through the |on-exception-catch| and |print-caught-exception?| key
;; arguments, see their definition site below for the possible settings.
;;
;; Last, we also want an on exception callback for exception monitoring and reporting purposes.
;;
;; For its definition, see its definition site below.
;;
;; ## Note on keepalives and TCP protocol logics
;; Note that the keep-alive logics are written in such a way that the client web browser is pushed
;; to make some of the close:s. This is good as close:s are medium-expensive - at the level of the
;; TCP stack, for each TCP connection, the party that initiates the close needs to keep a structure
;; must in RAM for ~2mins and during this time not more than ~32000 connections including those
;; closed ones can exist between the two IP:s.
;;
;; ## Primitives for server overload protection
;; As fundamentally Sack's action when in multithreaded mode is to launch a thread with the sack-app
;; for each received connection, there is a possibility that the server becomes overloaded in terms
;; of RAM, CPU or other use, and could go out of order.
;;
;; There are two points in Sack that are regarded by overloading:
;;
;;  a) The acceptance of new TCP connections
;;     (Regulates the number of TCP connections being processed)
;;
;;     An incoming TCP connection starts taking any resources at all in the Gambit process, at first
;;     when it has been (read) from the TCP server port.
;;
;;     For this reason, together with that as long as we haven't (read) a connection the peer will
;;     get a proper failure value in their TCP stack, we implement the overload protection as a
;;     limiting mechanism on the (read):ing of new connections.
;;
;;  b) The keeping of idling keepalive connections
;;     (Regulates the number of TCP connections being processed)
;;
;;     As we relate within these primitives to the number of TCP connections open as the basic
;;     metric for basing overload protection mechanisms on, then in a situation of overload, based
;;     on the max allowed number of TCP connections having been reached, no more TCP connections
;;     will be allowed.
;;
;;     Apart from that each TCP connection generally brings with it sack-app execution, it also
;;     importantly occupies an OS socket and possibly a SSL channel, which are both expensive
;;     resources.
;;
;;     If the max TCP connection limit is reached, it's remedial to close TCP connections that very
;;     probably are idling forever anyhow.
;;
;; These two points provide of course a limitation in that the overload mechanism this way not gives
;; any consideration to the amount of connections that are waiting to get accepted, whatever many or
;; few it may be. This though, is not an issue, as the goal is to deliver as much as we can but
;; safely, and how much we can deliver is a function of server-internal factors only and is not
;; affected by the amount of waiting connections.
;;
;; A third point for the acceptance of HTTP requests within a TCP connection to regulate the load
;; put on internal processing and sack-app execution was considered, though as the overhead of HTTP
;; request reading before sack-app launch is small and because it wouldn't make sense to put such a
;; point before any SSL channel negotiation has been done as the goal right after accepting a new
;; TCP connection is to get it ready to receive HTTP requests on and both TCP handshake and SSL
;; negotiations are quick processes, it was chosen that such a point should not be included.
;;
;; Please note that overload protection regarding the execution of the sack-app itself is not in the
;; scope of these overload protection primitives; that is to be implemented at the sack-app side,
;; for instance as an overload wrapper;
;;
;; These overload protection primitives are for providing overload protection logics that cannot be
;; implemented at the level of sack-app, solely.
;;
;; Primitives for regulating these three points are provided by the following optional arguments:
;;
;; accept-block  = A procedure that takes no argument and returns nothing, that is invoked
;;                 before Sack will start (read):ing a new connection. This way the Sack user can
;;                 choose the timing of and rate for acceptance of new TCP connections.
;;
;;                 Do not let the accept-block procedure block forever, as closure of the Sack
;;                 server is done synchronously with accept-block only.
;; on-connection = A procedure that takes the arguments (connection is-connect?) and returns
;;                 nothing.
;;
;;                 connection  = A closure unique to the connection opened. Takes the arguments (operation) and
;;                               returns the corresponding value or a closure that performs the respective operation:
;;
;;                                    ('on-connection-idle-set!) => procedure (on-connection-idle) => #!void
;;                                    = Allows the Sack user to set a |on-connection-idle| closure for the TCP
;;                                    connection. This can be done *only during* the |on-connection| callback.
;;
;;                                    on-connect-idle is a closure that takes the arguments (into-idle?) and returns
;;                                    nothing. Sack will invoke it when the TCP connection will invoke it when it goes
;;                                    into and out of reading the request line for a new HTTP request on a TCP connection,
;;                                    which coincides with HTTP's idling connection state, that lasts for up to the duration
;;                                    of the keepalive timeout.
;;
;;                                    Thanks to this closure, the overload protection can keep track of how long respective
;;                                    TCP connections have idled, and if needing to zero keepalives due to proximity to or
;;                                    actual overload of number of concurrent TCP connections.
;;
;;                                    ('keepalive-zero!) => procedure (only-if-idle-now?) => boolean
;;                                    = Set the keepalive timeout for this connection to zero. This will make the
;;                                    TCP connection close right away when it's not in use, which is of use for
;;                                    an overload mechanism for decreasing the number of TCP connections quickly,
;;                                    as to minimize the time close to or in a situation of TCP connection overload. I.e.,
;;                                    close to or when a Sack user's TCP connections roof is reached, it can
;;                                    'keepalive-zero! present connections as to free up connection slots faster.
;;
;;                                    only-if-idle-now? = boolean, if #t the zeroing of the keepalive will be done
;;                                                        only if Sack is currently idling.
;;
;;                                    Returns boolean for if the keepalive was actually zeroed, #t unless if
;;                                    only-if-idle-now? was set and the connection was not idling now.
;;
;;                 is-connect? = A boolean that is #t if the call regards the opening of a TCP connection,
;;                               and #f if the call regards the closing of a TCP connection.
;;
;;                 This allows for the Sack user to track the number of active connections, and to manage the number of idling
;;                 keepalive connections.
;;
;; ## History
;; 2011-02-01: Added level to selected force-output calls to ensure that data is actually
;;             transmitted to the client immediately on chunk write.
;; 2012-02-18: Updated the calling convention of sack:body to a more effective and functional one.
;;             See the comments inside handle-sack-response for more info.
;; 2013-03-06: Structured and commented code. Implemented the 'sack:client-ip/u8v and
;;             'sack:client-ip environment methods.
;; 2013-05-10: Added primitives for overload handling.
;;
;; ## TODO
;;  * Replace current body reading code with one that's implemented in a straight way.
;;  * To specify to the server that the response is already chunk-encoded, you have to specify a
;;    header like ("Transfer-Encoding" . "chunked"), that is, the value must not contain any
;;    whitespace, commas or any other values (for instance "chunked" or "identity, chunked").
;;    Otherwise it will not understand it. To specify more than one encoding, use several header
;;    field definitions. (?)
;;  * Implement maximal chunk size on chunked encoding. [how to do that?]
;;  * Implement conditional gets (?)
;;  * Implement partial gets (?)
;;  * Implement IP serialization for IPv6 IP:s
;;  * Make the sack quit closure set the timeout on the server port to -inf as to
;;    return the (read) for a new connection ASAP.
;;  * Make the keepalive timeout applied *only* to the reading of the request line and request
;;    headers, and not as currently also apply to the course of the request output IO work,
;;    which should not be subject to any timeout, or, at least subject to another timeout value,
;;    separate from the timeout.
;;

(declare
 (standard-bindings)
 (extended-bindings)
 (block))


(define filter
  (lambda (p s)
    (cond
     ((null? s) '())
     ((p (car s)) (cons (car s) (filter p (cdr s))))
     (else (filter p (cdr s))))))






;; ## Configuration settings
;; The mutations below are to work around the (declare (block)) for the respective values.
                                        ;
;; How many HTTP requests it's the most effective thing for a web browser to pipeline in a single
;; TCP connection, we let up to the web browser to decide. Due to the quite significant time it can
;; take to establish a TCP connection (including a SSL handshake, for example), it's not our
;; job to impose anything that could be related to as limits in this regard.
(define default-http-connection-requests-roof #f)
(set! default-http-connection-requests-roof 50)

;; # Debug output settings
;; The code contains outputs for debug logging. Since it's extremely rare that Sack would need to be debugged,
;; we provide the toggle for it by the user commenting our or not commenting out the actual code for it in here.
(define sack-verbose #f)
(define sack-verbose-about-gc-time-finalization? #f) ; #f = No debug output on GC-time sack-app thread finalization
                                        ; #t = Print output on GC-time sack-app thread finalization
                                        ; 'all = Print output on any sack-app thread finalization
(set! sack-verbose                             #f)
(set! sack-verbose-about-gc-time-finalization? #f)

;; # Default exception reporting settings
(define sack-exception-on-exception-catch      #f)
(define sack-exception-print-caught-exception? #f)
(define sack-exception-print-max-head          #f)
(define sack-exception-print-max-tail          #f)
(define sack-exception-on-exception-hook       #f)
(set! sack-exception-on-exception-catch      'exception&continuation)
(set! sack-exception-print-caught-exception? #t                     )
(set! sack-exception-print-max-head          100                    )
(set! sack-exception-print-max-tail          100                    )
(set! sack-exception-on-exception-hook       #f                     )

(define sack-version '(0 2))

(define-macro (sack-dbg . a)
  `(begin
     (if sack-verbose
         (begin
           (print port: console-output-port "sack: " (apply dumps (list ,@a)) "\n")
           (force-output console-output-port 1)))))

(define-macro (sack-dbg-warn . a)
  `(begin
     (print port: console-output-port " XX sack warning: " (apply dumps (list ,@a)) "\n")
     (force-output console-output-port 1)))


;; Utilities.

;; For making debug output
(define console-output-port (current-output-port))

;; The parameter name is assumed to be lowercase.
(define (has-header? headers name)
  (let loop ((lst headers))
    (cond
     ((null? lst) #f)
     ((equal? name
              (string-downcase (caar lst))) #t)
     (else (loop (cdr lst))))))

;; The parameter name is assumed to be lowercase. Note that this
;; function does not understand comma-separated lists of in header
;; values.
(define (has-header-with-value? headers name value)
  (let loop ((lst headers))
    (cond
     ((null? lst)
      #f)

     ((and (equal? name
                   (string-downcase (caar lst)))
           (equal? value
                   (cdar lst)))
      #t)

     (else
      (loop (cdr lst))))))


;;==============================================================================
;; HTTP server.

(define* (sack-start! sack-application ;; The Sack application thunk to be invoked for every received HTTP request.
                      ;; #f  = Accept connections from localhost only (default set to this, for security).
                      ;; "*" = Accept connections from anywhere
                      ;; A local IP address can be specified e.g. "1.2.3.4" = Listen to connections made to that IP only.
                      ;;                                                      This is of value on servers that have several IP:s
                      ;;                                                      and you want to provide different servers on different IP:s.
                      (server-address: #f)
                      ;; = TCP port to listen on, integer. Note that the ports < 1024 generally require
                      ;;   administrator privileges to listen on.
                      (port-number: 80)
                      ;; #f = Use Gambit's default value (which is 128).
                      (max-waiting-connections: #f)
                      ;; #t = Create a Gambit thread for each HTTP connection, so that several HTTP requests can
                      ;;      be handled concurrently.
                      ;; #f = Process only one HTTP connection at a time, in the HTTP server's thread.
                      ;;      Note that you need to disable keepalives completely for this to deliver.
                      (threaded?: #t)
                      ;; Integer, seconds that Sack waits for another HTTP request on a HTTP connection before considering
                      ;; the HTTP connection to be inactive and closing it.
                      (keep-alive-timeout: 15)
                      ;; Upper limit of number of HTTP requests to process on one
                      (keep-alive: default-http-connection-requests-roof)
                      ;; HTTP connection.
                      ;; IO timeout - if an IO operation takes longer than this, close HTTP connection.
                      (timeout: 300)
                      ;; Procedure invoked on IO error.
                      (io-error-callback: (lambda (e error-specific-to-a-connection?) #f))
                      ;; Name passed to make-thread for HTTP server thread.
                      ;; 'auto = Autogenerate a suitable name.
                      (thread-name: 'auto)
                      ;; "http" or "https". Use this scheme value in URI objects created.
                      (uri-scheme: "http")
                      ;; We accept #f as meaning |with-standard-primitives|, just not
                      ;; to require any user of this lib that might want to pass this
                      ;; value, to import io-primitives too.
                      (use-io-primitives: #f)
                      ;; Please see the "Primitives for server overload protection"
                      (accept-block: #f)
                      ;; header comments section for more info on these two.
                      (on-connection: #f)
                      ;; The time the server thread should sleep in case of an ECONNABORTED error etc.
                      ;; The purpose with the sleep is to not cause an infinite loop in the exotic
                      ;; case that we would get unending errors from the OS. We never saw this happen
                      ;; in practice though.
                      ;;
                      ;; # sack-app exception handling related options
                      ;; For these three variables, if set to 'default, the respective of the global variables
                      ;; |sack-exception-on-exception-catch|
                      ;; |sack-exception-print-caught-exception?| |sack-exception-print-max-head| |sack-exception-print-max-tail|
                      ;; |sack-exception-on-exception-hook|
                      ;; should be used instead, on the handling of the respective HTTP request.
                      (tcp-server-read-error-sleep: 0.025)
                      ;; = Either of:
                      ;;        #f                      = Don't catch exceptions, but let them
                      ;;                                  go to the thread system/REPL
                      ;;        'exception              = Catch exceptions
                      ;;        'exception&continuation = Catch exceptions and continuations
                      (on-exception-catch: 'default)
                      ;; = Either of:
                      ;;        #t = Print out the full exception/&continuation/ caught.
                      ;;        'only-exception = Print out the exception caught.
                      ;;        #f = Don't do a printout on exception
                      (print-caught-exception?: 'default)
                      (print-max-head: 'default)
                      (print-max-tail: 'default)
                      ;; = #f = no on exception hook, or
                      ;;   thunk taking the arguments (exception/continuation exception continuation).
                      ;;   thunk is only invoked if on-exception-catch above is set.
                      ;;   |exception/continuation| |continuation| are only set if |on-exception-catch|
                      ;;   is set to 'exception&continuation , otherwise they are #f.
                      (on-exception-hook: 'default)
                      ;; # Debugging related options
                      ;; Create a will object to finalize connections, as a means to guarantee that |on-connection|
                      ;; is invoked properly even if the sack-app aborts due to exception?
                      ;;
                      ;; That this function is disabable through this option gives the user the opportunity to
                      ;; evaluate whether the will object in any way would happen to contribute to a bug such as
                      ;; a memory leak or alike.
                      ;;
                      ;; This option is given only because use of |make-will| is so rare - in practice its use
                      ;; has been working perfectly all the time and this option has not been needed at any point.
                      (create-finalizer-will?: #t))
  (let* ((server-port
          (open-tcp-server
           `(server-address: ,(or server-address "")
                             port-number: ,port-number
                             ,@(if max-waiting-connections `(backlog: ,max-waiting-connections) '())
                             ;; reuse-address: #t - this is the default setting, no need mention.
                             ;; server-address: '#u8(127 0 0 1) ; on localhost interface only
                             ;; The reason we use this encoding is because all header data is communicated
                             ;; in this encoding anyhow. As for HTTP body content of the request or response,
                             ;; those are passed in u8vector format anyhow.
                             ;;
                             ;; We use ISO-8859-1 to guarantee symmetry of |with-standard-primitives| of io-primitives
                             ;; with any other io-primitives implementation such as that for HTTPS, which is to follow
                             ;; io-primitives' convention of usign ISO-8859-1 also.
                             char-encoding: ISO-8859-1)))
         (serve/perform
          (lambda (gambit-port)
            (sack-dbg "Got a connection, creating IO primitives instance.")
            (if use-io-primitives
                (io-primitives#with-io-primitives
                 gambit-port keep-alive-timeout
                 (lambda (io-primitives)
                   ;; (monitor-for-memory-leakage io-primitives)
                   ;; (set-timeout! connection keep-alive-timeout io-primitives)
                   (sack-dbg "IO primitives created, going into serve-connection .")
                   (serve-connection sack-application
                                     timeout
                                     io-primitives
                                     threaded?
                                     port-number
                                     uri-scheme
                                     keep-alive
                                     gambit-port
                                     on-exception-catch
                                     print-caught-exception? print-max-head print-max-tail
                                     on-exception-hook)))
                (with-standard-primitives
                 gambit-port keep-alive-timeout
                 (lambda (io-primitives)
                   ;; (monitor-for-memory-leakage io-primitives)
                   ;; (set-timeout! connection keep-alive-timeout io-primitives)
                   (sack-dbg "IO primitives created, going into serve-connection .")
                   (serve-connection sack-application
                                     timeout
                                     io-primitives
                                     threaded?
                                     port-number
                                     uri-scheme
                                     keep-alive
                                     gambit-port
                                     on-exception-catch
                                     print-caught-exception? print-max-head print-max-tail
                                     on-exception-hook))))
            ;; = Thread result value
            'terminated-normally))
         (serve
          (lambda (gambit-port)
            ;; (with-exception/continuation-catcher
            ;;  (lambda (e)
            ;;    (io-error-callback e #t)
            ;;    (list 'terminated-by-exception e)) ; Leaves thread termination message
            ;;  (lambda () (serve/perform connection))
            (serve/perform gambit-port)))
         ;; We put the central logics for the main thread's handling of an individual incoming HTTP connection
         ;; in a separate procedure, as to minimize the risk surface for bugs such as memory leaks.
         (deliver
          (lambda (connection)
            (define finalized-at-first-at-gc? #t)
            (define (finalize/do)
              (if sack-verbose-about-gc-time-finalization?
                  (cond (finalized-at-first-at-gc?
                         (sack-dbg-warn "XXX Connection " connection " finalized at first at GC time. (Thread " (current-thread) ".) XXX"))
                        ((eq? sack-verbose-about-gc-time-finalization? 'all)
                         (sack-dbg-warn "Connection " connection " finalized per the ordinary route. (Thread " (current-thread) ")"))
                        ;; (else (sack-dbg-warn "debug: finalize/do invoked for connection " connection ". (Thread " (current-thread) ")")) ; Mere debug
                        ))
              ;; (Currently the presence of on-connection is a condition for the creation
              ;; of this will and therefore no checking for that it's set is needed.)
              (on-connection monitoring-closure #f)) ; is-connect? = #f
            ;; We now have a newly accepted TCP connection |connection| to process.
            ;;
            ;; As processing |connection| may be (and generally is) done in a multithreaded fashion,
            ;; and we immediately after starting the processing thread get into an |accept-block| run if
            ;; it's specified and after it to accepting a new connection, then in order for the overload
            ;; protection primitives to work with integrity, we need to do any |on-connect| call here as
            ;; for it to be synchronous with the following |accept-block| call, so that it will block
            ;; or not in proper consideration of this new connection.
            ;;
            ;; Therefore:
            (define monitoring-closure (and on-connection ; Create it only if monitoring is actually going on.
                                            (lambda (operation)
                                              (case operation
                                                ((timeout-zero!) (error "timeout-zero! not implemented"))
                                                ;; Apart from providing a tap for debugging purposes, this procedure
                                                ;; enforces uniqueness of the closure -
                                                ;; If the closure closes over no variables post the compiler's optimization,
                                                ;; it returns one and the same closure always.
                                                ;; This one should be fixed by using a Gambit-provided best practice such as a (declare).
                                                ((connection) connection)
                                                ;; This would just add to our memory leak drama, let's save ourselves of that.
                                                ;; ; Debug tap of the finalization will. Could be used to check the finalization and GC:ing
                                                ;; ; status of a connection.
                                                ;; ((finalize-will) finalize-will)
                                                (else (error "Unknown operation" operation))))))
            ;; Finalization work for the handling of this TCP connection must be done as for our app
            ;; not to get into an undefined state. For this purpose, we create a will that trigs on GC,
            ;; just in case this sack thread would fail to the REPL and be shut down in such a way that
            ;; it does not execute up to its ordinary termination point.
            ;;
            ;; In ordinary cases however, the will is finalized already below, by the |finalize| call -
            ;; this is done by direct finalization of this will.
            ;;
            ;; Create the will for finalizing the sack-app execution
            ;; only if the will have anything of relevance to do.
            (define finalize-will (and on-connection
                                       create-finalizer-will?
                                       ;; We set the TCP connection port (named gambit-port in other places) as
                                       ;; testator; its garbage collection is indeed a perfect indicator that
                                       ;; Sack's handling of the TCP connection it contains is indeed over.
                                       (make-will connection
                                                  ;; action:
                                                  (lambda (testator) ; = |connection|
                                                    (finalize/do)))))
            (define (finalize)
              ;; Ordinary code path for finalization of TCP connection handling.
              ;; If finalize-will was created, finalize it, i.e. invoke the on-connect overload protection primitive
              ;; to tell it that this connection is now closed.
              ;;
              ;; It won't happen ever but just for reference, subsequent |will-execute!| invocations are noop:s.
              (if on-connection
                  (begin
                    (set! finalized-at-first-at-gc? #f)
                    (if create-finalizer-will?
                        (begin
                          ;; (sack-dbg-warn "finalize: Doing (will-execute! finalize-will) to trig will finalization of finalize-will.")
                          (will-execute! finalize-will))
                        (begin
                          ;; (sack-dbg-warn "finalize: We have no will, so running finalize/do right away.")
                          (finalize/do))))
                  ;; (sack-dbg-warn "finalize: on-connection not set, nothing to do.")
                  ))
            ;; Remove when Gambit's make-will + will-execute! bug has been resolved.
            (if finalize-will (sack-dbg-warn "Will #" (object->serial-number finalize-will) " was created with " connection " as testator."))
            ;; (monitor-for-memory-leakage connection)
            (if (not (port? connection)) (error "Internal inconsistency - connection not a port!" connection))
            (if finalize-will (if (not (port? (will-testator finalize-will))) (error "Internal inconsistency - finalize-will's testator not a port!" finalize-will)))
            ;; (if finalize-will (monitor-for-memory-leakage finalize-will))
            ;; If the on-connect overload protection primitive has been provided by the
            ;; Sack user, invoke it.
            (if on-connection
                (begin
                  ;; (monitor-for-memory-leakage monitoring-closure)
                  (on-connection monitoring-closure #t))) ; is-connect? = #t
            (if threaded?
                ;; Multithreaded mode.
                (let ((thread (make-thread
                               (lambda ()
                                 (let ((dummy-port (open-dummy)))
                                   (parameterize
                                    ((current-input-port  dummy-port)
                                     (current-output-port dummy-port))
                                    (sack-dbg "Got connection.")
                                    (let ((r (serve connection)))
                                      (finalize)
                                      r))))
                               '(http-server connection-handler-thread))))
                  ;; (monitor-for-memory-leakage thread)
                  (thread-start! thread))
                ;; Single-threaded mode.
                (begin
                  (sack-dbg "Got connection.")
                  (serve connection)
                  (finalize)))))
         ;; A mutex that is unlocked when the server should quit.
         (quit-mutex (make-mutex)))
    (mutex-lock! quit-mutex)
    (thread-start!
     (let ((thunk (lambda ()
                    (let loop ()
                      ;; If the accept-block overload protection primitive has been provided by the
                      ;; Sack user, invoke it.
                      (if accept-block (accept-block))
                      ;; Read a new TCP connection from the server port.
                      (let* ((connection
                              ;; If it was not for the possibility of ECONNABORTED Software caused connection abort ,
                              ;; we could just read straight here, i.e. (read server-port) .
                              ;;
                              ;; Now that that may happen though, we need a failsafe method.
                              ;;
                              ;; As I got it, ECONNABORTED happens on certain systems such as OpenBSD because
                              (let loop ()
                                (with-exception-catcher
                                 (lambda (e)
                                   (sack-dbg-warn "Reading from the HTTP server port gave exception, ignoring and resuming: " e)
                                   (thread-sleep! tcp-server-read-error-sleep)
                                   (loop))
                                 (lambda ()
                                   (let ((r (read server-port)))
                                     ;; One reason we check the type here, is that the connection is used as testator in
                                     ;; wills later, and if that testator is #f then the will never executes and thus
                                     ;; we'd get a memory leak.
                                     (if (not (port? r)) (error "Didn't get a port" r))
                                     r))))))
                        ;; Do all the delivery work regarding this connection.
                        (deliver connection)
                        ;; Proceed with next connection, or terminate if server shutdown has been initiated.
                        ;;
                        ;; If the mutex is not locked, it means that we should quit.
                        (if (not (mutex-lock! quit-mutex 0))
                            (loop))))))
           (thread-name (if (eq? thread-name 'auto)
                            `(http-server connection-listener-thread port: ,port-number)
                            thread-name)))
       (if thread-name
           (make-thread thunk thread-name)
           (make-thread thunk))))
    (lambda ()
      (if (mutex-lock! quit-mutex 0)
          (error "Server has already quit (or is quitting)")
          (begin
            (mutex-unlock! quit-mutex)
            (close-port server-port))))))


;;==============================================================================
;;!! Error functions.

(define (show-error code str io-primitives)
  (display-crlf io-primitives "HTTP/1.1 " code " " (http-status-code code))
  (display-header io-primitives `("Content-Length" . ,(string-length str)))
  (display-header io-primitives `("Content-Type" . "text/html; char-encoding=UTF-8"))
  (display-crlf io-primitives)
  ((io-primitives-display io-primitives) str)
  ((io-primitives-close-port io-primitives)))

(define (method-not-implemented-error io-primitives)
  (show-error 501
              "<html><head><title>501 Method Not Implemented</title></head>\n<body><h1>Method Not Implemented</h1></body></html>"
              io-primitives))

(define (internal-server-error io-primitives)
  (show-error 500
              "<html><head><title>500 Internal Server Error</title></head>\n<body><h1>Internal Server Error</h1></body></html>"
              io-primitives))

(define (bad-request-error io-primitives)
  (show-error 400
              "<html><head><title>400 Bad Request</title></head>\n<body><h1>Bad Request</h1></body></html>"
              io-primitives))

;;------------------------------------------------------------------------------
;; Sack functions.

(define (make-environment threaded?
                          uri
                          request-method
                          ;; Attributes is an alist of lowercase
                          ;; header names and their values
                          attributes
                          gambit-port
                          takeover-connection-thunk-set!)
  (sack-dbg "(make-environment): Invoked. attributes " attributes)
  (let* ((peer-socket-info (let ((d #f))
                             (lambda ()
                               (or d
                                   (begin
                                     (set! d (tcp-client-peer-socket-info gambit-port))
                                     ;; If d is #f, it means that the HTTP TCP connection is closed.
                                     ;;
                                     ;; This is nothing unexpected and therefore we do not relate to it as an error state like
                                     ;; (if (not d) (error "tcp-client-peer-socket-info returned #f" gambit-port))
                                     d)))))
         (client-ip/u8v (let ((d-retrieved? #f) (d #f))
                          (lambda ()
                            (if d-retrieved?
                                d
                                (let ((peer-socket-info (peer-socket-info)))
                                  (set! d (and peer-socket-info (socket-info-address peer-socket-info)))
                                  (set! d-retrieved? #t)
                                  d)))))
         (client-ip     (let ((d-retrieved? #f) (d #f))
                          (lambda ()
                                        ; Used to be (tcp-client-port-ip-string gambit-port) of (std net/tcpip) ,
                                        ; Gambit provides this itself though.
                            (if d-retrieved?
                                d
                                (let ((u (client-ip/u8v)))
                                  (define (join between args)
                                    (cond ((null? args) '())
                                          ((null? (cdr args)) (list (car args)))
                                          (else `(,(car args) ,between ,@(join between (cdr args))))))
                                  (set! d (and u
                                               (case (u8vector-length u)
                                                 ((4) (append-strings (join "." (map number->string (u8vector->list u)))))
                                                 (else (error "IP string serialization not implemented for IP type" u)))))
                                  (set! d-retrieved? #t)
                                  d))
                            )))
         (headers
          (lambda (name #!optional only-first?)
            (if (procedure? name)
                (for-each (lambda (pair)
                            (name (car pair) (cdr pair)))
                          attributes)
                (let ((r (map cdr
                              (filter (lambda (pair)
                                        (equal? (car pair) name))
                                      attributes))))
                  (if only-first?
                      (and (not (null? r)) (car r))
                      r)))))
         (sack:body #f))
    (lambda (name)
      (case name
        ;; Sack settings
        ((sack:version) sack-version)
        ((sack:single-thread?) (not threaded?))
        ((sack:root) "")        ; ??? Remove?
        ;; HTTP request information:
        ;;  * HTTP connection peer information
        ((sack:peer-socket-info) (peer-socket-info))
        ((sack:client-ip/u8v) (client-ip/u8v))
        ((sack:client-ip) (client-ip))
        ;;
        ;;  * Request and header contents
        ((sack:request-method) request-method)
        ((sack:uri) uri)
        ((sack:headers) headers)
        ;;
        ;;  * Body contents
        ;; The first call to sack:body is made by the internal |handle-sack-response| procedure.
        ;; It sets the sack:body variable to the right handler procedure, which is subsequently
        ;; returned on each call to 'sack:body . I.e., this code here is only like a trampoline
        ;; for the provision of that procedure.
        ((sack:body)
         (or sack:body
             (lambda (sack:body-to-set)
               (set! sack:body sack:body-to-set))))
        ;; Other
        ((sack:takeover-connection-thunk-set!) takeover-connection-thunk-set!)
        (else #f)))))

(define (handle-sack-response-headers io-primitives
                                      version
                                      code
                                      headers
                                      chunked-encoding?
                                      close-connection?
                                      has-close-header?)
  ;; Display headers
  (display-crlf io-primitives version " " code " " (http-status-code code))
  (if (not (has-header? headers "date"))
      (display-header io-primitives `("Date" . ,(time->string (current-time)))))
  (if (and close-connection?
           (not has-close-header?))
      (display-header io-primitives `("Connection" . "close")))
  (display-headers io-primitives headers)
  ;; It's important that this header is sent after the other headers,
  ;; because they might contain other transfer encodings, and chunked
  ;; should always be the last one.
  (if chunked-encoding?
      (display-header io-primitives `("Transfer-Encoding" . "chunked")))
  (display-crlf io-primitives)
  ((io-primitives-force-output io-primitives)))

;; HTTP request body reader procedure.
;; Invoked by |handle-sack-response|.
;;
;; The reading work is done in units of the read buffer or the HTTP chunk size, whichever is
;; smallest. The mechanism is structured in such a way that it can be invoked over and over. Here's
;; the convention:
;;
;; (make-sack-request-body-handler environment connection)
;; => ret
;;
;; ret = #f = nothing was read and there's nothing more to read
;;       reader-thunk w args (data-thunk copying?), that when invoked, returns another ret.
;;       (box reader-thunk) = same as the previos option, except, the data-thunk returned #f.
;; Note that |reader-thunk|:s are single-use.
;;
;; (This procedure was initially written in a format as to accomodate providing the reader user
;; with the newly read data as the return value of a procedure, this is why this procedure is
;; written as it is.)
(define (make-sack-request-body-handler environment io-primitives)
  ;; Read incoming HTTP request body
  ;; (with-output-to-port console-output-port (lambda () (print "attributes=") (pp attributes)))
  (let* ( ;; (data-thunk ((environment 'sack:body)))
         (sack:headers (environment 'sack:headers))
         (content-length (let ((v (sack:headers "content-length")))
                           ;; (with-output-to-port console-output-port (lambda () (print "Sack: v=") (write v) (print "\n")))
                           (and (not (null? v)) (string->number (car v)))))
         (chunked-encoding (let ((v (sack:headers "transfer-encoding")))
                             ;; (with-output-to-port console-output-port (lambda () (print "Sack: v=") (write v) (print "\n")))
                             (and (not (null? v)) (string-prefix? "chunked"
                                                                  (string-downcase (car v))))))
         (buf-size 4096)
         (buf #f))
    (define (init-buf!)
      (set! buf (make-u8vector buf-size)))
    (define (make-read-bytes bytes-left*)
      (lambda (data-thunk copying?)
        (let loop ((bytes-left bytes-left*) (data-thunk data-thunk) (copying? copying?))
          (if (< 0 bytes-left)
              (let* ((bytes-to-read (min buf-size bytes-left))
                     (_tmp (sack-dbg "(make-sack-request-body-handler): Going into read " bytes-to-read " bytes."))
                     (bytes-read ((io-primitives-read-subu8vector io-primitives) buf 0 bytes-to-read bytes-to-read)))
                (sack-dbg "(make-sack-request-body-handler): Read " bytes-read " bytes.")
                (if (and bytes-read (< 0 bytes-read))
                    (begin
                      (sack-dbg "(make-sack-request-body-handler): Read " bytes-read " bytes.")
                      (let* ((false-response? (if data-thunk
                                                  (not (if copying?
                                                           (data-thunk (subu8vector buf 0 bytes-read))
                                                           (data-thunk buf bytes-read)))
                                                  #f))
                             (continue (lambda (data-thunk copying?)
                                         (loop (- bytes-left bytes-read) data-thunk copying?))))
                        (if false-response? (box continue) continue)))
                    #f))
              #f))))
    (sack-dbg "(make-sack-request-body-handler): Now into assigning handlers for reading request contents body."
              " chunked-encoding=" chunked-encoding ", content-length=" content-length)
    (cond
     ;; HTTP request has chunked encoding
     ;; (End of body is denoted by a zero-length chunk.)
     (chunked-encoding
      (sack-dbg "(make-sack-request-body-handler): Entered. Using chunked encoding so returning routine for reading that out.")
      (lambda (data-thunk copying?)
        (define read-line-until-crlf ((io-primitives-read-line-until-crlf io-primitives)
                                      #t)) ; return-partial-string? = #t
        ;; This makes it returns the string up to the point of EOF, if EOF is reached.
        (sack-dbg "(make-sack-request-body-handler): Decoding chunked encoding.")
        (init-buf!)
        (let loop ((data-thunk data-thunk)
                   (copying? copying?))
          (let* ((len-str (read-line-until-crlf))
                 (len (chunked-coding-read-hex len-str)))
            (if (and len ; If we got an invalid or empty len-str, treat this the same way as if the len value is zero,
                                        ; namely by going to the block below which returns #f.
                     (not (zero? len)))
                (let ((read-bytes (make-read-bytes len)))
                  (let read-bytes-loop ((data-thunk data-thunk)
                                        (copying? copying?)
                                        )
                    (sack-dbg "(make-sack-request-body-handler): Reading " len " bytes http chunk")
                    (let ((k (read-bytes data-thunk copying?)))
                      (if k
                          (let ((boxed? (box? k)))
                            (set! read-bytes (if boxed? (unbox k) k))
                            (if boxed? (box read-bytes-loop) read-bytes-loop)) ; (We return read-bytes-loop)
                          (begin
                            (read-line-until-crlf io-primitives) ; To read the CRLF that every chunk ends with.
                            (loop data-thunk copying?))))))
                (begin
                  (sack-dbg "(make-sack-request-body-handler): Read all chunks, ending.")
                  #f))))))
     ;; HTTP request has Content-Length set
     (content-length
      (sack-dbg "(make-sack-request-body-handler): Entered. We know the content length, so returning routine for reading that out.")
      (if (not (zero? content-length)) ; No need to allocate buffer and do read op if length is zero
          (lambda (data-thunk copying?)
            (init-buf!)
            (sack-dbg "(make-sack-request-body-handler): Content-Length set to " content-length ", processing.")
            ((make-read-bytes content-length) data-thunk copying?))
          #f))
     ;; HTTP request has no Content-Length, but has Connection: Close
     ;; HTTP request has neither Content-Length nor Connection: Close
     ;; In this case we presume that there is no request body. (Presumably this is in accordance with the HTTP RFC?)
     (else
      (sack-dbg "(make-sack-request-body-handler): Entered. There's neither Content-Length nor Connection: Close nor "
                "Transfer-Encoding: Chunked, so presuming there's no request body, returning no handler.")
      #f)
     ;; There could be some HTTP 1.0 or 0.9 client we'd be missing form data from, that just send it after the headers
     ;; and then close the connection, without specifying Connection: Close or Content-Length.
     )
    ;; Please note that the following code was written for this procedure when it had done all reading work already
    ;; up to here. Now it's rather event-based. So the following code could not be taken in use straight off now.
    ;; (let ((b1 (read-u8 connection)) (b2 (read-u8 connection)))
    ;;   (if (not (and (memq b1 '(#\return #!eof)) (memq b2 '(#\newline #!eof))))
    ;;       (begin
    ;;         (with-output-to-port console-output-port (lambda () (print "Sack: Invalid end of request, expected chars 0D 0A. ")
    ;;                                                    (write b1) (print " ") (write b2) (print "\n")))
    ;;         (error "Invalid end of " b1 b2))))
    ;; ^ I think this is commented out because the error never happened again. In case it would though
    ;;   and it's because of erratic behavior from the client,
                                        ;
    ;; (with-output-to-port console-output-port (lambda () (print "Sack: Finished any request contents processing for connection.\n")))
    ))

;; This procedure handles a HTTP request within a HTTP connection.
;; It is invoked by |handle-request| of |serve-connection|.
;;
;; => keep-connection-alive?
(define (handle-sack-response
         ;; The server's keep-alive setting, an integer saying how many more HTTP requests we allow to be
         ;; processed over this HTTP connection.
         keep-alive
         ;; Sack application thunk to invoke
         sack-application
         ;; Environment closure for HTTP request (generally used under the name |env| in Sack app thunks).
         environment
         ;; io-primitives for doing IO on the HTTP connection
         io-primitives
         ;; HTTP version symbol: 'HTTP/1.1 etc.
         version
         takeover-connection-thunk-get
         gambit-port)
  ;; We get here from handle-request of serve-connection . At this point, the HTTP request line and the request
  ;; headers have been read in full, and none of the body has been read. The process now is to invoke the Sack
  ;; application and get the response HTTP headers in full from it, and then output the headers and
  ;; the HTTP body based on the |response-thunk|'s return values, and by that the handling of this HTTP
  ;; request is done, with the exception for reading the HTTP request body.
  ;;      This is done by invocation to the 'sack:body environment parameter. The sack application may
  ;; do this as for it to get the HTTP request body's contents. After all execution of the sack app
  ;; for this request, we also run 'sack:body from here to ensure complete drainage from the HTTP
  ;; connection port of the body, so that the connection is reset correctly for handling of any
  ;; subsequent HTTP request in the same connection.
  ;;      At the end of the handling of this request i.e. at the bottom of this procedure, 'sack:body
  ;; for this request is blocked from any subsequent reading, so that just in case it'd be called
  ;; by the sack app after the processing of this request has finished (by another thread etc.) it
  ;; won't interfere with the handling of subsequent HTTP requests using the same connection.
  (define http-request-body-read (make-sack-request-body-handler environment io-primitives))
  ;; sack:body for the sack-application works as follows:
  ;; ((environment 'sack:body) data-thunk #!key (copying? #t)) => reached-eof-wo-data-thunk-cancelling-by-returning-false?
  ;; If copying? is #t: data-thunk is a procedure that takes the arguments (u8v), where u8v is an
  ;; u8vector containing the most recently read block/chunk of data. Sack makes no further use of u8v.
  ;; If copying? is #f: data-thunk is a procedure that takes the arguments (u8v len), where u8v
  ;; is an u8vector whose len first bytes contain the most recently read block/chunk of data. The
  ;; data is guaranteed to be there up to and only up to that data-thunk returns.
  ;;
  ;; The read operation with sack:body per above returns when data-thunk returned #f or when the
  ;; request body has been fully read.
  ;;
  ;; sack:body returns #f if data-thunk returned #f, otherwise #t. (Other than this, its return value
  ;; is not affected by whether any data was actually read.)
  ;;
  ;; sack:body may be re-run at any time. If its previous return was because of end of data,
  ;; the new run will just return #f . If the previous return was because data-thunk returned #f,
  ;; the new sack:body run will continue reading right after the point the previous reading ended.
  (define sack:body (lambda* (data-thunk (copying?: #t))
                        (let loop ()
                          ;; If we reached the end already on the previous iteration,
                          (if (not http-request-body-read)
                              ;; Return #t to signal EOF.
                              #t
                              ;; Otherwise,
                              ;;
                              ;; (If http-request-body-read returned a box on the last iteration, then it was also unboxed
                              ;; on the last iteration and we have it in procedure form ready to invoke here now.)
                              ;;
                              ;; Do another read operation,
                              (and (let* ((n (http-request-body-read data-thunk copying?))
                                          (boxed? (box? n)))
                                     (set! http-request-body-read (if boxed? (unbox n) n))
                                     (not boxed?)) ; And if we reached EOF (reported as n = #f, which leads to boxed? = #f), or
                                   ;; we have more data to process (reported as n = a procedure, which leads to
                                   ;; boxed? = #f too) return #t, meaning that we should continue iterating.
                                   ;;
                                   ;; Otherwise (the only case left is:), the data-thunk returned #f (reported
                                   ;; as n = a box, which leads to boxed? = #t), return #f, meaning that the
                                   ;; iteration process is ended here and this #f becomes |sack:body|'s return value.
                                   (loop))))))
  ((environment 'sack:body) sack:body)
  (sack-dbg "Into handle-sack-response. (version=" version ")")
  ;; Invoke handler (i.e. page generator)
  (call-with-values
      (lambda () (sack-application environment))
    (lambda (code headers response-thunk)
      ;; code = integer = send this HTTP response
      ;;        #f = close connection immediately
      ;;
      ;; headers = alist
      ;;
      ;; response-thunk = thunk = response data generator
      ;;                  #f = no response data
      (let* ((write-subu8vector (io-primitives-write-subu8vector io-primitives))
             (write-response-body-verbatim
              (lambda ()
                (let loop ()
                  (let ((chunk (and response-thunk (response-thunk))))
                    (if chunk
                        (http-util#chunk-return-value->u8v&u8v-length
                         chunk
                         (write-subu8vector u8v 0 u8v-length)
                         (loop)))))))
             (chunked-encoding?
              (and (not (has-header? headers "content-length"))
                   (not (has-header-with-value? headers
                                                "transfer-encoding"
                                                "chunked"))
                   ;; It seems that for web browsers to properly understand 304 Not Modified, we must not tell HTTP
                   ;; response has chunked encoding because this is received as that new content is coming in.
                   ;;
                   ;; Due to the great similarity with 303 See Other and probably all 3xx status codes,
                   ;; we treat all of them with this rule.
                   ;;
                   ;; (We add a first check here to see if code is set at all; if it's not we'll close the connection
                   ;; right away and the chunked-encoding? value won't have any effect anyhow.)
                   code (not (fx<= 300 code 399))))
             (has-close-header?
              (has-header-with-value? headers
                                      "connection"
                                      "close"))
             (close-connection?
              (or (not code)
                  (not (eq? 'HTTP/1.1 version))
                  (member "close"
                          ((environment 'sack:headers) "connection"))
                  has-close-header?
                  (<= keep-alive 1))))
        (if code ; code = #f means, don't write any data but just close the connection.
            (case version
              ((HTTP/1.1 HTTP/1.0)
               ;; Write HTTP response headers
               (handle-sack-response-headers io-primitives
                                             version
                                             code
                                             headers
                                             chunked-encoding?
                                             close-connection?
                                             has-close-header?)
               ;; Write HTTP response body
               (if (and (not (equal? "head"
                                     (environment 'sack:request-method)))
                        (not (eq? 304 code)))
                   (if chunked-encoding?
                       (http-write-with-chunked-encoding io-primitives
                                                         (and response-thunk (response-thunk)) ; first-chunk
                                                         response-thunk) ; get-chunk
                       (write-response-body-verbatim)))
               ;; If this is a keepalive connection then ensure that all HTTP request body contents available
               ;; have been read out. This is to ensure that on the start of processing of the next HTTP request
               ;; on the HTTP connection, we will start reading the HTTP request line at the correct place.
               (if (not close-connection?)
                   (sack:body (lambda (u8v len) (void)) copying?: #f)))
              ;; HTTP 0.9, just dump the HTTP response body as one data block.
              (else
               (write-response-body-verbatim))))
        (let ((keep-connection-alive? (not close-connection?)))
          ;; Takeover connection handling must be done here which is before the connection may be closed.
          ;;
          ;; Invoke |takeover-connection!-thunk|, if set
          (let ((takeover-connection-thunk (takeover-connection-thunk-get)))
            (if takeover-connection-thunk
                (begin
                  ;; Set connection's timeout to indefinite - when here otherwise in the current implementaiton we have keep-alive or alike
                  ;; set to timeout. This works as a solution for now.
                  (input-port-timeout-set!  gambit-port +inf.0 (lambda () ((io-primitives-close-port io-primitives)) #f))
                  (output-port-timeout-set! gambit-port +inf.0 (lambda () ((io-primitives-close-port io-primitives)) #f))
                  (if (not (takeover-connection-thunk io-primitives))
                      (set! keep-connection-alive? #f)))))
          ;; Was:
          ;; (if close-connection?
          ;;     (close-port conn)
          ;;     (force-output conn))
          ;;
          ;; Just to ensure completely that the data is sent (I believe this is superfluous but let's keep it for now):
          ((io-primitives-force-output io-primitives))
          (if close-connection? ((io-primitives-close-port io-primitives)))
          keep-connection-alive?)))))

;;------------------------------------------------------------------------------
;;!! Low-level serving functions.

(define version-table (make-token-table
                       ("HTTP/1.0" 'HTTP/1.0)
                       ("HTTP/1.1" 'HTTP/1.1)))

;; This procedure sets IO timeouts at the level of Gambit TCP port.
;; It's invoked by the HTTP TCP connection bootstrap code in |sack-start!|.
;;
;; ** Note: Currently this one overlaps timeout handlers already set by the io-primitives module.
(define (set-timeout! connection timeout io-primitives)
  ;; Configure the connection with the client so that if we can't read
  ;; the request after [timeout] seconds, the read/write operation
  ;; will fail (and the thread will terminate).
  (input-port-timeout-set! connection
                           timeout
                           (lambda ()
                             (sack-dbg "Port timed out as there was no new data within the timeout period of " timeout
                                       " seconds, so closing connection now.")
                                        ; (close-port connection)
                             ((io-primitives-close-port io-primitives))
                             #f)) ; Signals to Gambit that the operation that timed out should be cancelled.
  (output-port-timeout-set! connection
                            timeout
                            (lambda ()
                              (sack-dbg "Port timed out as no new data could be written within the timeout period of " timeout
                                        " seconds, so closing connection now.")
                                        ; (close-port connection)
                              ((io-primitives-close-port io-primitives))
                              #f))) ; Signals to Gambit that the operation that timed out should be cancelled.

(define (serve-connection sack-application
                          timeout
                          io-primitives
                          threaded?
                          port-number
                          uri-scheme
                          keep-alive
                          gambit-port ; In here we pass it to |make-environment| only, for resolving client IP.
                          on-exception-catch
                          print-caught-exception? print-max-head print-max-tail
                          on-exception-hook)
  (let reuse-connection ((keep-alive keep-alive))
    (let ((req (((io-primitives-read-line-until-crlf io-primitives)
                 #f))))                 ; = return-partial-string?
      (sack-dbg "Got request: " req)
      (if (not req)
          ;; If HTTP connection closed before sending us any data, then just close the connection.
          ;; This happens for instance when a Keepalive connection is closed.
          #!void
          (begin
            ;; (with-output-to-port console-output-port (lambda () (print "Got HTTP req line: ") (write req) (print ".\n")))
            ;; (set-timeout! connection timeout) - done by the connection bootstrack code in |sack-start!| now.
            ;; |set-timeout!| is specific to Gambit's IO system, that's the reason for keeping it out of |serve-connection|.
            (let* ((end
                    (let loop ((i 0))
                      (cond ((= i (string-length req))
                             #f)
                            ((char=? (string-ref req i) #\space)
                             i)
                            (else
                             (loop (+ i 1))))))
                   (method-name
                    (let ((m (and end (substring req 0 end))))
                      (if m (string-downcase! m)) ; (If there's no space in the method, m is #f.)
                      ;; We don't reuse |req| anywhere so mutating it is fine, saves us of an object allocation.
                      m)))
              (define takeover-connection-thunk #f)
              (define (takeover-connection-thunk-set! thunk) (set! takeover-connection-thunk thunk))
              (define (takeover-connection-thunk-get) takeover-connection-thunk)
              ;; Invoked by |handle-version| below only.
              ;; Presuming that it finds the HTTP request satisfactorily correct, it invokes |handle-sack-response| defined in
              ;; the global namespace above for proceeding with handling the HTTP request.
              ;;
              ;; This procedure performs any
              ;;
              ;; => keep-connection-alive?
              (define (handle-request version attributes uri)
                ;; This procedure performs the actual operation of this procedure; below it is
                ;; its context code for doing this with the proper exception handling.
                (define (handle-request-do)
                  ;; Add some more info to the uri object. This is
                  ;; useful for the sack environment object.
                  (let* ((host/port (let ((ret (assoc "host" attributes)))
                                      (and ret (cdr ret))))
                         (host+port (string-split-char #\: (or host/port
                                                               "0.0.0.0"))))
                                        ; TODO: Mutate URI object port slots instead.
                    (set! uri (uri-port-set   uri (or (and (pair? (cdr host+port))
                                                           (string->number (cadr host+port)))
                                                      port-number)))
                    (set! uri (uri-host-set   uri (car host+port)))
                    (set! uri (uri-scheme-set uri uri-scheme)))
                  ;; (with-output-to-port console-output-port (lambda () (print "Sack: Handles request " uri ".\n")))
                  ;; Create an environment closure. It is the core handling block for each HTTP request.
                  ;;
                  ;; It is passed on verbatim onto the Sack app thunk.
                  ;;
                  ;; Normally within a Sack app thunk, the environment goes under the variable name |env|.
                  (let* ((environment (make-environment threaded?
                                                        uri
                                                        method-name
                                                        attributes
                                                        gambit-port
                                                        takeover-connection-thunk-set!)))
                    ;; Proceed with handling the HTTP request.
                    (handle-sack-response keep-alive
                                          sack-application
                                          environment
                                          io-primitives
                                          version
                                          takeover-connection-thunk-get
                                          gambit-port)))
                ;; What exception handling and reporting behavior to apply here is directed by the following variables.
                (let* ((on-exception-catch      (if (eq? on-exception-catch      'default) sack-exception-on-exception-catch      on-exception-catch     ))
                       (print-caught-exception? (if (eq? print-caught-exception? 'default) sack-exception-print-caught-exception? print-caught-exception?))
                       (print-max-head          (if (eq? print-max-head          'default) sack-exception-print-max-head          print-max-head         ))
                       (print-max-tail          (if (eq? print-max-tail          'default) sack-exception-print-max-tail          print-max-tail         ))
                       (on-exception-hook       (if (eq? on-exception-hook       'default) sack-exception-on-exception-hook       on-exception-hook      )))
                  ;; This procedure is invoked when there's been an exception during sack application execution.
                  (define (handle-exception exception/continuation exception continuation)
                    ;; # Print exception to console, if applicable
                    (if print-caught-exception?
                        ;; Gambit's IO primitives are now set not to throw any exceptions for the HTTP connection port,
                        ;; so we should never get any exceptions related to it here.
                        ;;
                        ;; (if (and (os-exception? exception)
                        ;;          (let ((v (os-exception-arguments exception)))
                        ;;            (and (list? v) (>= (length v) 1)
                        ;;                (port? (car v))
                        ;;                 ; (eq? connection (car v)) - Now this new check is a bit arbitrary. Best would be if we
                        ;;                 ;                            could make the Gambit IO routines not throw any exceptions.
                        ;;                 )))
                        ;;
                        ;;     ; Typically this is a "Broken pipe" exception. Don't know exactly how
                        ;;     ; to typecheck for it though.
                        ;;     (print port: console-output-port
                        ;;            " #### Sack application crashed, most probably IO error from connection failure. " e "\n")
                        (print port: console-output-port
                               "\n\n #### Sack application crashed in thread " (current-thread) " with exception:\n"
                               (if (and exception/continuation (eq? #t print-caught-exception?))
                                   (exception/continuation->string exception/continuation
                                                                   #f ; = for-console
                                                                   #t ; = display-environment
                                                                   print-max-head print-max-tail)
                                   exception)))
                    ;; # Send internal server error to connection
                    ;; If we get an exception while doing that, ignore it.
                    (with-exception-catcher
                     (lambda (e) #t)
                     (lambda () (internal-server-error io-primitives)))
                    ;; # Call exception handling hook, if applicable
                    ;; We sent the error message on the connection above just in case the hook invocation here would take time.
                    ;;
                    ;; We invoke the hook here *within the same execution scope* as the sack-app is ordinarily executed.
                    ;; (And for instance, we don't start a separate thread for invoking the hook.)
                    ;; The reason we do it like this, is that the hook invocation potentially can be expensive; it may
                    ;; involves contacting a third party such as sending an email. Thus this way we save the hooks from needing
                    ;; to have their own overload code, and from such overload logics from becoming overloaded.
                    ;; This behavior makes perfect sense when put in relation with that exceptions should be extremely rare
                    ;; anyhow.
                    (if on-exception-hook
                        (on-exception-hook exception/continuation exception continuation))
                    ;; # Close HTTP connection
                    #f)           ; keep-connection-alive? = #f
                  ;; Do any exception catching at all?
                  (case on-exception-catch
                    ;; Yes, do exception & continuation catching
                    ((exception&continuation)
                     (with-exception/continuation-catcher
                      (lambda (exception/continuation) (handle-exception exception/continuation
                                                                    (exception/continuation-exception    exception/continuation)
                                                                    (exception/continuation-continuation exception/continuation)))
                      handle-request-do))
                    ;; Yes, do exception catching
                    ((exception)
                     (with-exception-catcher
                      (lambda (exception) (handle-exception #f ; exception/continuation
                                                       exception
                                                       #f)) ; continuation
                      handle-request-do))
                    ;; Don't do any exception catching,
                    ((#f)
                     (handle-request-do))
                    (else
                     (error "Invalid on-exception-catch value" on-exception-catch)))))
              ;; Invoked by request handling code below this define only.
              ;; On success, invokes |handle-request| defined above for proceeding with handling the HTTP request.
              ;;
              ;; Procedure to proceed with the handling of a HTTP request, given knowledge of the HTTP protocol
              ;; version number specified in the HTTP request.
              ;;
              ;; => keep-connection-alive?
              (define (handle-version version uri)
                (sack-dbg "Request uses HTTP version " version)
                (case version
                  ;; HTTP 1.0 or 1.1 request
                  ((HTTP/1.0 HTTP/1.1)
                   (sack-dbg "Request is HTTP 1.0 or 1.1.")
                   (let ((attributes (read-headers io-primitives))) ; Read headers.
                     (sack-dbg "Loaded headers " attributes)
                     (cond
                      ;; If failed to read headers, fail.
                      ((not attributes)
                       (sack-dbg "Request contained no attributes, aborting.")
                       (bad-request-error io-primitives)
                       #f)
                      ;; If is HTTP 1.1 request and has no Host: header, fail.
                      ;; (It is essential for any HTTP 1.1 request to have a Host: header.)
                      ((and (eq? 'HTTP/1.1 version)
                            (not (has-header? attributes "host")))
                       (sack-dbg "Request is for HTTP/1.1 but has no \"host\" attribute, aborting.")
                       (bad-request-error io-primitives)
                       #f)
                      ;; Proceed with handling request.
                      (else
                       (sack-dbg "Going into |handle-request|.")
                       (handle-request version attributes uri)))))
                  ;; HTTP 0.9 request
                  ((#f)
                   (sack-dbg "Treating HTTP request as being for HTTP/0.9, now going into |handle-request|.")
                   (handle-request 'HTTP/0.9
                                   '() ; headers - HTTP 0.9 does not support nor headers nor keepalives.
                                        ;           Here thusly we specify an empty list of headers.
                                   uri))
                  ;; Invalid HTTP version specified, fail.
                  (else
                   (sack-dbg "HTTP version is " version ", unknown, aborting.")
                   (bad-request-error io-primitives)
                   #f)))
              (if method-name
                  ;; So we got past parsing out the method field. Now, end + 1 is the string position at which the URL should start.
                  ;; We ask |parse-uri| of the uri module to parse it out, and call us back with the URI object loaded and with the
                  ;; index in the req string that the URI ended, as for us to know at what index + 1 the HTTP version description starts.
                  (parse-uri
                   req                  ; = str
                   (+ end 1)            ; = start
                   (string-length req)  ; = end
                   #t                   ; = decode?
                   (lambda (uri uri-end-at-string-index)
                     (sack-dbg "Got URI object " uri ", uri-end-at-string-index=" uri-end-at-string-index)
                     ;; Handle connection i.e. process the HTTP request fully and provide HTTP response fully.
                     (let* ((keep-connection-alive?
                             (cond
                              ;; If no URI was provided in HTTP request (or was so extremely misformatted that parse-uri
                              ;; wouldn't accept it), fail.
                              ((not uri)
                               (sack-dbg "parse-uri gave us #f for uri, must have been very broken.")
                               (bad-request-error io-primitives)
                               #f)
                              ;; If the HTTP request lacks HTTP version, then it's a HTTP 0.9 request.
                              ;; (For more info see http://stackoverflow.com/questions/6686261/what-at-the-bare-minimum-is-required-for-an-http-request
                              ;; and the HTTP 1.0 RFC, 1945.
                              ;;
                              ;; HTTP 0.9 connections do not support nor headers nor keepalives.
                              ((not (< uri-end-at-string-index (string-length req)))
                               (handle-version #f ; version (#f = 0.9)
                                               uri))
                              ;; If the URI is not followed by a whitespace, fail.
                              ((not (char=? (string-ref req uri-end-at-string-index) #\space))
                               (sack-dbg "No space after GET/POST+space+PATH part of HTTP request.")
                               (bad-request-error io-primitives)
                               #f)
                              ;; A HTTP version was specified. This is the general usecase.
                              (else
                               (let ((version-index (token-table-lookup-substring version-table
                                                                                  req
                                                                                  (+ uri-end-at-string-index 1)
                                                                                  (string-length req))))
                                 (if version-index
                                     ;; Valid HTTP version specified, proceed with handling request.
                                     (let ((http-protocol-version-sy (vector-ref version-table (+ version-index 1)))) ; 'HTTP/1.1 etc.
                                       (handle-version http-protocol-version-sy uri))
                                     ;; Invalid HTTP version specified, fail.
                                     (begin
                                       (sack-dbg "Bad HTTP version, tried to parse it out from " (string->list (substring req (+ uri-end-at-string-index 1) (string-length req)))
                                                 ". (=index " (+ uri-end-at-string-index 1) " up to the end of the string, whose length is " (string-length req))
                                       (bad-request-error io-primitives)
                                       #f)))))))
                       ;; If this connection is to be kept alive, then simply reiterate this HTTP request handling procedure.
                       ;;
                       ;; (Subroutines determine if this is the case - essentially if the request is with HTTP 1.1 and does
                       ;; not have a Connection: Close header set, then it will be kept alive.)
                       (if keep-connection-alive?
                           (reuse-connection (- keep-alive 1))))))
                  ;; Request line contained no method. This means it was a bad HTTP request.
                  ;; At least for now, we always send a full HTTP response here, if not else then as to make clear
                  ;; to the requestor that this is a web server.
                  ;; (method-not-implemented-error io-primitives)
                  (bad-request-error io-primitives))))))))
