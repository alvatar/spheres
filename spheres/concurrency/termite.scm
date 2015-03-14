;;!!! Termite (Erlang-like concurrency system)
;; .author Guillaume Germain, Copyright (C) 2005-2009, All Rights Reserved.
;; .author Alvaro Castro-Castilla, 2015.

;; See "examples/start1.sh" for a minimal Termite program.
;; The global environment should be the same on every node, because it
;; isn't included in the serialization of ojects.
;; One should avoid to make references to unserializable objects in
;; closures and continuations, else things will fail.
;; The programs should not use mutations.  Instead, rely on the fact that
;; passing messages around /is/ a representation of mutable state.  See
;; "examples/cons.scm" for an example.  Still, mutable data structures
;; can be hidden behind processes with some care.  Have a look at
;; 'data.scm' for examples.
;; To stay in the "spirit" of Termite, one should not use SET!, SET-CAR!,
;; SET-CDR!, VECTOR-SET!, STRING-SET! and similar functions.  Better
;; integration in the future with Gambit might prevent those forms and
;; functions from being available.
;;
;; Datatypes:
;; ---------
;; NODE -> node ID
;; (make-node ip-address tcp-port#)
;;
;; TAG -> universally unique identifier
;; (make-tag)
;;
;; Functions and special forms:
;; ---------------------------
;; (node-init node)
;; Necessary to initialize the system.
;;
;; (spawn thunk)
;; Start a new process executing the 'body' code and return its PID.
;;
;; (spawn-link thunk)
;; Start a new process executing the 'body' code and linking that process
;; to the current one and return its PID.
;;
;; (remote-spawn node thunk)
;; Spawn a new thunk on a remote node and return its PID.
;;
;; (self)
;; Get the PID of the running process.
;;
;; (current-node)
;; Get the current node we're executing on.
;;
;; (! pid message)
;; Send message to process.
;;
;; (? [timeout [default-value]])
;; Receive a message, block for 'timeout' seconds if no messages.  An
;; exception will be raised if no default-value is specified.
;;
;; (?? pred? [timeout [default-value]])
;; Receive a message for which (pred? message) is true.
;;
;; (recv
;;   (pattern                . code)
;;   (pattern (where clause) . code)
;;   (after   seconds        . code))
;; Selectively receive a message that match a pattern, and destructure
;; it.  The last clause can optionally be a 'timeout' clause, with code
;; to execute if no messages received after a certain amount of time.
;;
;; (!? pid message [timeout [default-value]])
;; Remote procedure call (or synchronous message).  This requires
;; doing something like:
;; (recv
;;   ...
;;   ((from token message) (! from (list token reply)))
;;   ...)
;;
;; (shutdown!)
;; Nicely terminate the execution of the current process.
;;
;; (terminate! pid)
;; Forcefully terminate the execution of a local process.


;; ----------------------------------------------------------------------------
;; System configuration & global data

(define current-node (lambda () (error "uninitialized node")))

(define *global-mutex* (make-mutex "global termite mutex"))

;; translation tables for "published" PIDs
(define *foreign->local* (make-table weak-values: #t))
(define *local->foreign* (make-table weak-keys: #t))
;; translation table for "published" tags
(define *uuid->tag* (make-table weak-values: #t))

;; Get the current time in seconds.
(define (now)
  (time->seconds
   (current-time)))

(define (formatted-current-time)
  (time->string (current-time)))

;; ----------------------------------------------------------------------------
;; Datatypes

(define (process? obj) (thread? obj))
(define (process-links pid) (thread-specific pid))
(define (process-links-set! pid obj) (thread-specific-set! pid obj))

;; universal pid
(define-type upid
  id: 9e096e09-8c66-4058-bddb-e061f2209838
  tag
  node)

;; nodes
(define-type node
  id: 8992144e-4f3e-4ce4-9d01-077576f98bc5
  read-only:
  host
  port)

;; tags
(define-type tag
  id: efa4f5f8-c74c-465b-af93-720d44a08374
  (uuid init: #f))

;; FIXME: hack due to syntax-case expander bug
;; (define-type tag
;;   id: efa4f5f8-c74c-465b-af93-720d44a08374
;;   constructor: make-tag/uuid
;;   uuid)
;; (define (make-tag) (make-tag/uuid #f))

;; * Test whether 'obj' is a pid.
(define (pid? obj)
  (or (process? obj) (upid? obj)))

;; NOTE It might be better to integrate with Gambit's exception mechanism
(define-type termite-exception
  id: 6a3a285f-02c4-49ac-b00a-aa57b1ad02cf
  origin
  reason
  object)


;; ----------------------------------------------------------------------------
;; process manipulation primitives

;;! Get the pid of the current process.
(define self current-thread)

;;! Start a new process executing the code in 'thunk'.
(define (spawn thunk #!key (links '()) (name 'anonymous))
  (let ((t (make-thread
            (lambda ()
              (with-exception-handler
               base-exception-handler
               thunk)
              (shutdown!))
            name)))
    (thread-specific-set! t links)
    (thread-start! t)
    t))

(define (spawn-linked-to to thunk #!key (name 'anonymous-linked-to))
  (spawn thunk links: (list to) name: name))

;;! Start a new process with a bidirectional link to the current
;; process.
(define (spawn-link thunk #!key (name 'anonymous-linked))
  (let ((pid (spawn thunk links: (list (self)) name: name)))
    (outbound-link pid)
    pid))

;;! Start a new process on remote node 'node', executing the code
;; in 'thunk'.
(define (remote-spawn node thunk #!key (links '()) (name 'anonymous-remote))
  (if (equal? node (current-node))
      (spawn thunk links: links name: name)
      (!? (remote-service 'spawner node)
          (list 'spawn thunk links name))))

;;! Start a new process on remote node 'node', with a bidirectional
;; link to the current process.
(define (remote-spawn-link node thunk)
  (let ((pid (remote-spawn node thunk links: (list (self)))))
    (outbound-link pid)
    pid))

;;! Cleanly stop the execution of the current process.  Linked
;; processes will receive a "normal" exit message.
(define (shutdown!)
  (for-each
   (lambda (pid)
     (! pid (make-termite-exception (self) 'normal #f)))
   (process-links (self)))
  (halt!))

;;! This is *not* nice: it wont propagate the exit message to the other
;; processes
(define (halt!)
  (thread-terminate! (current-thread)))

;;! Forcefully terminate a local process.  Warning: it only works on
;; local processes!  This should be used with caution.
(define (terminate! victim)
  (thread-terminate! victim)
  (for-each
   (lambda (link)
     (! link (make-termite-exception victim 'terminated #f)))
   (process-links victim)))

;; TODO 'wait-for' and 'alive?' should be grouped in a more general
;; procedure able to determine the status of a process (alive, dead,
;; waiting, etc.) and the procedure should work on remote processes

;;! Wait for the end of a process 'pid'.  Does not return anything.
;; Warning: will not work on remote processes.
(define (%wait-for pid)
  (with-exception-catcher
   (lambda (e)
     (void))
   (lambda ()
     (thread-join! pid)
     (void))))

;;! Check whether the process 'pid' is still alive.  Warning: will not
;; work on remote processes.
(define (%alive? pid)
  (with-exception-catcher
   (lambda (e)
     (join-timeout-exception? e))
   (lambda ()
     (thread-join! pid 0)
     #f)))


;;-------------------------------------------------------------------------------
;;!! Messages

;;! Send a message 'msg' to 'pid'.  This means that the message will
;; be enqueued in the mailbox of the destination process.
;;
;; Delivery of the message is unreliable in theory, but in practice
;; local messages will always be delivered, and remote messages will
;; not be delivered only if the connection is currently broken to the
;; remote node, or if the remote node is down.
;;
;; Note that you will not get an error or an exception if the message
;; doesn't get there: you need to handle errors yourself.
(define (! to msg)
  (cond
   ((process? to)
    (thread-send to msg))
   ((upid? to)
    (thread-send dispatcher (list 'relay to msg)))
   (else
    (error "invalid-message-destination" to))))

;; incorrect, because it doesn't handle exception messages
;; (define ? thread-receive)

;;! Retrieve the first message from the mailbox of the current
;; process.  If no message is available, the process will block until
;; a message is received.  If 'timeout' is specified, the process will
;; only block for that amount of time, and then raise an exception.
;; It is possible to also pass the 'default' argument to return a
;; value instead of raising an exception.
(define (? . opt) ;; TODO: inefficient, fix
  (termite-match opt
                 (()
                  (recv
                   (msg msg)))
                 ((timeout)
                  (recv
                   (msg msg)
                   (after timeout (thread-receive 0))))
                 ((timeout default)
                  (recv
                   (msg msg)
                   (after timeout default)))))

;; benchmark to see if faster...
;; (define (? #!optional (timeout +inf.0) (default (lambda (thread-receive 0))))
;;   (with-exception-catcher
;;    (lambda (exception)
;;      (if (mailbox-receive-timeout-exception? exception)
;;          (default)
;;          (raise exception)))
;;    (lambda ()
;;      (thread-receive timeout))))

;;! Retrieve the first message from the mailbox of the current
;; process that satisfised the predicate 'pred?'.  If no message
;; qualifies, the process will block until a message satisfying the
;; predicate is received.  If 'timeout' is specified, the process will
;; only block for that amount of time, and then raise an exception.
;; It is possible to also pass the 'default' argument to return a
;; value instead of raising an exception.
;; TODO: inefficient, fix
(define (?? pred? . opt)
  (termite-match opt
                 (()
                  (recv
                   (msg (where (pred? msg)) msg)))
                 ((timeout)
                  (recv
                   (msg (where (pred? msg)) msg)
                   (after timeout (thread-receive 0))))
                 ((timeout default)
                  (recv
                   (msg (where (pred? msg)) msg)
                   (after timeout default)))))


;;-------------------------------------------------------------------------------
;;!! Higher-order concurrency primitives

;;! Send a "synchronous" message to a process.  The message will be
;; annotated with a tag and the pid of the current process, therefore
;; sending a message of the form '(from tag msg)'.  The server
;; receiving the message must specifically handle that format of
;; message, and reply with a message of the form '(tag reply)'.
;;
;; Like for the |?| and |??| message retrieving operators, it is
;; possible to specify a 'timeout' to limit the amount of time to wait
;; for a reply, and a 'default' value to return if no reply has been
;; received.
;; RPC
(define (!? pid msg . opt)
  (let ((tag (make-tag)))
    (! pid (list (self) tag msg))
    (termite-match opt
                   (()
                    (recv
                     ((,tag reply) reply)))
                   ((timeout)
                    (recv
                     ((,tag reply) reply)
                     (after timeout (raise 'timeout))))
                   ((timeout default)
                    (recv
                     ((,tag reply) reply)
                     (after timeout default))))))

;;! Evaluate a 'thunk' on a remote node and return the result of that
;; evaluation.  Just like for |!?|, |?| and |??|, it is possible to
;; specify a 'timeout' and a 'default' argument.
(define (on node thunk)
  (let ((tag (make-tag))
        (from (self)))
    (remote-spawn node
                  (lambda ()
                    (! from (list tag (thunk)))))
    (recv
     ((,tag reply) reply))))


;;-------------------------------------------------------------------------------
;;!! Termite I/O

;; Wraps 'pid's representing Gambit output ports.
(define-type termite-output-port
  id: b0c30401-474c-4e83-94b4-d516e00fe363
  unprintable:
  pid)

;; Wraps 'pid's representing Gambit input ports.
(define-type termite-input-port
  id: ebb22fcb-ca61-4765-9896-49e6716471c3
  unprintable:
  pid)

;;! Start a process representing a Gambit output port.
(define (spawn-output-port port #!optional (serialize? #f))
  (output-port-readtable-set!
   port
   (readtable-sharing-allowed?-set
    (output-port-readtable port)
    serialize?))
  (make-termite-output-port
   (spawn
    (lambda ()
      (let loop ()
        (recv
         (proc
          (where (procedure? proc))
          (proc port))
         (x (termite-warning "unknown message sent to output port: " x)))
        (loop)))
    name: 'termite-output-port)))

;;! Start a process representing a Gambit input port.
(define (spawn-input-port port #!optional (serialize? #f))
  (input-port-readtable-set!
   port
   (readtable-sharing-allowed?-set
    (input-port-readtable port)
    serialize?))
  (make-termite-input-port
   (spawn
    (lambda ()
      (let loop ()
        (recv
         ((from token proc)
          (where (procedure? proc))
          (! from (list token (proc port))))
         (x (termite-warning "unknown message sent to input port: " x)))
        (loop)))
    name: 'termite-input-port)))

;; IO parameterization
;; (define current-termite-input-port (make-parameter #f))
;; (define current-termite-output-port (make-parameter #f))


;;-------------------------------------------------------------------------------
;;! Distribution

;;! Convert a 'pid'
(define (pid->upid obj)
  (mutex-lock! *global-mutex*)
  (cond
   ((table-ref *local->foreign* obj #f)
    => (lambda (x)
         (mutex-unlock! *global-mutex*)
         x))
   (else
    (let ((upid (make-upid (make-uuid) (current-node))))
      (table-set! *local->foreign* obj upid)
      (table-set! *foreign->local* upid obj)
      (mutex-unlock! *global-mutex*)
      upid))))

(define (tag->utag obj)
  (mutex-lock! *global-mutex*)
  (cond
   ((tag-uuid obj)
    (mutex-unlock! *global-mutex*)
    obj)
   (else
    (let ((uuid (make-uuid)))
      (tag-uuid-set! obj uuid)
      (table-set! *uuid->tag* uuid obj)
      (mutex-unlock! *global-mutex*)
      obj))))

(define (serialize-hook obj)
  (cond
   ((process? obj)
    (pid->upid obj))
   ((tag? obj)
    (tag->utag obj))
   ;; unserializable objects, so instead of crashing we set them to #f
   ((or (port? obj))
    #f)
   (else obj)))

(define (upid->pid obj)
  (cond
   ((table-ref *foreign->local* obj #f)
    => (lambda (pid) pid))
   ((and (symbol? (upid-tag obj))
         (resolve-service (upid-tag obj)))
    => (lambda (pid)
         pid))
   (else
    (error "don't know how to upid->pid"))))

(define (utag->tag obj)
  (let ((uuid (tag-uuid obj)))
    (cond
     ((table-ref *uuid->tag* uuid #f)
      => (lambda (tag) tag))
     (else obj))))

(define (deserialize-hook obj)
  (cond
   ((and (upid? obj)
         (equal? (upid-node obj)
                 (current-node)))
    (upid->pid obj))
   ((tag? obj)
    (utag->tag obj))
   (else obj)))


(define (serialize obj port)
  (let* ((serialized-obj
          (object->u8vector obj serialize-hook))
         (len
          (u8vector-length serialized-obj))
         (serialized-len
          (u8vector (bitwise-and len #xff)
                    (bitwise-and (arithmetic-shift len -8) #xff)
                    (bitwise-and (arithmetic-shift len -16) #xff)
                    (bitwise-and (arithmetic-shift len -24) #xff))))
    (begin
      (write-subu8vector serialized-len 0 4 port)
      (write-subu8vector serialized-obj 0 len port))))

(define (deserialize port)
  (let* ((serialized-len
          (u8vector 0 0 0 0))
         (n
          (read-subu8vector serialized-len 0 4 port)))
    (cond ((= 0 n)
           #!eof)
          ((not (= 4 n))
           (error "deserialization error"))
          (else
           (let* ((len
                   (+ (u8vector-ref serialized-len 0)
                      (arithmetic-shift (u8vector-ref serialized-len 1) 8)
                      (arithmetic-shift (u8vector-ref serialized-len 2) 16)
                      (arithmetic-shift (u8vector-ref serialized-len 3) 24)))
                  (serialized-obj
                   (make-u8vector len))
                  (n
                   (read-subu8vector serialized-obj 0 len port)))
             (if (not (eqv? len n))
                 (begin
                   (error "deserialization error"
                          (list len: len n: n)))
                 (let ((obj (u8vector->object serialized-obj deserialize-hook)))
                   (if (vector? obj)
                       (vector->list obj)
                       obj))))))))

(define (start-serializing-output-port port)
  (spawn-link
   (lambda ()
     (let loop ()
       (recv
        (('write data)
         ;; (debug out: data)
         (serialize data port)
         (force-output port)) ;; io override
        (msg
         (termite-warning "serializing-output-port ignored message: " msg)))
       (loop)))
   name: 'termite-serializing-output-port))

(define (start-serializing-active-input-port port receiver)
  (spawn-link
   (lambda ()
     (let loop ()
       (let ((data (deserialize port)))
         ;; to receive exceptions...
         (? 0 'ok)
         ;; (debug in: data)
         (if (eof-object? data) (shutdown!))
         (! receiver (list (self) data))
         (loop))))
   name: 'termite-serializing-active-input-port))

;; a tcp server listens on a certain port for new tcp connection
;; requests, and call ON-CONNECT to deal with those new connections.
(define (start-tcp-server tcp-port-number on-connect)
  (let ((tcp-server-port
         (open-tcp-server (list
                           port-number: tcp-port-number
                           coalesce: #f))))
    (spawn
     (lambda ()
       (let loop ()
         (on-connect (read tcp-server-port)) ;; io override
         (loop)))
     name: 'termite-tcp-server)))

;; MESSENGERs act as proxies for sockets to other nodes

;; initiate a new bidirectional connection to another node important:
;; caller is responsible for registering it with the dispatcher
(define (initiate-messenger node)
  ;; (print "OUTBOUND connection established\n")
  (spawn
   (lambda ()
     (with-exception-catcher
      (lambda (e)
        (! dispatcher (list 'unregister (self)))
        (shutdown!))
      (lambda ()
        (let ((socket (open-tcp-client
                       (list server-address: (node-host node)
                             port-number:    (node-port node)
                             coalesce:       #f))))
          ;; the real interesting part
          (let ((in  (start-serializing-active-input-port socket (self)))
                (out (start-serializing-output-port socket)))
            (! out (list 'write (current-node)))
            (messenger-loop node in out))))))
   name: 'termite-outbound-messenger))

;; start a MESSENGER for an 'inbound' connection (another node
;; initiated the bidirectional connection, see initiate-messenger)
(define (start-messenger socket)
  ;; (print "INBOUND connection established\n")
  (spawn
   (lambda ()
     (with-exception-catcher
      (lambda (e)
        (! dispatcher (list 'unregister (self)))
        (shutdown!))
      (lambda ()
        (let ((in  (start-serializing-active-input-port socket (self)))
              (out (start-serializing-output-port socket)))
          (recv
           ((,in node)
            ;; registering messenger to local dispatcher
            (! dispatcher (list 'register (self) node))
            (messenger-loop node in out)))))))
   name: 'termite-inbound-messenger))

(define (messenger-loop node in out)
  (recv
   ;; incoming message
   ((,in ('relay id message))
    (let ((to (upid->pid (make-upid id (current-node)))))
      (! to message)))
   ;; outgoing message
   (('relay to message)
    ;; 'to' is a upid
    (let* ((id (upid-tag to))
           ;; (node (upid-node to))
           ;; (host (node-host node))
           ;; (port (node-id node))
           )
      (! out (list 'write (list 'relay id message)))))
   ;; unknown message
   (msg
    (termite-warning "messenger-loop ignored message: " msg)))
  (messenger-loop node in out))

;; the DISPATCHER dispatches messages to the right MESSENGER, it keeps
;; track of known remote nodes
(define dispatcher
  (let ((remove
         (lambda (proc lst)
           (let recur ((lst lst))
             (if (null? lst)
                 '()
                 (let ((head (car lst)))
                   (if (proc head)
                       (recur (cdr lst))
                       (cons head (recur (cdr lst))))))))))
    (spawn
     (lambda ()
       ;; the KNOWN-NODES of the DISPATCHER LOOP is an a-list of NODE => MESSENGER
       (let loop ((known-nodes '()))
         (recv
          (('register messenger node)
           (loop (cons (cons node messenger) known-nodes)))
          (('unregister messenger)
           (loop (remove (lambda (m) (equal? (cdr m) messenger)) known-nodes)))
          (('relay upid message)
           (let ((node (upid-node upid)))
             (cond
              ;; the message should be sent locally (ideally should not happen
              ;; for performance reasons, but if the programmer wants to do
              ;; that, then OK...)
              ((equal? node (current-node))
               (! (upid->pid upid) message)
               (loop known-nodes))
              ;; the message is destined to a pid on a known node
              ((assoc node known-nodes)
               => (lambda (messenger)
                    (! (cdr messenger) (list 'relay upid message))
                    (loop known-nodes)))

              ;; unconnected node, must connect
              (else
               (let ((messenger (initiate-messenger node)))
                 (! messenger (list 'relay upid message))
                 (loop (cons (cons node messenger) known-nodes)))))))

          (msg
           (termite-warning "dispatcher ignored message: " msg) ;; uh...
           (loop known-nodes)))))
     name: 'termite-dispatcher)))

;;-------------------------------------------------------------------------------
;;!! Services

;;! LINKER (to establish exception-propagation links between processes)
(define linker
  (spawn
   (lambda ()
     (let loop ()
       (recv
        (('link from to)
         (cond
          ((process? from)
           (process-links-set! from (cons to (process-links from)))) ;;;;;;;;;;
          ((upid? from)
           (! (remote-service 'linker (upid-node from))
              (list 'link from to)))
          (else
           (termite-warning "in linker-loop: unknown object"))))
        (msg
         (termite-warning "linker ignored message: " msg)))
       (loop)))
   name: 'termite-linker))

;;! Remote spawning
;; the SPAWNER answers remote-spawn request
(define spawner
  (spawn
   (lambda ()
     (let loop ()
       (recv
        ((from tag ('spawn thunk links name))
         (! from (list tag (spawn thunk links: links name: name))))
        (msg
         (termite-warning "spawner ignored message: " msg)))
       (loop)))
   name: 'termite-spawner))

;; the PUBLISHER is used to implement a mutable global env. for
;; process names
(define publisher
  (spawn
   (lambda ()
     (define dict (make-dict))
     (let loop ()
       (recv
        (('publish name pid)
         (dict-set! dict name pid))
        (('unpublish name pid)
         (dict-set! dict name))
        ((from tag ('resolve name))
         (! from (list tag (dict-ref dict name))))
        (msg
         (termite-warning "puslisher ignored message: " msg)))
       (loop)))
   name: 'termite-publisher))

(define (publish-service name pid)
  (! publisher (list 'publish name pid)))

(define (unpublish-service name pid)
  (! publisher (list 'unpublish name pid)))

;; This should probably only used internally
(define (resolve-service name #!optional host)
  (!? publisher (list 'resolve name)))

;;! Get the pid of a service on a remote node 'node' which has been
;; published with |publish-service| to the name 'service-name'.
(define (remote-service service-name node)
  (make-upid service-name node))


;;-------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------


;;!! Erlang/OTP-like behavior for "generic servers"

;; "Types" for the functions in a SERVER plugin
;;
;; INIT      :: args         -> state
;; CALL      :: term   state -> reply state
;; CAST      :: term   state -> state
;; TERMINATE :: reason state -> void
(define-type server-plugin
  id: 2ca2d07c-5d6a-44a8-98eb-422b2b8e7296
  read-only:
  init
  call
  cast
  terminate)

(define *server-timeout* 1)

(define (make-server plugin)
  (let loop ((state #f))
    (recv
     ((from tag ('init args))
      (let ((state ((server-plugin-init plugin) args)))
        (! from (list tag state))
        (loop state)))

     ((from tag ('call term))
      (call-with-values
          (lambda ()
            ((server-plugin-call plugin) term state))
        (lambda (reply state)
          (! from (list tag reply))
          (loop state))))
     (('cast term)
      (loop ((server-plugin-cast plugin) term state)))
     (('stop reason)
      ((server-plugin-terminate plugin) reason state)))))

(define (internal-server-start spawner plugin args name)
  (let ((server (spawner (lambda () (make-server plugin)) name: name)))
    (!? server (list 'init args) *server-timeout*)
    server))

(define (server:start plugin args #!key (name 'anonymous-generic-server))
  (internal-server-start spawn plugin args name))

(define (server:start-link plugin args #!key (name 'anonymous-linked-generic-server))
  (internal-server-start spawn-link plugin args))

(define (server:call server term)
  (!? server (list 'call term) *server-timeout*))

(define (server:cast server term)
  (! server (list 'cast term)))

(define (server:stop server reason)
  (! server (list 'stop reason)))


;; build a trivial server, give a single callback to be invoked on
;; calls, should expect two values and return two
(define (make-simple-server-plugin initial-state callback)
  (make-server-plugin
   ;; INIT
   (lambda (args)
     initial-state)
   ;; CALL
   (lambda (term state)
     (callback term state))
   ;; CAST
   (lambda (args state)
     state)
   ;; TERMINATE
   (lambda (reason state)
     (void))))

;;!! Erlang/OTP-like behavior for "event handlers"

;; "Types" for the functions in a EVENT-HANDLER
;;
;; INIT      :: arg           -> state
;; NOTIFY    :: event  state  -> state
;; CALL      :: args   state  -> reply state
;; TERMINATE :: reason state  -> void
(define-type event-handler
  id: 1d3007b8-c5aa-4090-ab55-e352040a4498
  read-only:
  init
  notify
  call
  terminate)

(define *event-manager-timeout* 1)

(define (event-manager)
  (define (remove proc lst)
    (let recur ((lst lst))
      (if (null? lst)
          '()
          (let ((head (car lst)))
            (if (proc head)
                (recur (cdr lst))
                (cons head (recur (cdr lst))))))))
  (let loop ((handlers '()))
    (recv
     ((from tag ('call handler args))
      (termite-match (assq handler handlers)
                     ((handler . state)
                      (call-with-values
                          (lambda ()
                            ((event-handler-call handler) args state))
                        (lambda (reply state)
                          (! from (list tag reply))
                          (loop (cons (cons handler state)
                                      (remove (lambda (x)
                                                (eq? (car x) handler))
                                              handlers))))))
                     (#f (error "handler doesn't exists"))))
     ;; should check to avoid duplicates
     (('add-handler handler args)
      (loop (cons (cons handler
                        ((event-handler-init handler) args))
                  handlers)))
     (('notify event)
      (loop (map
             (lambda (pair)
               (termite-match pair
                              ((handler . state)
                               (cons handler
                                     ((event-handler-notify handler) event state)))))
             handlers)))
     (('stop)
      (for-each
       (lambda (pair)
         (termite-match pair
                        ((handler . state)
                         ((event-handler-terminate handler) 'normal state))))
       handlers)
      (void)))))

(define (internal-event-manager-start spawner handlers name)
  (let ((em (spawn event-manager name: name)))
    (for-each
     (lambda (handler)
       (event-manager:add-handler em handler))
     handlers)
    em))

(define (event-manager:start
         #!key (name 'anonymous-event-manager)
         #!rest handlers)
  (internal-event-manager-start spawn handlers name))

(define (event-manager:start-link
         #!key (name 'anonymous-linked-event-manager)
         #!rest handlers)
  (internal-event-manager-start spawn-link handlers name))

(define (event-manager:add-handler event-manager handler . args)
  (! event-manager (list 'add-handler handler args)))

(define (event-manager:notify event-manager event)
  (! event-manager (list 'notify event)))

(define (event-manager:call event-manager handler args)
  (!? event-manager (list 'call handler args) *event-manager-timeout*))

(define (event-manager:stop event-manager)
  (! event-manager (list 'stop)))

;; build a trivial event handler with no state, only invoking a callback on any event
(define (make-simple-event-handler callback initial-state)
  (make-event-handler
   ;; INIT
   (lambda (args)
     initial-state)
   ;; NOTIFY
   (lambda (event state)
     (callback event state))
   ;; CALL
   (lambda (args state)
     (values (void) state))
   ;; TERMINATE
   (lambda (reason state)
     (void))))


;; ----------------------------------------------------------------------------
;;!! Various mutable data structures implemented behind processes

;; (it would be "better" if those were implemented functionally)
(define (data-make-process-name type)
  (string->symbol
   (string-append
    (symbol->string
     (thread-name
      (current-thread)))
    "-"
    (symbol->string type))))

;;! Cells
(define (make-cell #!key (name (data-make-process-name 'cell))
                   #!rest content)
  (spawn
   (lambda ()
     (let loop ((content (if (pair? content)
                             (car content)
                             (void))))
       (recv
        ((from tag 'empty?)
         (! from (list tag (eq? (void) content)))
         (loop content))
        ((from tag 'ref)
         (! from (list tag content))
         (loop content))
        (('set! content)
         (loop content)))))
   name: name))

(define (cell-ref cell)
  (!? cell 'ref))

(define (cell-set! cell value)
  (! cell (list 'set! value)))

(define (cell-empty! cell)
  (! cell (list 'set! (void))))

(define (cell-empty? cell)
  (!? cell 'empty?))

;; or: (define-termite-type cell content)

;; Mutable record created with this are implemented as processes.
;; (define-macro (define-termite-type type id tag . fields)
;;   (define (symbol-append . symbols)
;;     (string->symbol
;;      (apply
;;       string-append
;;       (map symbol->string symbols))))
;;   (define (make-maker type)
;;     (symbol-append 'make '- type))
;;   (define (make-getter type field)
;;     (symbol-append type '- field))
;;   (define (make-setter type field)
;;     (symbol-append type '- field '-set!))
;;   (if (not (eq? id id:))
;;       (error "id: is mandatory in define-termite-type"))
;;   (let* ((maker (make-maker type))
;;          (getters (map (lambda (field)
;;                          (make-getter type field))
;;                        fields))
;;          (setters (map (lambda (field)
;;                          (make-setter type field))
;;                        fields))
;;          (internal-type (gensym type))
;;          (internal-maker (make-maker internal-type))
;;          (internal-getters (map (lambda (field)
;;                                   (make-getter internal-type field))
;;                                 fields))
;;          (internal-setters (map (lambda (field)
;;                                   (make-setter internal-type field))
;;                                 fields))
;;          (facade-maker (gensym maker))
;;          (plugin (gensym (symbol-append type '-plugin)))
;;          (pid (gensym 'pid)))
;;     `(begin
;;        (define-type ,type
;;          id: ,tag
;;          constructor: ,facade-maker
;;          unprintable:
;;          ,pid)
;;        (define-type ,internal-type
;;          ,@fields)
;;        (define ,plugin
;;          (make-server-plugin
;;           ;; init
;;           (lambda (args)
;;             (apply ,internal-maker args))
;;           ;; call
;;           (lambda (term state)
;;             (match term
;;                    ,@(map (lambda (getter internal-getter)
;;                             `(',getter (values (,internal-getter state) state)))
;;                           getters
;;                           internal-getters)))
;;           ;; cast
;;           (lambda (term state)
;;             (match term
;;                    ,@(map (lambda (setter internal-setter)
;;                             `((',setter x) (,internal-setter state x) state))
;;                           setters
;;                           internal-setters)))
;;           ;; terminate
;;           (lambda (reason state)
;;             (void))))
;;        (define (,maker ,@fields)
;;          (,facade-maker (server:start ,plugin (list ,@fields) name: ',type)))
;;        ,@(map (lambda (getter)
;;                 `(define (,getter x)
;;                    (server:call (,(make-getter type pid) x)
;;                                 ',getter)))
;;               getters)
;;        ,@(map (lambda (setter)
;;                 `(define (,setter x value)
;;                    (server:cast (,(make-getter type pid) x)
;;                                 (list ',setter value))))
;;               setters))))

;;! Dictionary
(define (make-dict #!key (name (data-make-process-name 'dictionary)))
  (spawn
   (lambda ()
     (let ((table (make-table test: equal?
                              init: #f)))
       (let loop ()
         (recv
          ((from tag ('dict?))
           (! from (list tag #t)))
          ((from tag ('dict-length))
           (! from (list tag (table-length table))))
          ((from tag ('dict-ref key))
           (! from (list tag (table-ref table key))))
          (('dict-set! key)
           (table-set! table key))
          (('dict-set! key value)
           (table-set! table key value))
          ((from tag ('dict-search proc))
           (! from (list tag (table-search proc table))))
          (('dict-for-each proc)
           (table-for-each proc table))
          ((from tag ('dict->list))
           (! from (list tag (table->list table))))
          ((msg
            (termite-warning (list ignored: msg)))))
         (loop))))
   name: name))

(define (dict? dict)
  (!? dict (list 'dict?) 1 #f)) ;; we only give a second to reply to this

(define (dict-length dict)
  (!? dict (list 'dict-length)))

(define (dict-ref dict key)
  (!? dict (list 'dict-ref key)))

(define (dict-set! dict . args)
  (termite-match args
                 ((key)
                  (! dict (list 'dict-set! key)))
                 ((key value)
                  (! dict (list 'dict-set! key value)))))

(define (dict-search proc dict)
  (!? dict (list 'dict-search proc)))

(define (dict-for-each proc dict)
  (! dict (list 'dict-for-each proc)))

(define (dict->list dict)
  (!? dict (list 'dict->list)))

;; test...

;; (init)
;;
;; (define dict (make-dict))
;;
;; (print (dict->list dict))
;; (dict-set! dict 'foo 123)
;; (dict-set! dict 'bar 42)
;; (print (dict->list dict))
;; (print (dict-search (lambda (k v) (eq? k 'bar) v) dict))
;; (dict-for-each (lambda (k v) (print k)) dict)
;; (dict-set! dict 'foo)
;; (print (dict->list dict))
;; (? 1 #t)


;;! Bag
(define (make-bag #!key (name (data-make-process-name 'bag)))
  (spawn
   (lambda ()
     (let ((table (make-table test: equal?
                              init: #f)))
       (let loop ()
         (recv
          ((from tag ('bag?))
           (! from (list tag #t)))
          ((from tag ('bag-length))
           (! from (list tag (table-length table))))
          (('bag-add! elt)
           (table-set! table elt #t))
          (('bag-remove! elt)
           (table-set! table elt))
          ((from tag ('bag-member? elt))
           (table-ref table elt))
          ((from tag ('bag-search proc))
           (! from (list tag (table-search (lambda (k v) (proc k)) table))))
          (('bag-for-each proc)
           (table-for-each (lambda (k v) (proc k)) table))
          ((from tag ('bag->list))
           (! from (list tag (map car (table->list table))))))
         (loop))))
   name: name))

(define (bag? bag)
  (!? bag (list 'bag?) 1 #f)) ;; we only give a second to reply to this

(define (bag-length bag)
  (!? bag (list 'bag-length)))

(define (bag-add! bag elt)
  (! bag (list 'bag-add! elt)))

(define (bag-remove! bag elt)
  (! bag (list 'bag-remove! elt)))

(define (bag-member? bag elt)
  (!? bag (list 'bag-member? elt)))

(define (bag-search proc bag)
  (!? bag (list 'bag-search proc)))

(define (bag-for-each proc bag)
  (! bag (list 'bag-for-each proc)))

(define (bag->list bag)
  (!? bag (list 'bag->list)))

;; test...

;; (init)
;;
;; (define bag (make-bag))
;;
;; (print (bag->list bag))
;; (bag-add! bag 'foo)
;; (bag-add! bag 'bar)
;; (print (bag->list bag))
;; (print (bag-search (lambda (elt) (eq? elt 'bar) elt) bag))
;; (bag-for-each (lambda (elt) (print elt)) bag)
;; (bag-remove! bag 'foo)
;; (print (bag->list bag))
;; (? 1 #t)


;;-------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------


;;!! Migration

;; Task moves away, lose identity
(define (migrate-task node)
  (call/cc
   (lambda (k)
     (remote-spawn node (lambda () (k #t)))
     (halt!))))

;; Task moves away, leave a proxy behind
(define (migrate/proxy node)
  (define (proxy pid)
    (let loop ()
      (! pid (?))
      (loop)))
  (call/cc
   (lambda (k)
     (proxy
      (remote-spawn-link node (lambda () (k #t)))))))

;;!! Ping

(define ping-server
  (spawn
   (lambda ()
     (let loop ()
       (recv
        ((from tag 'ping)
         (! from (list tag 'pong)))
        (msg (termite-debug "ping-server ignored message" msg)))
       (loop)))
   name: 'termite-ping-server))

(define (ping node #!optional (timeout 1.0))
  (!? (remote-service 'ping-server node) 'ping timeout 'no-reply))


;; ----------------------------------------------------------------------------
;;!! A logging facility for Termite

;; (Ideally, this should be included with the services, but the
;; writing style is much different.  Eventually, the other services
;; might use similar style.)
(define (report-event event port)
  (termite-match event
                 ((type who messages)
                  (with-output-to-port port
                    (lambda ()
                      (newline)
                      (display "[")
                      (display type)
                      (display "] ")
                      (display (formatted-current-time))
                      (newline)
                      (display who)
                      (newline)
                      (for-each (lambda (m) (display m) (newline)) messages)
                      (force-output))))
                 (_ (display "catch-all rule invoked in reporte-event")))
  port)

(define file-output-log-handler
  (make-event-handler
   ;; init
   (lambda (args)
     (termite-match args
                    ((filename)
                     (open-output-file (list path: filename
                                             create: 'maybe
                                             append: #t)))))
   ;; event
   report-event
   ;; call
   (lambda (term port)
     (values (void) port))
   ;; shutdown
   (lambda (reason port)
     (close-output-port port))))

;; 'type' is a keyword (error warning info debug)
(define (termite-log type message-list)
  (event-manager:notify logger (list type (self) message-list)))

(define (termite-warning . terms)
  (termite-log 'warning terms))

(define (termite-info . terms)
  (termite-log 'info terms))

(define (termite-debug . terms)
  (termite-log 'debug terms))

(define logger
  (let ((logger (event-manager:start name: 'termite-logger)))
    (event-manager:add-handler logger
                               (make-simple-event-handler
                                report-event
                                (current-error-port)))
    (event-manager:add-handler logger
                               file-output-log-handler
                               "_termite.log")
    logger))


;;-------------------------------------------------------------------------------
;;!! Links and exception handling

;; Base exception handler for Termite processes.
(define (base-exception-handler e)
  (continuation-capture
   (lambda (k)
     (let ((log-crash
            (lambda (e)
              (termite-log
               'error
               (list
                (call-with-output-string ""
                                         (lambda (port)
                                           (display-exception-in-context e k port))))))))
       (cond
        ;; Propagated Termite exception?
        ((termite-exception? e)
         (if (not (eq? (termite-exception-reason e) 'normal))
             (log-crash (termite-exception-object e)))
         (for-each
          (lambda (pid) (! pid e))
          (process-links (self)))
         (halt!))
        ;; Gambit exception in the current process
        (else
         (log-crash e)
         (for-each
          (lambda (pid)
            (! pid (make-termite-exception (self) 'failure e)))
          (process-links (self)))
         (halt!)))))))

;; Default callback for received exceptions.
(define (handle-exception-message event)
  (raise event))

;;! Link another process 'pid' /to/ the current one: any exception
;; not being caught by the remote process and making it crash will be
;; propagated to the current process.
(define (inbound-link pid)
  (! linker (list 'link pid (self))))

;;! Link the current process /to/ another process 'pid': any
;; exception not being caught by the current process will be
;; propagated to the remote process.
(define (outbound-link pid)
  (let* ((links (process-links (self))))
    (if (not (memq pid links))
        (process-links-set! (self) (cons pid links)))))

;;! Link bidirectionally the current process with another process
;; 'pid': any exception not being caught in any of the two processes
;; will be propagated to the other one.
(define (full-link pid)
  (inbound-link  pid)
  (outbound-link pid))


;;-------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------
;;-------------------------------------------------------------------------------

;;!! Initialization

(process-links-set! (self) '())

(define (node-init node)
  (start-tcp-server (node-port node) start-messenger)
  (set! current-node (lambda () node))
  (publish-external-services)
  'ok)

(define (publish-external-services)
  ;; --------------------
  ;; Services
  ;; publishing the accessible exterior services
  ;; (essentially, opening the node to other nodes)
  (publish-service 'spawner spawner)
  (publish-service 'linker linker)
  (publish-service 'ping-server ping-server))


;; Some convenient definitions
;; (define node1 (make-node "localhost" 3001))
;; (define node2 (make-node "localhost" 3002))

