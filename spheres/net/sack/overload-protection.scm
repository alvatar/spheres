;;!!! Sack overload protection module
;; .author Mikael More
;; .author Alvaro Castro-Castilla
;;
;; Copyright (C) 2013 Mikael More
;;
;; Provides Sack with simple, efficient overload protection.
;;
;; Interfaces Sack through its overload primitives |accept-block| and |on-connect|.
;;
;; Does not attempt to work around a DDOS, for instance by finding and blocking overloading IP:s.
;;
;; ** Please note that
;;
;; ## TODO
;; (Nothing.)

(declare (block)
         (standard-bindings)
         (extended-bindings))

;; Values: #f = none
;;         #t = report overload blocks
;;         2 = report every accept-block
;;         3 = like 2 + report every disconnect
;;         4 = like 3 + report every connect (we offer this to provide a way for printouts of the
;;             connection object, for the programmer to ensure it's always unique as intended.)
(define overload-protection-verbose? #!void)
(set!   overload-protection-verbose? #f) ; To work around the (block).

(define overload-protection-verbose-port (console-port))

(define-macro (overload-protection-dbg-force . a)
  `(begin
     (print port: overload-protection-verbose-port "overload-protection: " ,@a "\n")
     (force-output overload-protection-verbose-port 1)))

(define-macro (overload-protection-dbg . a)
  `(if overload-protection-verbose?
       (overload-protection-dbg-force ,@a)))

(define* (make-overload-protection max-concurrent-connections
                                   (zero-keepalives-as-to-maintain-%-slots-available: 0.33)
                                   (debug-name: #f))
  (define-macro (+! value to-add)
    `(let ((n (+ ,value ,to-add)))
       (set! ,value n)
       n))
  (define-macro (-! value to-add)
    `(let ((n (- ,value ,to-add)))
       (set! ,value n)
       n))
  (define-macro (+1! value)
    `(+! ,value 1))
  (define-macro (-1! value)
    `(-! ,value 1))
  (define sack-thread #f)
  (define induce-block!? #f)
  (define connections (make-table
                       ;; eq? for connection closure aka monitoring-closure.
                       test: eq?
                       ;; Value is set to #t.
                       ;; Sack uses a will object on the Gambit port to detect when a connection will be GC:ed, as to
                       ;; consistently call (on-connection monitoring-closure #t) for them even if the respective Sack
                       ;; thread failed with exception so the ordinary code path to accomplish that not is invoked.
                       ;;
                       ;; For this to happen, there must not be any strong GC reference to the Gambit port object from
                       ;; Sack directly or indirectly such as from this module.
                       ;;
                       ;; It would make reasonable sense that the connection closure through some path has a [strong GC]
                       ;; reference to the Gambit port.
                       ;;
                       ;; Sack's will object bound to the Gambit port however has a strong reference to the connection
                       ;; closure for the (on-connection monitoring-closure #t) call, so Sack ensures that this object
                       ;; has the proper lifetime as is already.
                       ;;
                       ;; Thus: The connection closure is kept alive as long as needed by sack, while if we'd make a
                       ;; strong reference to it here we could break things.
                       ;;
                       ;; Thus, we keep a weak reference only to the connection closure here.
                       weak-keys: #t))
  ;; We maintain this variable only as a means to optimize away the need for a couple of |table-length| calls.
  (define connections-count 0)
  ;; Total number of connections handled until now
  (define connections-handled 0)
  (set! debug-name (if debug-name (string-append debug-name " ") ""))
  (list
   ;; accept-block:
   (lambda ()
     ;; This procedure is to produce a blocking state as long as the number of connections is >= max-concurrent-connections .
     (define (process-a-disconnected-connection timeout)
       (let ((disconnected-connection (thread-receive timeout #f)))
         (if disconnected-connection
             (begin
               (if (eq? 4 overload-protection-verbose?)
                   (overload-protection-dbg debug-name "on-connect: Registering disconnect of " disconnected-connection "."))
               (table-set! connections disconnected-connection)
               (-1! connections-count)
               #t)
             #f)))
     (let ((had-induce-block!? induce-block!?))
       (if had-induce-block!? (overload-protection-dbg-force "Block induced. Blocking till induced block is released..."))
       ;; Each disconnect provides us with a message. To not overload with messages, get every new message, on each accept-block:
       (let drain-messages-loop ()
         (if induce-block!? (thread-sleep! 1))
         (if (or (process-a-disconnected-connection 0) induce-block!?)
             (drain-messages-loop)))
       (if had-induce-block!? (overload-protection-dbg-force "Induced block released, continuing.")))
     ;; Then, as long as even so all slots are occupied, continue draining messages:
     (let wait-for-an-available-slot-loop ()
       (define wait-for-disconnect? (<= max-concurrent-connections connections-count))
       (if (memq overload-protection-verbose? '(2 3 4))
           (overload-protection-dbg debug-name "accept-block: max-concurrent-connections " max-concurrent-connections " connections-count " connections-count
                                    " => wait-for-disconnect? " wait-for-disconnect?))
       (if wait-for-disconnect?
           (begin
             (overload-protection-dbg "Waiting for a slot to become available.")
             (process-a-disconnected-connection +inf.0)
             (wait-for-an-available-slot-loop)))))
   ;; on-connection:
   (lambda (connection is-connect?)
     ;; We get only one call for connect and only one call for disconnect.
     ;;
     ;; The connect call is always made in the Sack thread.
     ;;
     ;; A disconnect calls are always preceded by a connect call, so we know the first call we get is a connect call.
     (if is-connect?
         ;; Connect.
         ;;
         ;; We need no particular logics for accept-block to get up to date about the connections-count update
         ;; performed by this code block, as it's made synchronously with accept-block calls.
         ;;
         ;; For this reason, we can also operate on connections and connections-count completely securely.
         (begin
           (if (not sack-thread) (set! sack-thread (current-thread)))
           (if (eq? 4 overload-protection-verbose?)
               (overload-protection-dbg debug-name "on-connect: Registering connect of " connection "."))
           (table-set! connections connection #t)
           (+1! connections-count)
           (+1! connections-handled))
         ;; Disconnect.
         ;;
         ;; We do need special logics to get accept-block updated about the disconnect, because the disconnect
         ;; is made asynchronously with accept-block's execution.
         ;;
         ;; To be completely safe we should also not perform connections or connections-count mutations here.
         (begin
           (if (memq overload-protection-verbose? '(3 4))
               (overload-protection-dbg debug-name "on-connect: Reporting disconnect for " connection "."))
           (thread-send sack-thread connection))))
   ;; debug:
   ;; Provides a dump of the overload protection mechanism's inner state, for debug purposes.
   (lambda* ((command #f))
       (case command
         ((connections-handled) (lambda () connections-handled))
         ((connections-count  ) (lambda () connections-count  ))
         (else
          (list connections: connections
                connections-count: connections-count
                induce-block!: (lambda (induce-block!?*)
                                 (set! induce-block!? induce-block!?*))))))))

(define* (overload-protection:add-connections-handled-to-gc-report! overload-protection
                                                                    (label " http-c.:s; "))
  (define console-output-port (current-output-port))
  (define get-connections-handled ((list-ref overload-protection 2) 'connections-handled))
  (define get-connections-count   ((list-ref overload-protection 2) 'connections-count  ))
  (##add-gc-interrupt-job! (lambda ()
                             (print port: console-output-port
                                    (get-connections-count) "(" (get-connections-handled) ")" label)
                             (force-output console-output-port 1))))

