;;!!! Cookie handling module for the Sack web server middleware
;; .author Per Eckerdal, 2008-2009
;; .author Mikael More, 2012-2013
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (C) 2008-2009 Per Eckerdal
;; Copyright (C) 2012-2013 Mikael More
;;
;;
;; ## Typical usecase
;; Wrap your sack-app procedure with the |cookies->| procedure e.g.
;;
;;      (sack-start! (cookies-> (lambda (env) ..your code)))
;;
;; Then in your sack app, you set, get and remove cookies like this:
;;
;;      ; Set cookie string value:
;;      ((env 'sack:cookie:set-value!) "my key" "my value" expires: 'in-a-year path: "/")
;;
;;      ; Get cookie string value:
;;      ; (If cookie is removed, expired or otherwise nonexistent => #f)
;;      ((env 'sack:cookie:get-value) "my key") => "my value"
;;
;;      ; Delete cookie:
;;      ((env 'sack:cookie:set-value!) "my key" #f path: "/")
;;
;; Remember that to delete a cookie, the same settings need to be used on deletion as were used when setting the cookie (i.e.
;; same domain, path, port, secure and http-only value.)
;;
;; ## Terminology
;; In this module,
;;  * By 'cookie', we mean a cookie object
;;  * By 'cookie value', we mean a convenience
;;
;; ## Exports
;; The key procedure about this module is the cookies-> procedure, which takes a sack app and returns a sack app.
;; It is to be used as an adapter between Sack and your sack app, i.e.
;; (sack-start! (cookies->
;;                (lambda (env)
;;                  ..your code)))
;;
;; Provided are the new env methods:
;;
;;      To access cookie content directly (this is generally what you want to do):
;;
;;      'sack:cookie:get-value  = Takes one argument, being the name of the cookie whose string value to return. If cookie not found, returns #f.
;;                                Also takes an optional argument, being the default value to return in case no cookie with the specified name is set.
;;      'sack:cookie:set-value! = Takes the same arguments as |make-cookie|, being a cookie which to include in the result back to the client:
;;                                (name value #!key expires domain path port secure http-only) , where
;;                                value = string = Set cookie value to this string
;;                                        #f = Remove cookie. (This is done by internally setting value to an empty string and expires to 'delete)
;;                                expires = either of:
;;                                          ommitted i.e. #f = Keep this cookie for the lifetime of the web browser session onl
;;                                          'in-a-year       = Keep this cookie for a year starting now
;;                                          'in-a-week       = Keep this cookie for a month starting now
;;                                          'delete          = Set expiry time to a time in the past, so that web browser instantly removes the cookie.
;;                                          string           = A SRFI 19 date object or RFC1123-formatted date string
;;                                          integer          = Keep this cookie for this number of secnods counting from now
;;                                domain = Scope of set cookie as regards domain, either of:
;;                                         #f = Same domain as HTTP request is done to, and any of its subdomains
;;                                         "domain.com" = domain.com and all its subdomains, see
;;                                                        http://stackoverflow.com/questions/1062963/how-do-browser-cookie-domains-work and
;;                                                        http://stackoverflow.com/questions/5258126/domain-set-cookie-for-subdomain for more info.
;;                                path = Scope of set cookie as regards path, either of:
;;                                       #f = Any page that starts with the same path as the path the HTTP request is done to
;;                                       "/" etc. = Any page that starts with the given path.
;;                                port = #f = Cookie regards any web server port
;;                                       integer, string, list of integer = Cookie is scoped to only the mentioned web server port/-s
;;                                secure = boolean, #t = Send this cookie only on HTTPS (encrypted) requests.
;;                                         (If setting this one, of course ensure that it is done on a HTTP connection that is encrypted already,
;;                                         otherwise the cookie content could be picked up by a third party, which ought to be what you don't want
;;                                         as you set this attribute.)
;;                                http-only = boolean, #t = Include this cookie in HTTP and HTTPS requests only, and do not allow access to it by
;;                                            other means such as the Javascript document.cookie variable.
;;
;;                                This is unless value is #f, in which case the cookie is removed.
;;
;;      To access cookies at the level of the cookie structure datatype:
;;
;;      'sack:cookie:get-all    = Takes no arguments, returns all cookies in a list.
;;      'sack:cookie:get        = Takes one argument, being the name of the cookie to return. If not found, returns #f.
;;      'sack:cookie:set!       = Takes one argument, being the cookie object which to include in the result back to the client.
;;
;; There is also a variant of |cookies->| by the name |cookies->/notlazy| whose sourcecode is simpler to read than |cookies->| for
;; study purposes, and which should provide higher execution speed if every HTTP request made performs both cookie get:s and set:s.
;;
;; # Internals exported for testing/debugging
;; The procedure cookie-parse-to-list is the one used internally to split out a "Cookie:" header value. Example:
;; (cookie-parse-to-list "key1=value1; key2=value2")
;; => (#<cookie #1 name: "key2" value: "value2" expires: #f domain: #f path: #f port: #f secure: #f http-only: #f>
;;     #<cookie #2 name: "key1" value: "value1" expires: #f domain: #f path: #f port: #f secure: #f http-only: #f>)
;;
;; The procedure cookie-to-http is the one used internally to serialize a cookie object to a "Set-Cookie:" header value. Example:
;; (cookie-to-http (make-cookie "key1" "value1" expires: 'in-a-year)) => "key1=value1; expires=Mon, 03 Mar 2014 12:19:49 GMT; Version=1"
;;
;; ## Performance
;; This module has now been implemented for maximum performace. Basically |cookies->| is optimized for general use scenarios which is really pretty
;; much any out there, while |cookies->/notlazy| would probably be marginally faster for Sack apps that receive and set cookies on every invocation.
;;
;; The absolutely highest performance would probably be gotten if this module would be compiled together with the beneficiary application's
;; code as well as with the sack server's code (with this module and sack having (declare (block)), which they have), so Gambit could
;; do smart inlining of the code used, and alike.
;;
;; ## History
;; 2012-10-28: Updated the use of the procedure string-split to its new name string-split-char .
;; 2012-11-04: Improved performance characteristics for this module by making all object allocations in |cookies->| and |env| accesses lazy,
;;             only done when needed (on cookie read and cookie set only), so the performance overhead for sack app invocations that don't
;;             use cookie functionality at all should be negligibly small. This way this cookies module can be used satisfactorily for uses
;;             where most Sack app invocations are for content that do not use cookies, such as delivery of static content.
;;                  The old implementation is preserved under the name |cookies->/notlazy|, as a functionally equivalent, easier to read
;;             implementation example for anyone wanting to study this code, and for use for any sack app that performs both cookie gets
;;             and sets on every sack app invocations, in which case it provides a mariginally better performance.
;; 2013-03-05: Added ability to remove cookie. Improved documentation.
;; 2013-07-20: Added default value as optional second argument to 'sack:cookie:get-value .
;;
;; ## TODO
;;  * Supports UTF-8? Should it??
;;  * Is our use of urlencoding consistent?? If x-www-form-urlencoded is not needed as an import, remove.

(declare (block)
         (standard-bindings)
         (extended-bindings))


(define (find pred lst)
  (let recur ((lst lst))
    (if (null? lst)
        #f
        (let ((head (car lst)))
          (if (pred head)
              head
              (recur (cdr lst)))))))


(define-type cookie
             id: D00E6EE6-5E55-47F2-B901-4DECBB3AA011
             constructor: make-cookie/no-check
             (name read-only:) ;; string. Must be a valid cookie name as defined in the spec
             value             ;; string
             expires           ;; date, as in SRFI 19, or #f
             domain            ;; string or #f
             path              ;; string or #f
             port              ;; string or #f (?)
             secure            ;; boolean
             http-only         ;; boolean
             )

(define http-separators
  (let ((lst '()))
    (string-for-each (lambda (x)
                       (set! lst (cons x lst)))
                     "()<>@,;:\\\"/[]?={} \t")
    lst))

;; One optimization might be to simply not call this function
(define (valid-cookie-name? name)
  (let ((r #t))
    (string-for-each
     (lambda (chr)
       (let ((int (char->integer chr)))
         (if (or (not (<= 32 int 126))
                 (find (lambda (x) (eq? x chr))
                       http-separators))
             (set! r #f))))
     name)
    r))

(define* (make-cookie name value
                      (expires: #f)
                      (domain: #f)
                      (path: #f)
                      (port: #f)
                      (secure: #f)
                      (http-only: #f)
                      ;; incorrect-ok? = boolean regulating whether we should
                      ;; check that cookie name etc. are correct. We allow
                      ;; incorrect input from the HTTP request, but we do not introduce any
                      ;; incorrectnesses - it's to allow such input that this argument is provided.
                      (incorrect-ok?: #f))
  (if (and (not incorrect-ok?) (not (valid-cookie-name? name)))
      (error "Invalid cookie name:" name))
  (let* ((expires (if value expires 'delete))
         (expires (case expires
                    ((in-a-year) (time-utc->date (add-duration (current-time*) (make-time time-duration 0 (* 365 86400))))) ; (86400 * 365 uses to be considered the max reasonable expiry time for a cookie.)
                    ((in-a-week) (time-utc->date (add-duration (current-time*) (make-time time-duration 0 (* 7   86400)))))
                    ((delete)    "Thu, 01-Jan-1970 00:00:01 GMT")
                    (else
                     (if (integer? expires)
                         (time-utc->date (add-duration (current-time*) (make-time time-duration 0 expires)))
                         expires
                         ))))
         (value (or value "")))
    (make-cookie/no-check name value expires domain path port secure http-only)))

(define (cookie-to-http c)
  (let ((name (cookie-name c))
        (value (cookie-value c))
        (expires (cookie-expires c))
        (domain (cookie-domain c))
        (path (cookie-path c))
        (port (cookie-port c))
        (secure (cookie-secure c))
        (http-only (cookie-http-only c)))
    (define (join between args)
      (cond ((null? args) '())
            ((null? (cdr args)) (list (car args)))
            (else `(,(car args) ,between ,@(join between (cdr args))))))
    (apply string-append
           `(,name "=" ,value
                   ,@(if expires
                         `("; expires=" ,(if (string? expires) expires (date->rfc1123 expires)))
                         '())
                   ,@(if domain
                         `("; domain=" ,domain)
                         '())
                   ,@(if path
                         `("; path=" ,path)
                         '())
                   ,@(if port
                         `("; port="
                           ,@(cond
                              ((number? port)
                               `("\"" ,(number->string port) "\""))
                              ((pair? port)
                               `("\""
                                 ,@(join "," (map number->string port))
                                 "\""))
                              (else `(,port)))))
                   ,(if secure    "; secure"   "")
                   ,(if http-only "; HttpOnly" "")
                   "; Version=1"))))

;; Takes the raw Cookie: field data and splits it into a list
;; of key/value pairs.
(define (cookie-parse-split data)
  (let ((cookies (make-table)))
    (map (lambda (s)
           (let ((sp (string-split-char #\= s)))
             (if (or (null? sp)
                     (null? (cdr sp)))
                 (cons "" "")
                 (cons (urldecode (car sp))
                       (urldecode (cadr sp))))))
         (map string-strip (string-split-char #\; data)))))

(define (cookie-parse-to-list data)
  (let ((ps (cookie-parse-split data))
        (current-cookie #f)
        (default-prefs '()) ;; The special attributes specified before
        ;; any other cookie
        (cookies '())) ;; List of processed cookies
    (define (set-pref name val)
      (let ((name (string-downcase name)))
        (if current-cookie
            (cond
             ((equal? name "$path")
              (cookie-path-set! current-cookie val))
             ((equal? name "$domain")
              (cookie-domain-set! current-cookie val))
             ((equal? name "$port")
              (cookie-port-set! current-cookie val)))
            (set! default-prefs (cons (cons name val)
                                      default-prefs)))))
    (define (new-cookie name val)
      (if current-cookie
          (set! cookies (cons current-cookie cookies)))
      ;; Going with incorrect-ok? = #t as not to get an exception if we have received with the HTTP request
      ;; a cookie name that does not pass |valid-cookie-name?|, such as a cookie name containing a
      ;; colon.
      ;;
      ;; Ours is the task to look through the finger with standard-unconformities from the requestor's
      ;; side, while ourselves keeping a standards-compliant conduct.
      (set! current-cookie (make-cookie name val incorrect-ok?: #t))
      (for-each (lambda (x) (set-pref (car x) (cdr x)))
                default-prefs))
    (for-each
     (lambda (pair)
       (let ((name (car pair))
             (val (cdr pair)))
         (if (not (= 0 (string-length name)))
             (if (eq? #\$ (string-ref name 0))
                 (set-pref name val)
                 (new-cookie name val)))))
     ps)
    (if current-cookie
        (cons current-cookie cookies)
        '())))

;; Parses the value of a Cookie: header and returns it as a
;; table with the cookies, where the keys are the cookie names.
(define (cookie-parse data)
  (let ((tbl (make-table)))
    (for-each (lambda (c)
                (table-set! tbl (cookie-name c) c))
              (cookie-parse-to-list data))
    tbl))

(define (cookie-headers tbl port)
  (table-for-each
   (lambda (key val)
     (display-crlf port "Set-Cookie: " (cookie-to-http val)))
   tbl))

;; Not needed, as we have the 1970 timestamp set on value = #f or/and expires = 'delete .
;; (define date-in-the-past (make-date 0 0 0 0 1 1 1990 0))
                                        ;
;; This is the unoptimized version of |cookies->|. If you have a sack app that reads
;; and sets cookies on every invocation, this implementation is mariginally faster.
(define (cookies->/notlazy app)
  (lambda (env)
    (let* ((cookies
            (let ((cookie-tbl (make-table)))
              (for-each
               (lambda (cookie-header)
                 (for-each (lambda (cookie)
                             (table-set! cookie-tbl
                                         (cookie-name cookie)
                                         cookie))
                           (cookie-parse-to-list cookie-header)))
               ((env 'sack:headers) "cookie"))
              cookie-tbl))
           (changes
            (make-table))
           (cookie-get-all
            (lambda ()
              (let ((ret '()))
                (table-for-each
                 (lambda (key value)
                   (set! ret (cons value ret)))
                 cookies)
                ret)))
           (cookie-get
            (lambda (name)
              (or (table-ref changes name #f)
                  (table-ref cookies name #f))))
           (cookie-get-value
            (lambda (name #!optional default)
              (let ((cookie (cookie-get name)))
                (or (and cookie (cookie-value cookie))
                    default))))
           (cookie-set!
            (lambda (cookie)
              (let ((name (cookie-name cookie)))
                (table-set! changes name cookie)
                (table-set! cookies name cookie))))
           (cookie-set-value!
            (lambda a
              (cookie-set! (apply make-cookie a)))))
      (call-with-values
          (lambda ()
            (app
             (lambda (arg)
               (case arg
                 ((sack:cookie:get-all   ) cookie-get-all   )
                 ((sack:cookie:get       ) cookie-get       )
                 ((sack:cookie:get-value ) cookie-get-value )
                 ((sack:cookie:set!      ) cookie-set!      )
                 ((sack:cookie:set-value!) cookie-set-value!)
                 (else (env arg))))))
        (lambda (code headers respond)
          (table-for-each
           (lambda (key val)
             (set! headers
                   (cons (cons "Set-Cookie" (cookie-to-http val))
                         headers)))
           changes)
          (values code headers respond))))))

;; For a simpler to read implementation example, read |cookies->/notlazy|. This is the optimized version performing exactly the same thing.
(define (cookies-> app)
  (lambda (env)
    (let* ((get-cookies-table
            (let ((cookies #f))
              (lambda ()
                (or cookies
                    (begin
                      (set! cookies (make-table))
                      (for-each
                       (lambda (cookie-header)
                         (for-each (lambda (cookie)
                                     (table-set! cookies
                                                 (cookie-name cookie)
                                                 cookie))
                                   (cookie-parse-to-list cookie-header)))
                       ((env 'sack:headers) "cookie"))
                      cookies)))))
           (changes-table-internal #f)
           (get-changes-table
            (lambda ()
              (or changes-table-internal
                  (begin
                    (set! changes-table-internal (make-table))
                    changes-table-internal))))
           (cookie-get-all
            (lambda ()
              (let ((ret '()))
                (table-for-each
                 (lambda (key value)
                   (set! ret (cons value ret)))
                 (get-cookies-table))
                ret)))
           (cookie-get
            (lambda (name)
              (or (and changes-table-internal (table-ref changes-table-internal name #f))
                  (table-ref (get-cookies-table) name #f))))
           (cookie-get-value
            (lambda (name #!optional default)
              (let ((cookie (cookie-get name)))
                (or (and cookie (cookie-value cookie))
                    default))))
           (cookie-set!
            (lambda (cookie)
              (let ((name (cookie-name cookie)))
                (table-set! (get-changes-table) name cookie)
                (table-set! (get-cookies-table) name cookie))))
           (cookie-set-value!
            (lambda a
              (cookie-set! (apply make-cookie a))))
           ;; (Assigning a name to this procedure even though it's only used by the |app|
           ;; call below, for possibly increasing clarity in debugging a bit.)
           (env/overlapped (lambda (arg)
                             (case arg
                               ((sack:cookie:get-all   ) cookie-get-all   )
                               ((sack:cookie:get       ) cookie-get       )
                               ((sack:cookie:get-value ) cookie-get-value )
                               ((sack:cookie:set!      ) cookie-set!      )
                               ((sack:cookie:set-value!) cookie-set-value!)
                               (else (env arg)))))
           (app-return-values
            (app env/overlapped)))
      ;; Were any changes made to the cookies during this sack app invocation?
      (if changes-table-internal
          ;; Yes. Add these as Set-Cookie headers in the return values to Sack.
          (let ((cookies->/generate-returnvalue ; (Assigning a name to as to possibly increase clarity in debugging)
                 (lambda (code headers respond)
                   (table-for-each
                    (lambda (key val)
                      (set! headers
                            (cons (cons "Set-Cookie" (cookie-to-http val))
                                  headers)))
                    changes-table-internal)
                   (values code headers respond))))
            (call-with-values (lambda () app-return-values) cookies->/generate-returnvalue))
          ;; No. Return the sack-app's return values verbatim.
          app-return-values))))
