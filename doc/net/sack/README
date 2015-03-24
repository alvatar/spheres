(define-type cookie
  id: D00E6EE6-5E55-47F2-B901-4DECBB3AA011
  
  (name read-only:) ;; A string. Must be a valid cookie name as defined in the spec
  (value read-only:) ;; A string
  (expires read-only:) ;; A date, as in SRFI 19, or #f
  (domain read-only:) ;; A string or #f
  (path read-only:) ;; A string or #f
  (port read-only:) ;; A string or #f (?)
  (secure read-only:) ;; A boolean
  (http-only read-only:)) ;; A boolean

(define-type uri
  id: 62788556-c247-11d9-9598-00039301ba52

  (scheme read-only:) ;; A string.
  (userinfo read-only:) ;; A string.
  (host read-only:) ;; A string.
  (port read-only:) ;; An integer.
  (path read-only:) ;; A string.
  (query read-only:) ;; An alist of strings to strings.
  (fragment read-only:)) ;; A string.

(lambda (env)
  ;; The content of the request can be found in stdin

  ;; Sack applications must return only once.
  
  ;; Output to stderr is considered as logging, both in this procedure
  ;; and the body producing procedure.
  ;;
  ;; Really?
  
  ;; Behavior for output to stdout in this procedure is undefined.

  ;; I'm trying to avoid any global variables or dynamic environment or
  ;; anything like that. Everything relevant to the request and the
  ;; response should be in the environment and in the return value of
  ;; the sack application.
  
  ;; If there is a value that isn't in the env, it is as if it was #f.
  
  ;; Values that must be present:
  ;; The list '(0 3), representing this version of Sack.
  (env 'sack:version)
  ;; Lowercase string, ie "head". It's a string because we don't want
  ;; to need to know which ones that are accepted, which would be
  ;; required otherwise in order to prevent memory leak attacks.
  (env 'sack:request-method)
  ;; A closure taking a lowercase string as argument (eg
  ;; "content-type"), returning a list of the values for that header
  ;; (possibly '())
  ;;
  ;; It's a closure and not a hash or an a-list to make it possible to
  ;; use several different implementation techniques.
  ;;
  ;; This procedure should also be able to take a procedure as argument,
  ;; in which case it works as an iterator over all the headers.
  (env 'sack:headers)
  ;; A virtual path to the sack application being executed, used for
  ;; self-referencing URLs.
  (env 'sack:root)
  (env 'sack:uri)
  
  ;; #t if the server expects (but does not guarantee!) that the
  ;; application will only be invoked this one time during the life of
  ;; its containing process. Normally, this will only be #t for a
  ;; server based on CGI (or something similar).
  (env 'sack:run-once?)
  ;; #t if the application object may be simultaneously invoked by
  ;; another thread in the same process, #f otherwise.
  (env 'sack:single-thread?)
  ;; #t if an equivalent application object may be simultaneously
  ;; invoked by another process, #f otherwise.
  (env 'sack:single-process?) ; bool, like rack


  ;; Possible extensions: sack:session, sack:cookie

  ;; How to do them, if the environment must be immutable?

  ;; Calling the functions to mutate the environment after the sack
  ;; application has returned results in undefined behaviour.

  (env 'sack:cookie:all) ;; Returns a list of the cookies, including
                         ;; changes made by the app.
  (env 'sack:cookie:get name) ;; Returns a cookie or #f
  (env 'sack:cookie:set! cookie)

  (env 'sack:session:all) ;; An a-list of the session data.
  (env 'sack:session:get name)
  (env 'sack:session:set! name value) ;; Name must be a string
  (env 'sack:session:delete! name)
  (env 'sack:session:destroy!)
  (env 'sack:session:id) ;; Possibly not there
  (env 'sack:session:regenerate-id!) ;; Possibly a no-op
  
  (values 200
          ;; If the headers don't include a Content-Length, and not a
          ;; Transfer-Encoding: chunked header, the content will be
          ;; chunk encoded.
          '(("Content-Type" . "text/html"))
          (lambda ()
            ;; Returns an u8vector that should be outputted. This
            ;; procedure will be called again and again until it
            ;; returns #f.
            )))
