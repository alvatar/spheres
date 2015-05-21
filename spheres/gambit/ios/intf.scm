;;!!! Interface for some iOS classes
;; .author Marc Feeley, 2011-2015

;;;============================================================================

;;; File: "intf.scm"

;;; Copyright (c) 2011-2015 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;; TODO: this dependencies
;; (##namespace ("intf#"))
;;
;; (##include "~~lib/gambit#.scm")
;; (##include "~~lib/_gambit#.scm")
;;
;; (##include "intf#.scm")
;; (##include "url#.scm")

(declare
 (standard-bindings)
 (extended-bindings)
 (block)
 (fixnum)
 ;;(not safe)
)


;;;----------------------------------------------------------------------------

;; Interface with NSDate Class.

(define NSDate      (string->Class "NSDate"))
(define alloc       (string->SEL "alloc"))
(define init        (string->SEL "init"))
(define description (string->SEL "description"))

(define (date)
  (id->string
   (send0 (send0 (send0 NSDate alloc) init) description)))

;;;----------------------------------------------------------------------------

;; Interface with NSBundle Class.

(define NSBundle    (string->Class "NSBundle"))
(define mainBundle  (string->SEL "mainBundle"))
(define objectForInfoDictionaryKey (string->SEL "objectForInfoDictionaryKey:"))

(define (mainBundle-info key)
  (let ((info
         (send1 (send0 NSBundle mainBundle)
                objectForInfoDictionaryKey (string->id key))))
    (and info
         (id->string info))))

(define CFBundleName (mainBundle-info "CFBundleName"))
(define CFBundleDisplayName (mainBundle-info "CFBundleDisplayName"))

;;;----------------------------------------------------------------------------

;; Interface with UIDevice Class.

(define currentDevice-batteryLevel
  (c-lambda () float
    "___result = [[UIDevice currentDevice] batteryLevel];"))

(define currentDevice-batteryMonitoringEnabled
  (c-lambda () bool
    "___result = [UIDevice currentDevice].batteryMonitoringEnabled;"))

(define currentDevice-batteryMonitoringEnabled-set!
  (c-lambda (bool) void
    "[UIDevice currentDevice].batteryMonitoringEnabled = ___arg1;"))

(define currentDevice-multitaskingSupported
  (c-lambda () bool
    "___result = [UIDevice currentDevice].multitaskingSupported;"))

(define currentDevice-model
  (c-lambda () NSString*
    "___result = [[UIDevice currentDevice] model];"))

(define currentDevice-name
  (c-lambda () NSString*
    "___result = [[UIDevice currentDevice] name];"))

(define currentDevice-systemName
  (c-lambda () NSString*
    "___result = [[UIDevice currentDevice] systemName];"))

(define currentDevice-systemVersion
  (c-lambda () NSString*
    "___result = [[UIDevice currentDevice] systemVersion];"))

(define currentDevice-uniqueIdentifier
  (c-lambda () NSString*
    "___result = [[[UIDevice currentDevice] identifierForVendor] UUIDString];"))

(define (device-status)
  (currentDevice-batteryMonitoringEnabled-set! #t)
  (list (currentDevice-batteryLevel)
        (currentDevice-batteryMonitoringEnabled)
        (currentDevice-multitaskingSupported)
        (currentDevice-model)
        (currentDevice-name)
        (currentDevice-systemName)
        (currentDevice-systemVersion)
        (currentDevice-uniqueIdentifier)))

(define (device-model)
  (let ((m (currentDevice-model)))
    (cond ((has-prefix? m "iPhone")
           'iPhone)
          ((has-prefix? m "iPod touch")
           'iPod-touch)
          ((has-prefix? m "iPad")
           'iPad)
          (else
           #f))))

(define (UDID)
  (currentDevice-uniqueIdentifier))


;;;----------------------------------------------------------------------------

;; Interface with AudioToolbox.

(c-declare #<<c-declare-end

#import <AudioToolbox/AudioToolbox.h>

c-declare-end
)

(c-define-type SystemSoundID unsigned-int32)

(define AudioServicesPlayAlertSound
  (c-lambda (SystemSoundID) void "AudioServicesPlayAlertSound"))

(define AudioServicesPlaySystemSound
  (c-lambda (SystemSoundID) void "AudioServicesPlaySystemSound"))

(define kSystemSoundID_FlashScreen        #x00000FFE)
(define kSystemSoundID_Vibrate            #x00000FFF)
(define kSystemSoundID_UserPreferredAlert #x00001000)

;;;----------------------------------------------------------------------------

;; Interface with ViewController.

(c-declare #<<c-declare-end

#include "ViewController.h"

c-declare-end
)

;; C functions callable from Scheme.

(define (set-ext-keys
         view
         portrait-small
         #!optional
         (landscape-small portrait-small)
         (portrait-large portrait-small)
         (landscape-large portrait-large))
  ((c-lambda (int NSString* NSString* NSString* NSString*) void "set_ext_keys")
   view
   portrait-small
   landscape-small
   portrait-large
   landscape-large))

(define set-navigation
  (c-lambda (int) void "set_navigation"))

(define show-cancelButton
  (c-lambda () void "show_cancelButton"))

(define hide-cancelButton
  (c-lambda () void "hide_cancelButton"))

(define (show-webView view #!optional (kbd-enabled #f)  (kbd-should-shrink-view #f))
  ((c-lambda (int bool bool) void "show_webView")
   view
   kbd-enabled
   kbd-should-shrink-view))

(define (show-textView view #!optional (kbd-enabled #f)  (kbd-should-shrink-view #f))
  ((c-lambda (int bool bool) void "show_textView")
   view
   kbd-enabled
   kbd-should-shrink-view))

(define (show-imageView view #!optional (kbd-enabled #f)  (kbd-should-shrink-view #f))
  ((c-lambda (int bool bool) void "show_imageView")
   view
   kbd-enabled
   kbd-should-shrink-view))

(define show-currentView
  (c-lambda () void "show_currentView"))

(define set-textView-font
  (c-lambda (int NSString* int) void "set_textView_font"))

(define set-textView-content
  (c-lambda (int NSString*) void "set_textView_content"))

(define get-textView-content
  (c-lambda (int) NSString* "get_textView_content"))

(define add-output-to-textView
  (c-lambda (int NSString*) void "add_output_to_textView"))

(define add-text-input-to-textView
  (c-lambda (int NSString*) void "add_text_input_to_textView"))

(define add-key-input-to-textView
  (c-lambda (int NSString*) void "add_key_input_to_textView"))

(define (set-webView-content view str #!optional (base-url-path #f) (enable-scaling #f) (mime-type "text/html"))
  ((c-lambda (int NSString* NSString* bool NSString*) void "set_webView_content") view str base-url-path enable-scaling mime-type))

(define (set-webView-content-from-file view path #!optional (base-url-path (path-directory path)) (enable-scaling #f) (mime-type "text/html"))
  ((c-lambda (int NSString* NSString* bool NSString*) void "set_webView_content_from_file") view path base-url-path enable-scaling mime-type))

(define add-text-input-to-webView
  (c-lambda (int NSString*) void "add_text_input_to_webView"))

(define add-key-input-to-webView
  (c-lambda (int NSString*) void "add_key_input_to_webView"))

(define add-text-input-to-currentView
  (c-lambda (NSString*) void "add_text_input_to_currentView"))

(define add-key-input-to-currentView
  (c-lambda (NSString*) void "add_key_input_to_currentView"))

(define eval-js-in-webView
  (c-lambda (int NSString*) NSString* "eval_js_in_webView"))

(define open-URL
  (c-lambda (NSString*) void "open_URL"))

(define send-SMS
  (c-lambda (NSString* NSString*) bool "send_SMS"))

(define pick-image
  (c-lambda () bool "pick_image"))

(define set-idle-timer
  (c-lambda (bool) void "set_idle_timer"))

(define set-toolbar-alpha
  (c-lambda (double) void "set_toolbar_alpha"))

(define show-toolbar
  (c-lambda () void "show_toolbar"))

(define hide-toolbar
  (c-lambda () void "hide_toolbar"))

(define toggle-toolbar
  (c-lambda () void "toggle_toolbar"))

(define segm-ctrl-set-title
  (c-lambda (int NSString*) void "segm_ctrl_set_title"))

(define segm-ctrl-insert
  (c-lambda (int NSString*) void "segm_ctrl_insert"))

(define segm-ctrl-remove
  (c-lambda (int) void "segm_ctrl_remove"))

(define segm-ctrl-remove-all
  (c-lambda () void "segm_ctrl_remove_all"))

(define set-pref
  (c-lambda (NSString* NSString*) void "set_pref"))

(define get-pref
  (c-lambda (NSString*) NSString* "get_pref"))

(define set-pasteboard
  (c-lambda (NSString*) void "set_pasteboard"))

(define get-pasteboard
  (c-lambda () NSString* "get_pasteboard"))

(define get-documents-dir
  (c-lambda () NSString* "get_documents_dir"))

(define request-icloud-container-dir
  (c-lambda () void "request_icloud_container_dir"))

(define popup-alert
  (c-lambda (NSString* NSString* NSString* NSString*) void "popup_alert"))

(define (setup-location-updates desired-accuracy #!optional (distance-filter 0.0))
  ((c-lambda (double double) void "setup_location_updates") desired-accuracy distance-filter))

(define (set-navigation-bar titles)
  (segm-ctrl-remove-all)
  (let loop ((i 0) (lst titles))
    (if (pair? lst)
        (begin
          (segm-ctrl-insert i (car lst))
          (loop (+ i 1) (cdr lst))))))

;; Scheme functions callable from C.

(c-define (send-input str) (NSString*) void "send_input" "extern"

  (let ((rp repl-port))
    (if (port? rp)
        (begin
          (display str rp)
          (force-output rp)))))

(c-define (send-event str) (NSString*) void "send_event" "extern"

  (let ((ep event-port))
    (if (port? ep)
        (begin
          (write str ep)
          (force-output ep)))))

(c-define (send-text-input input) (NSString*) void "send_text_input" "extern"

  (let ((ht handle-text-input))
    (if (procedure? ht)
        (ht input))))

(define handle-text-input #f)

(set! handle-text-input
  (lambda (input)
    (add-text-input-to-currentView input)))

(c-define (send-key-input input) (NSString*) void "send_key_input" "extern"

  (let ((hk handle-key-input))
    (if (procedure? hk)
        (hk input))))

(define handle-key-input #f)

(set! handle-key-input
  (lambda (input)
    (add-key-input-to-currentView input)))

(c-define (heartbeat) () double "heartbeat" "extern"

  ;; make sure other threads get to run
  (##thread-heartbeat!)

  ;; check if there has been any REPL output
  (let ((rp repl-port))
    (if (port? rp)
        (let ((output (read-line rp #f)))
          (if (string? output)
              (add-output-to-textView 0 output)))))

  ;; return interval until next heartbeat
  (next-heartbeat-interval))

(define (next-heartbeat-interval)

  (##declare (not interrupts-enabled))

  (let* ((run-queue
          (macro-run-queue))
         (runnable-threads?
          (##not
           (let ((root (macro-btq-left run-queue)))
             (and (##not (##eq? root run-queue))
                  (##eq? (macro-btq-left root) run-queue)
                  (##eq? (macro-btq-right root) run-queue))))))
    (if runnable-threads?

        (begin
          ;; There are other threads that can run, so request
          ;; to call "heartbeat" real soon to run those threads.
          interval-runnable)

        (let* ((next-sleeper
                (macro-toq-leftmost run-queue))
               (sleep-interval
                (if (##eq? next-sleeper run-queue)
                    +inf.0
                    (begin
                      ;; There is a sleeping thread, so figure out in
                      ;; how much time it needs to wake up.
                      (##flonum.max
                       (##flonum.- (macro-thread-timeout next-sleeper)
                                   (##current-time-point))
                       interval-min-wait))))
               (next-condvar
                (macro-btq-deq-next run-queue))
               (io-interval
                (if (##eq? next-condvar run-queue)
                    interval-no-io-pending ;; I/O is not pending, just relax
                    interval-io-pending))) ;; I/O is pending, so come back soon
          (##flonum.min sleep-interval io-interval)))))

(define interval-runnable 0.0)
(set! interval-runnable 0.0)

(define interval-io-pending 0.0)
(set! interval-io-pending 0.02)

(define interval-no-io-pending 0.0)
(set! interval-no-io-pending 1.0)

(define interval-min-wait 0.0)
(set! interval-min-wait 0.0001)

(c-define (eval-string str) (NSString*) NSString* "eval_string" "extern"
  (let ()

    (define (catch-all-errors thunk)
      (with-exception-catcher
       (lambda (exc)
         (write-to-string exc))
       thunk))

    (define (write-to-string obj)
      (with-output-to-string
        ""
        (lambda () (write obj))))

    (define (read-from-string str)
      (with-input-from-string str read))

    (catch-all-errors
     (lambda () (write-to-string (eval (read-from-string str)))))))

;;;----------------------------------------------------------------------------

;; Setup pipe to do I/O on the REPL being run by the primordial thread.

(define repl-port #f)

(receive (i o) (open-string-pipe)

  ;; Hack... set the names of the port.
  (##vector-set! i 4 (lambda (port) '(console)))

  (set! ##stdio/console-repl-channel (##make-repl-channel-ports i i))

  (set! repl-port o)

  (input-port-timeout-set! o -inf.0))

;;;----------------------------------------------------------------------------

;; Handling of events from the webView.

(define event-port #f)

(define event-handler
  (lambda (event)
    ;; ignore event
    #f))

(define location-update-event-handler
  (lambda (event)
    ;; ignore event
    #f))

(receive (i o) (open-vector-pipe '(direction: input))

  (set! event-port o)

  (thread-start!
   (make-thread
    (lambda ()
      (let loop ()
        (let ((event (read i)))
          (if (not (eof-object? event))
              (let ((x (has-prefix? event "location-update:")))
                (if x
                    (let ((location
                           (with-exception-catcher
                            (lambda (e)
                              #f)
                            (lambda ()
                              (list->vector (with-input-from-string x read-all))))))
                      (location-update-event-handler location))
                    (event-handler event))
                (loop)))))))))

(define (set-event-handler proc)
  (set! event-handler (proc event-handler)))

(define (set-location-update-event-handler proc)
  (set! location-update-event-handler proc))

(define (show-view view #!optional (kbd-enabled #f) (kbd-should-shrink-view #f))
  (show-webView view kbd-enabled kbd-should-shrink-view))

(define (set-view-content view content #!optional (base-url-path #f) (enable-scaling #f) (mime-type "text/html"))
  (set-webView-content
   view
   (with-output-to-string "" (lambda () (print content)))
   base-url-path
   enable-scaling
   mime-type))

(define (has-prefix? str prefix)
  (and (string? str)
       (string? prefix)
       (let ((len-str (string-length str))
             (len-prefix (string-length prefix)))
         (and (>= len-str len-prefix)
              (string=? (substring str 0 len-prefix) prefix)
              (substring str len-prefix len-str)))))

(define (get-event-parameters rest)
  (call-with-input-string
   rest
   (lambda (port)
     (map url-decode
          (read-all port (lambda (p) (read-line p #\:)))))))


;;;----------------------------------------------------------------------------

;; Make it impossible to quit the application with a call to "exit" or
;; with a ",q" from the REPL.  This is needed to conform to the iOS
;; Developer Program License Agreement (I don't know which section
;; but I remember it had to do with the iOS human interface design).

(set! ##exit
      (lambda (#!optional (status 0))
        (error "To exit, press the sleep button for 5 seconds then the home button for 10 seconds")))


;;;----------------------------------------------------------------------------

;; Make it impossible to access files outside of Gambit REPL.  This is
;; needed to conform to the iOS Developer Program License Agreement:
;; 
;; 3.3.4 An Application may only read data from or write data to an
;; Application's designated container area on the device, except as
;; otherwise specified by Apple.

;; "/" will be equal to the app's iCloud Documents directory (if available).
;; "~/" will be equal to the app's Documents directory.
;; "~~/" will be equal to the app's bundle directory.

(define app-icloud-dir
  (string-append "/private/var/mobile/Library/Mobile Documents/iCloud~"
                 CFBundleName
                 "/"))

(define app-icloud-documents-dir
  (string-append app-icloud-dir "Documents/"))

(define app-root-dir app-icloud-documents-dir) ;; root of FS (i.e. /) is app's iCloud dir

(define app-documents-dir (##path-normalize (get-documents-dir)))

(define app-bundle-dir (##path-normalize "~~"))

(set! ##os-path-homedir (lambda () app-documents-dir))

(define (contained-path-resolve path)
  (let loop ()
    (let ((xpath (##path-normalize (##path-expand path))))
      (cond ((has-prefix? xpath "~/") =>
             (lambda (rest)
               (##path-expand rest app-documents-dir)))
            ((equal? xpath "~")
             app-documents-dir)
            ((or (has-prefix? xpath app-bundle-dir)
                 (has-prefix? xpath app-documents-dir)
                 (has-prefix? xpath app-icloud-dir))
             xpath) ;; OK if path is in sandbox
            ((has-prefix? xpath "/") =>
             (lambda (rest)
               (##path-expand rest app-root-dir)))
            (else
             (##path-expand xpath app-root-dir))))))

(set! ##path-resolve-hook contained-path-resolve)

(define (contained-path-unresolve path)
  (cond ((has-prefix? path app-bundle-dir) =>
         (lambda (rest)
           (string-append "~~/" rest)))
        ((has-prefix? path app-documents-dir) =>
         (lambda (rest)
           (string-append "~/" rest)))
        ((has-prefix? path app-icloud-documents-dir) =>
         (lambda (rest)
           rest))
        (else
         path)))

(set! ##path-unresolve-hook contained-path-unresolve)

(set! ##repl-path-normalize-hook (lambda (path) path))

(define app-icloud-container-dir #f)

(define (iCloudAccountAvailabilityChanged)
  (request-icloud-container-dir) ;; check iCloud availability
  #f)

(define (iCloudContainerDirChanged dir)
  (set! app-icloud-container-dir (if (equal? dir "") #f dir))
  (emacs#setup-iCloudStatus)
  #f)

(iCloudAccountAvailabilityChanged)

;; Make the current-directory equal to the root directory.

(##define-macro (macro-parameter-descr param)
  `(##closure-ref ,param 1))

(macro-parameter-descr-filter-set!
 (macro-parameter-descr ##current-directory)
 (lambda (val)
   (macro-check-string val 1 (##current-directory val)
     (let ((normalized-dir
            (##os-path-normalize-directory
             (##path-normalize (##path-expand val)))))
       (if (##fixnum? normalized-dir)
         (##raise-os-exception #f normalized-dir ##current-directory val)
         (contained-path-unresolve normalized-dir))))))

(##current-directory "/")


;;;============================================================================
