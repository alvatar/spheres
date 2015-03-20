;;!!! Filename extension to MIME content-type lookup table module
;; .author Per Eckerdal, 2008
;; .author Mikael More, 2013
;; .author Alvaro Castro-Castilla, 2015
;;
;; Copyright (C) 2008 Per Eckerdal
;; Copyright (C) 2013 Mikael More
;;
;; ## History
;; 2013-03-04: Added requires-charset-setting? setting to the mime types, so that the module user
;;             can know if a particular file format specified requires a charset setting as to
;;             deliver correctly to all agents.
;;
;;             Changed the name of the default MIME type setting from "default" to #f.

(define mimes
  (list->table ;; test: equal? for string.
   '(          ;; (extension-s mime-type-s requires-charset-setting?)
     (".avi"     "video/x-msvideo"                   #f)
     (".bz2"     "application/x-bzip"                #f)
     (".class"   "application/octet-stream"          #f)
     (".css"     "text/css"                          #t)
     (".dtd"     "text/xml"                          #t)
     (".dvi"     "application/x-dvi"                 #f)
     (".gif"     "image/gif"                         #f)
     (".gz"      "application/x-gzip"                #f)
     (".htm"     "text/html"                         #f)
     (".html"    "text/html"                         #f)
     (".jpeg"    "image/jpeg"                        #f)
     (".jpg"     "image/jpeg"                        #f)
     (".js"      "text/javascript"                   #f)
     (".m3u"     "audio/x-mpegurl"                   #f)
     (".mov"     "video/quicktime"                   #f)
     (".mp3"     "audio/mpeg"                        #f)
     (".mpeg"    "video/mpeg"                        #f)
     (".mpg"     "video/mpeg"                        #f)
     (".ogg"     "application/ogg"                   #f)
     (".pdf"     "application/pdf"                   #f)
     (".png"     "image/png"                         #f)
     (".ps"      "application/postscript"            #f)
     (".qt"      "video/quicktime"                   #f)
     (".sig"     "application/pgp-signature"         #f)
     (".swf"     "application/x-shockwave-flash"     #f)
     (".tar"     "application/x-tar"                 #f)
     (".tar.bz2" "application/x-bzip-compressed-tar" #f)
     (".tar.gz"  "application/x-tgz"                 #f)
     (".tbz"     "application/x-bzip-compressed-tar" #f)
     (".tgz"     "application/x-tgz"                 #f)
     (".torrent" "application/x-bittorrent"          #f)
     (".txt"     "text/plain"                        #t)
     (".wav"     "audio/x-wav"                       #f)
     (".wax"     "audio/x-ms-wax"                    #f)
     (".wma"     "audio/x-ms-wma"                    #f)
     (".wmv"     "video/x-ms-wmv"                    #f)
     (".xml"     "text/xml"                          #t)
     (".zip"     "application/zip"                   #f)
     (#f         "application/octet-stream"          #f))))

;; Gambit has a path-extension so this one should be superfluous, but why does it return "" for "a/.b"?
(define (filename-get-extension filename)
  (let ((v (string-split-at-last-nice #\. filename)))
    (and v (string-append "." (string-downcase (cdr v))))))

(define (filename->mime-type&requires-charset-setting? filename)
  (table-ref mimes (filename-get-extension filename) #f))

(define (filename->mime-type filename)
  (let ((v (filename->mime-type&requires-charset-setting? filename)))
    (and v (car v))))

;; charset = If the mime-type is a text format, then include the name of the charset in the mime type
;;           We pre-suggest utf-8 as that is the charset that's most used today the one that is
;;           the most universal for almost all uses.
(define (filename->mime-content-type filename #!optional (charset "utf-8"))
  (let ((v (filename->mime-type&requires-charset-setting? filename)))
    (and v
         (apply (lambda (mime-type-s requires-charset-setting?)
                  (if (and requires-charset-setting? charset)
                      (string-append mime-type-s "; charset=" charset)
                      mime-type-s))
                v))))
