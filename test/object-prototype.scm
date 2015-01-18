(##spheres-load energy: testing)

(##spheres-load prototype)
(##spheres-load strings: format)

(test-begin "Prototype-based object system")





  
;; Helpers
(define (->integer n)
  ;; return integer closest to n
  (inexact->exact (round n)))

(define (every*? pred? . rest)
  (let loop ( (list rest) )
    (cond
     ((null? list) #t)
     ((pred? (car list)) (loop (cdr list)))
     (else #f))))

;==============================================================;
;;; POINT x y

(define-predicate point?)

;; use new-* for objects (vs make-* for records)
(define* (new-point (x: 0) (y: 0))
  ;; 2d, exact-integer values for x,y
  ;; Intent is for pixel position graphics
  (unless (and (integer? x)
               (integer? y))
          (error 'new-point "x and y must be integers" x y))
  (object ([x x] [y y])
          ;;methods
          [(point? self) #t] ;; Yes, this is a point!
          [(name self) 'point]
          [(->string self) (format "(new-point x: ~a y: ~a)" [$ x self] [$ y self])]
          [(add self other)
           (cond
            ((point?  other) (new-point x: (+ [$ x self] [$ x other])
                                        y: (+ [$ y self] [$ y other])))
            ((number? other) (new-point x: (+ [$ x self] other)
                                        y: (+ [$ y self] other)))
            (else (error 'point:add "Can't add self to other" self other)))]
          [(point-between self other)
           (unless (point? other)
                   (error 'point:between
                          "Don't know how to be between point and non-point"
                          self other))
           (new-point x: (->integer (/ (+ [$ x self] [$ x other]) 2))
                      y: (->integer (/ (+ [$ y self] [$ y other]) 2)))]
          [(sub self other)
           (cond
            [(point? other)  (new-point x: (- [$ x self] [$ x other])
                                        y: (- [$ y self] [$ y other]))]
            [(number? other) (new-point x: (- [$ x self] other)
                                        y: (- [$ y self] other))]
            [else (error 'point:sub "Can't add self to other" self other)])]
          [(negate self)
           (new-point x: (- [$ x self]) y: (- [$ y self]))]
          [(scale-by self scale)
           (unless (number? scale)
                   (error 'point:scale-by "Don't know scale point with other" self scale))
           (new-point x: (->integer (* scale [$ x self]))
                      y: (->integer (* scale [$ y self])))]
          [(=? self other)
           (unless (point? other)
                   (error 'point:=?
                          "Don't know how compare point to non-point"
                          self other))
           (and (= [$ x self] [$ x other]) (= [$ y self] [$ y other]))]
          [(<? self other)
           (unless (point? other)
                   (error 'point:<?
                          "Don't know how compare point to non-point"
                          self other))
           (and (< [$ x self] [$ x other]) (< [$ y self] [$ y other]))]
          [(>? self other)
           (unless (point? other)
                   (error 'point:>?
                          "Don't know how compare point to non-point"
                          self other))
           (and (> [$ x self] [$ x other]) (> [$ y self] [$ y other]))]
          [(<=? self other)
           (unless (point? other)
                   (error 'point:<=?
                          "Don't know how compare point to non-point"
                          self other))
           (and (<= [$ x self] [$ x other]) (<= [$ y self] [$ y other]))]
          [(>=? self other)
           (unless (point? other)
                   (error 'point:>=?
                          "Don't know how compare point to non-point"
                          self other))
           (and (>= [$ x self] [$ x other]) (>= [$ y self] [$ y other]))]
          [(<>? self other)
           (unless (point? other)
                   (error 'point:<>?
                          "Don't know how compare point to non-point"
                          self other))
           (or (not (= [$ x self] [$ x other]))
               (not (= [$ y self] [$ y other])))]
          [(min-point self other)
           (unless (point? other)
                   (error 'point:min-point
                          "Requires two points"
                          self other))
           (new-point x: (min [$ x self] [$ x other])
                      y: (min [$ y self] [$ y other]))]
          [(max-point self other)
           (unless (point? other)
                   (error 'point:max-point
                          "Requires two points"
                          self other))
           (new-point x: (max [$ x self] [$ x other])
                      y: (max [$ y self] [$ y other]))]
          [(nearest-point-on-line-between self start end)
           (unless (and (point? start) (point? end))
                   (error 'point:nearest-point-on-line-between
                          "Needs a point and two line end-points"
                          self start end))
           (cond
            [(= [$ x start] [$ x end])
             (new-point x: [$ x start] y: [$ y self])]
            [(= [$ y start] [$ y end])
             (new-point x: [$ x self] y: [$ y start])]
            [else
             (let* ( (x1  [$ x start])
                     (y1  [$ y start])
                     (x21 (- [$ x end] x1))
                     (y21 (- [$ y end] y1))
                     (tao (/ (+ (/ (- [$ y self] y1) x21)
                                (/ (- [$ x self] x1) y21))
                             (+ (/ x21 y21)
                                (/ y21 x21)))))
               (new-point x: (->integer (+ x1 (* tao x21)))
                          y: (->integer (+ y1 (* tao y21)))))])]
          [(distance-between self other)
           (unless (point? other)
                   (error 'point:distance-between
                          "Needs two points"
                          self other))
           (let ([dx (- [$ x self] [$ x other])]
                 [dy (- [$ y self] [$ y other])])
             (sqrt (+ (* dx dx) (* dy dy))))]
          [(as-rectangle self)
           (new-rectangle x: [$ x self] y: [$ y self] width: 0 height: 0)]
          [(rect-from-extent self extent) ; extent = (width,height)
           (unless (point? extent)
                   (error 'point:extent
                          "Needs two points"
                          self extent))
           (new-rectangle x: [$ x self] y: [$ y self] width: [$ x extent] height: [$ y extent])]
          [(rect-from-any-2-points self other)
           (unless (point? other)
                   (error 'point:rect-from-any-2-points
                          "Needs two points"
                          self other))
           [$ rect-from-2-points [$ min-point self other]
              [$ max-point self other]]]
          [(rect-from-2-points self other)
           (new-rectangle x: [$ x self] y: [$ y self]
                          width:  (abs (- [$ x other] [$ x self]))
                          height: (abs (- [$ y other] [$ y self])))]
          [(polar-r self)
           [$ distance-between self (new-point x: 0 y: 0)]]
          [(polar-theta self)
           (atan (+ 0.0 (/ [$ y self] [$ x self])))]))

(define (polar->point r theta)
  (new-point x: (->integer (* r (sin theta)))
             y: (->integer (* r (cos theta)))))

(define (list-of-points->rectangle list-of-points)
  (if (null? list-of-points)
      (new-rectangle x: 0 y: 0 width: 0 height: 0)
      (let loop ([min-pt (car list-of-points)]
                 [max-pt (car list-of-points)]
                 [points (cdr list-of-points)])
        (if (null? points)
            [$ rect-from-2-points min-pt max-pt]
            (let ((pt (car points)))
              (loop [$ min-point min-pt pt]
                    [$ max-point max-pt pt]
                    (cdr points)))))))

;==============================================================;
;;; RECTANGLE x y width height
;;
;; Nota Bene: Screen (0,0) is upper left corner
;;              ..with Y increasing downward.
;;  o-->X
;;  |
;;  v   (x1,y1) => upper-left corner point
;;  Y     *----------*
;;        |          |\
;;        |          | } height
;;        |          |/
;;        *----------* (x2,y2) = lower-right corner
;;           width    extent = (width, height)

(define-predicate rectangle?)

(define* (new-rectangle (x: 0) (y: 0) (width: 0) (height: 0))
  (unless (every*? (lambda (n) (and (integer? n) (>= n 0)))
                   x y width height)
          (error 'new-rectangle
                 "parameters must be non-negative integers"
                 x y width height))
  (let ([ul-point (new-point x: x y: y)])
    (object ( [width width] [height height])
            [(rectangle? self) #t]
            [(delegate self) ul-point]
            [(name self) 'rectangle]
            [(->string self)
             (format "(new-rectangle x: ~a y: ~a width: ~a height: ~a)"
                     [$ x self]     [$ y self]
                     [$ width self] [$ height self])]
            [(=? self other)
             (if (rectangle? other)
                 (and (= [$ x self] [$ x other])
                      (= [$ y self] [$ y other])
                      (= [$ width  self] [$ width  other])
                      (= [$ height self] [$ height other]))
                 (error 'rectangle:=?
                        "Can only compare two rectangles"
                        self other))]
            [(extent self) (new-point x: [$ width self] y: [$ height self])]
            [(max-x self) (+ [$ x self] [$ width  self])]
            [(max-y self) (+ [$ y self] [$ height self])]
            [(with-width self width)
             (new-rectangle x: [$ x self] y: [$ y self]
                            width: [->integer width] height: [$ height self])]
            [(with-height self height)
             (new-rectangle x: [$ x self] y: [$ y self]
                            width: [$ width self] height: (->integer height))]
            [(with-x self x)
             (new-rectangle x: (->integer x) y: [$ y self]
                            width: [$ width self] height: [$ height self])]
            [(with-y self y)
             (new-rectangle x: [$ x self] y: (->integer y)
                            width: [$ width self] height: [$ height self])]
            [(with-extent self extent-pt)
             (new-rectangle x: [$ x self] y: [$ y self]
                            width: [$ x extent-pt] height: [$ y extent-pt])]
            ;; Control Handle Support
            [(center-point self)
             (new-point x: (->integer (+ [$ x self] (/ [$ width  self] 2)))
                        y: (->integer (+ [$ y self] (/ [$ height self] 2))))]
            [(top-left-point self)
             [$ delegate self]] ;; @@?Copy to avoid side effects?@@
            [(top-right-point self)
             (new-point x: (+ [$ x self] [$ width self]) y: [$ y self])]
            [(bottom-left-point self)
             (new-point x: [$ x self] y: (+ [$ y self] [$ height self]))]
            [(bottom-right-point self)
             (new-point x: (+ [$ x self] [$ width  self])
                        y: (+ [$ y self] [$ height self]))]
            [(left-center-point self)
             (new-point x: [$ x self]
                        y: (->integer (+ [$ y self] (/ [$ height self] 2))))]
            [(right-center-point self)
             (new-point x: (+ [$ x self] [$ width self])
                        y: (->integer (+ [$ y self] (/ [$ height self] 2))))]
            [(top-center-point self)
             (new-point x: (->integer (+ [$ x self] (/ [$ width self] 2)))
                        y: [$ y self])]
            [(bottom-center-point self)
             (new-point x: (->integer (+ [$ x self] (/ [$ width self] 2)))
                        y: (+ [$ y self] [$ height self]))]
            [(with-top-left self point)
             [$ rect-from-any-2-points point [$ bottom-right-point self]]]
            [(with-top-right self point)
             [$ rect-from-any-2-points point [$ bottom-left-point self]]]
            [(with-bottom-left self point)
             [$ rect-from-any-2-points point [$ top-right-point self]]]
            [(with-bottom-right self point)
             [$ rect-from-any-2-points point [$ top-left-point self]]]
            [(with-left-center self point)
             (new-rectangle x: [$ x point]
                            y: [$ y self]
                            width:  (+ [$ width self] (- [$ x self] [$ x point]))
                            height: [$ height self])]
            [(with-right-center self point)
             (new-rectangle x: [$ x point]
                            y: [$ y self]
                            width:  (- [$ x point] [$ x self])
                            height: [$ height self])]
            [(with-bottom-center self point)
             (new-rectangle x: [$ x self]
                            y: [$ y self]
                            width:  [$ width self]
                            height: (- [$ y point] [$ y self]))]
            [(with-top-center self point)
             (new-rectangle x: [$ x self]
                            y: [$ y point]
                            width:  [$ width self]
                            height: (+ [$ height self]
                                       (- [$ y self] [$ y point])))]
            ;; Containment
            [(contains-point? self point)
             (and (<= [$ x self] [$ x point] (+ [$ x self] [$ width  self]))
                  (<= [$ y self] [$ y point] (+ [$ y self] [$ height self])))]
            [(contains-rect? self rectangle) ;; STRICT [full] containment
             (and (<= [$ x self] [$ x rectangle])
                  (<= [$ y self] [$ y rectangle])
                  (<= [$ max-x rectangle] [$ max-x self])
                  (<= [$ max-y rectangle] [$ max-y self]))]
            ;; Point must remain within rect
            [(constrain-point self point) 
             (new-point x: (min [$ max-x self] (max [$ x self] [$ x point]))
                        y: (min [$ max-y self] (max [$ y self] [$ y point])))]
            [(intersection self other)
             (unless (rectangle? other)
                     (error 'rectangle:intersection
                            "Interscetion requires two rectangles"
                            self other))
             [$ rect-from-2-points [$ max-point [$ top-left-point self]
                                      [$ top-left-point other]]
                [$ min-point [$ bottom-right-point self]
                   [$ bottom-right-point other]]]]
            [(union self other)
             (unless (rectangle? other)
                     (error 'rectangle:union
                            "Union requires two rectangles"
                            self other))
             [$ rect-from-2-points [$ min-point [$ top-left-point self]
                                      [$ top-left-point other]]
                [$ max-point [$ bottom-right-point self]
                   [$ bottom-right-point other]]]]
            [(empty? self)
             (and (zero? [$ width self]) (zero? [$ height self]))]
            [(distance-between self other)
             (cond
              [(rectangle? other)
               (if (not [$ empty? [$ intersection self other]])
                   0
                   (let*
                       ( [p1 [$ closest-point-to self [$ center-point other]]]
                         [p2 [$ closest-point-to other p1]]
                         )
                     [$ distance-between p1 p2]))]
              [(point? other)
               (if [$ contains-point? self other]
                   0
                   [$ closest-point-to self other])]
              [else
               (error 'rectangle:distance-between
                      "Requires a rectangle and another rectangle or a point"
                      self other)])]
            [(closest-point-to self point)
             (unless (point? point)
                     (error 'rectangle:closest-point-to
                            "requires a rectangle and a point"
                            self point))
             ;; return a perimeter point
             (new-point x: (min (max [$ x self] [$ x point]) [$ max-x self])
                        y: (min (max [$ y self] [$ y point]) [$ max-y self]))
             ]
            [(translate-by self delta)
             (unless (point? delta)
                     (error 'rectangle:translate-by
                            "requires a rectangle and a point"
                            self delta))
             (new-rectangle x: (+ [$ x delta] [$ x self])
                            y: (+ [$ y delta] [$ y self])
                            width:  [$ width  self]
                            height: [$ height self])]
            [(scale-by self scale)
             (cond
              [(number? scale)
               (new-rectangle x: [$ x self] y: [$ y self]
                              width:  (->integer (* scale [$ width  self]))
                              height: (->integer (* scale [$ height self])))]
              [(rectangle? scale)
               ;; relative rectangle, e.g. pane spec in a window
               (new-rectangle x: (+ [$ x self] (* [$ x scale] [$ width  self]))
                              y: (+ [$ y self] (* [$ y scale] [$ height self]))
                              width:  (* [$ width  self] [$ width  scale])
                              height: (* [$ height self] [$ height scale]))]
              ;; NB: a rectangle is also a point, so point comes 2nd
              [(point? scale)
               (new-rectangle x: [$ x self] y: [$ y self]
                              width:  (* [$ x scale] [$ width  self])
                              height: (* [$ y scale] [$ height self]))]
              [else
               (error 'rectangle:scale
                      "Don't know how to scale a rectangle with scale.."
                      self scale)])]
            [(expand-by self expansion)
             [$ inset-by self
                (if (prototype-object? expansion) [$ negate expansion] (- expansion))]]
            [(inset-by self inset)
             (cond
              [(number? inset)
               (new-rectangle x: (->integer (+ inset [$ x self]))
                              y: (->integer (+ inset [$ y self]))
                              width:  (->integer (- [$ width  self] (* inset 2)))
                              height: (->integer (- [$ height self] (* inset 2))))]
              [(point? inset)
               (new-rectangle x: (+ [$ x self] [$ x inset])
                              y: (+ [$ y self] [$ y inset])
                              width:  (- [$ width  self] (* [$ x inset] 2))
                              height: (- [$ height self] (* [$ y inset] 2)))]
              [else
               (error 'rectangle:inset-by
                      "Don't know how to inset rectangle by inset.."
                      self inset)])])))


;==============================================================;
;;; COLORs  red green blue

(define-predicate color?)

(define* (new-color (red: 0) (green: 0) (blue: 0))
  ;; @@FIXME@@ rgb values range from 0 to 1, use 0..255
  (unless (every*? (lambda (c) (and (number? c) (<= 0 c 1)))
                   red green blue)
          (error 'new-color
                 "red, green, and blue values must be in range 0..1"
                 red green blue))
  (object ( [red red] [green green] [blue blue] )
          [(color? self) #t]
          [(->string self)
           (format "(new-color red: ~a green: ~a blue: ~a)"
                   [$ red self] [$ green self] [$ blue self])]
          [(=? self other)
           (unless (color? other)
                   (error 'color:=?
                          "comparison requires two colors"
                          self other))
           (let ( [epsilon 0.0001] ) ;; color values range 0..1
             (and (< (- [$ red   self] [$ red   other]) epsilon)
                  (< (- [$ green self] [$ green other]) epsilon)
                  (< (- [$ blue  self] [$ blue  other]) epsilon)))]
          [(mixed-with self other proportion)
           (unless (and (color? other)
                        (number? proportion)
                        (<= 0 proportion 1))
                   (error 'color:mixed-with
                          "requires two colors and a proportion"
                          self other proportion))
           (let ( [left  proportion]
                  [right (- 1 proportion)])
             (new-color red:   (+ (* [$ red   self] left) (* [$ red   other] right))
                        green: (+ (* [$ green self] left) (* [$ green other] right))
                        blue:  (+ (* [$ blue  self] left) (* [$ blue  other] right))))]
          [(darker self shades)
           (unless (and (integer? shades)
                        (>= shades 0))
                   (error 'color:darker
                          "shades must a positive integer"
                          shades))
           (let ( [black (color-named 'black)] )
             (let loop ( [shades shades] [color [$ mixed-with self black 0.5]] )
               (if (zero? shades)
                   color
                   (loop (- shades 1) [$ mixed-with color black 0.5]))))]
          [(lighter self shades)
           (unless (and (integer? shades)
                        (>= shades 0))
                   (error 'color:lighter
                          "shades must a positive integer"
                          shades))
           (let ( [white (color-named 'white)] )
             (let loop ( [shades shades] [color [$ mixed-with self white 0.5]] )
               (if (zero? shades)
                   color
                   (loop (- shades 1) [$ mixed-with color white 0.5]))))]))

(define (rgb->color r g b) ;; range of r g b is 0..#xFF=255
  (unless (and (integer? r) (integer? g) (integer? b)
               (<= 0 r 255)
               (<= 0 g 255)
               (<= 0 b 255))
    (error 'rgb->color
           "rgb values must be integers in range 0..255"
           r g b))
  (new-color red: (/ r 255) green: (/ g 255) blue: (/ b 255)))

;; HSB a.k.a. HSV
;; http://en.wikipedia.org/wiki/HSL_color_space#Conversion_from_HSV_to_RGB
(define (hsb->color hue saturation brightness)
  (unless (and (integer? hue)
               (number?  saturation)
               (number?  brightness)
               (<= 0 hue        360)
               (<= 0 saturation   1)
               (<= 0 brightness   1))
    (error 'hsb->color
           "hue range 0..360 [degrees]; saturation & brightness range 0..1"
           hue saturation brightness))
  (if (zero? saturation)
      ;; zero saturation yields gray with given brightness
      (rgb->color brightness brightness brightness)
      (let*
          ( (hue/60 (/ (modulo hue 360) 60))
            (hue-index (floor hue/60)) ;; integer part
            (f (- hue/60 hue-index))   ;; fractional part
            (p (* brightness (- 1 saturation)))
            (q (* brightness (- 1 (* f saturation))))
            (t (* brightness (- 1 (* (- 1 f) saturation)))))
        ;;         (display  ;;@@DEBUG@@;;
        ;;          (format "i=~a f=~a p=~a q=~a t=~a b=~a ~%"
        ;;                  hue-index f p q t brightness))
        (case hue-index
          ((0) (new-color red: brightness green: t blue: p))
          ((1) (new-color red: q green: brightness blue: p))
          ((2) (new-color red: p green: brightness blue: t))
          ((3) (new-color red: p green: q blue: brightness))
          ((4) (new-color red: t green: p blue: brightness))
          ((5) (new-color red: brightness green: p blue:  q))
          (else (new-color red: 0 green: 0 blue: 0))))))

(define color-named
  (let ((name-db
          ;;          ...@@CHECK THESE@@
          (list
             (cons 'black      (new-color red:  0 green:  0 blue:   0))
             (cons 'white      (new-color red:  1 green:  1 blue:   1))
             (cons 'gray       (new-color red: .8 green: .8 blue:  .8))
             (cons 'red        (new-color red: .8 green:  0 blue:   0))
             (cons 'green      (new-color red:  0 green: .8 blue:   0))
             (cons 'yellow     (new-color red:  0 green:  0 blue:  .8))
             (cons 'blue       (new-color red:  0 green:  0 blue:  .8))
             (cons 'purple     (rgb->color 131   0 201))
             (cons 'magenta    (rgb->color 204   0 255))
             (cons 'turquoise  (rgb->color   0 240 255))
             (cons 'brown      (rgb->color 182  67   0))
             (cons 'orange     (rgb->color 255 153   0))
             (cons 'lime-green (rgb->color  51 255   0))
             (cons 'cyan       (rgb->color   0 255 255))
             (cons 'pink       (rgb->color 255  30 153))
             (cons 'light-gray (rgb->color 189 190 192))
             (cons 'dark-gray  (new-color  red: .4 green: .4 blue: .4)))))
    (lambda (name)
      (let ( (name-sym (if (string? name) (string->symbol name) name)) )
        (cond
         ((assq name-sym name-db) => cdr)
         (else
          (error 'color-named
                 "unknown color name"
                 name)))))))
  
;==============================================================;
;;; EVENTS

(define-predicate event?)

(define (make-event name . args)
  (object ([name name] [args args])
   [(event? self) #t]))

(define default-handler
  (lambda (evt)
    (error 'event-handler
           "unknown event"
           evt)))

(define event-handlers
  (let ([handler-alist '()])
    (object ()
            [(name self) 'event-handlers]
            [(->string self)
             (format "#<EVENT-HANDLER ~a>" (map car handler-alist))]
            [(add-handler! self event-name new-handler)
             (cond
              [(assq event-name handler-alist)
               => (lambda (bucket)
                    (let ( (old-handler (cdr bucket)) )
                      (set-cdr! bucket new-handler)
                      old-handler))]
              [else
               (set! handler-alist
                     (cons (cons event-name new-handler) handler-alist))
               default-handler])]
            [(handle-event self event)
             (cond
              [(and (event? event)
                    (assq [$ name event] handler-alist))
               => (lambda (bucket) ((cdr bucket) event))]
              [else (default-handler event)])])))


;==============================================================;
;;; TESTS


(define p0 (new-point x:  0 y:   0))
(define p1 (new-point x: 23 y:  14))
(define p2 (new-point x:  7 y: 123))
(define p3 (new-point x: 17 y:   5))
(define p4 (new-point x: 20 y:  14))
(define (=? a b) [$ =? a b])

#;
(test-equal
 "Point 1"
 (with-output-to-string
   '()
   (lambda ()
     ;; Type-class _with_
     (with ((<Shape> point-shape))
           (draw test-point))))
 "Shape (1, 2)\n")

(test-assert "(point? 3)" (not (point? 3)))
(test-assert "(point? p1)" (point? p1))
(test-assert "point:=?" [$ =?  p1 (new-point x: [$ x p1] y: [$ y p1])])
(test-assert "(point:=? p1 p2)" (not [$ =?  p1 p2]))
(test-assert "(point:<>? p1 p2)" [$ <>? p1 p2])
(test-assert "point:<>?" (not [$ <>? p1 (new-point x: [$ x p1] y: [$ y p1])]))

(test-error "[$ =? p1 3]" [$ =? p1 3])

(test-assert "point equality 1" (=? (new-point x:  30 y:  137) [$ add p1 p2]))
(test-assert "point equality 2" (=? (new-point x:  16 y: -109) [$ sub p1 p2]))
(test-assert "point equality 3" (=? (new-point x:  26 y:   17) [$ add p1  3]))
(test-assert "point equality 4" (=? (new-point x:  20 y:   11) [$ sub p1  3]))
(test-assert "point equality 5" (=? (new-point x:  15 y:   68) [$ point-between p1 p2]))
(test-assert "point equality 6" (=? (new-point x: -23 y:  -14) [$ negate p1]))

(test-assert "point comparison 1" [$ <=? p0 p1])
(test-assert "point comparison 2" (not [$ <=? p1 p3]))
(test-assert "point comparison 3" [$ <?  p0 p1])
(test-assert "point comparison 4" (not [$ <?  p4 p1]))
(test-assert "point comparison 5" [$ <=? p4 p1])
(test-assert "point comparison 6" (not [$ >?  p4 p1]))
(test-assert "point comparison 7" [$ >?  p4 p0])
(test-assert "point comparison 8" [$ >=? p1 p4])
(test-assert "point comparison 9" (not [$ >?  p1 p4]))
(test-assert "point comparison 10" [$ <>? p1 p4])
(test-assert "point comparison 11" (not [$ <>? p1 p1]))

(test-assert "point min" (=? (new-point x:   7 y:   5) [$ min-point p2 p3]))
(test-assert "point max 1" (=? (new-point x:  17 y: 123) [$ max-point p2 p3]))
(test-assert "point max 2" (=? (new-point x: 120 y: 123) [$ max-point (new-point x: 120 y: 45) p2]))

(test-assert (= 5.0 [$ distance-between (new-point x: 0 y: 3) (new-point x: 4 y: 0)]))

(test-assert (=? (new-point x: 16 y: 13)
                 [$ nearest-point-on-line-between p1 p2 p3]))

(test-error "[$ nearest-point-on-line-between p1 2 p3]"
            [$ nearest-point-on-line-between p1 2 p3])

(test-assert "point operation 1" (=? (new-point x: 3 y: 4) (polar->point 5 (asin (+ 0.0 (/ 3 5))))))
(test-assert "point operation 2" (=? (new-rectangle x: [$ x p1] y: [$ y p1] width: [$ x p2] height: [$ y p2])
                                     [$ rect-from-extent p1 p2]))
(test-assert "point operation 3"
             (=? (new-rectangle x: 10 y: 20 width: 30 height: 40)
                 [$ rect-from-2-points (new-point x: 10 y: 20)
                    (new-point x: 40 y: 60)]))
(test-assert "point operation 4" (=? (new-rectangle x: [$ x p1] y: [$ y p1] width: 0 height: 0)
                 [$ as-rectangle p1]))
(test-assert "point operation 5" (= 5.0 [$ polar-r (new-point x: 3 y: 4)]))
(test-assert "point operation 6" (= (atan (+ 0.0 (/ 4 3))) [$ polar-theta (new-point x: 3 y: 4)]))
(test-assert "point operation 7" (=? (new-rectangle x: 7 y: 5 width: 113 height: 118)
                                     (list-of-points->rectangle
                                      (list p1 p2 p3 p4 (new-point x: 120 y: 24)))))

;==============================================================;
;;; RECTANGLEs

(define r1 (new-rectangle x:  3 y:  4 width: 600 height: 400))
(define r2 (new-rectangle x:  4 y:  6 width: 200 height: 100))
(define r3 (new-rectangle x:  5 y: 10 width: 600 height: 400))
(define r4 (new-rectangle x: 15 y:  5 width: 600 height: 400))

(test-assert "rectangle operation 1" [$ contains-rect? r1 r2])
(test-assert "rectangle operation 2" (not [$ contains-rect? r1 r3]))
(test-assert "rectangle operation 3" [$ contains-point? r1 (new-point x: 5 y: 8)])
(test-assert "rectangle operation 4" (not [$ contains-point? r1 (new-point x: 2 y: 2)]))
(test-assert "rectangle operation 5" (= (+ 3 600) [$ max-x r1]))
(test-assert "rectangle operation 6" (= (+ 4 400) [$ max-y r1]))
(test-assert "rectangle operation 7" (=? (new-rectangle x:   3 y: 4 width: 300 height: 400)
                                         [$ with-width  r1 300]))
(test-assert "rectangle operation 8" (=? (new-rectangle x:   3 y: 4 width: 600 height: 200)
                                         [$ with-height r1 200]))
(test-assert "rectangle operation 9" (=? (new-rectangle x:  12 y: 4 width: 600 height: 400)
                                         [$ with-x      r1  12]))
(test-assert "rectangle operation 10" (=? (new-rectangle x:  3 y: 12 width: 600 height: 400)
                                          [$ with-y      r1  12]))
(test-assert "rectangle operation 11" (=? (new-rectangle x:  3 y:  4 width: 100 height: 200)
                                          [$ with-extent r1 (new-point x: 100 y: 200)]))
(test-assert "rectangle operation 12" (=? (new-point x: (+ 3 300) y: (+ 4 200)) [$ center-point r1]))
(test-assert "rectangle operation 13" (=? (new-point x:   3 y:   4) [$ top-left-point      r1]))
(test-assert "rectangle operation 14" (=? (new-point x: 603 y:   4) [$ top-right-point     r1]))
(test-assert "rectangle operation 15" (=? (new-point x:   3 y: 404) [$ bottom-left-point   r1]))
(test-assert "rectangle operation 16" (=? (new-point x: 603 y: 404) [$ bottom-right-point  r1]))
(test-assert "rectangle operation 17" (=? (new-point x:   3 y: 204) [$ left-center-point   r1]))
(test-assert "rectangle operation 18" (=? (new-point x: 603 y: 204) [$ right-center-point  r1]))
(test-assert "rectangle operation 19" (=? (new-point x: 303 y:   4) [$ top-center-point    r1]))
(test-assert "rectangle operation 20" (=? (new-point x: 303 y: 404) [$ bottom-center-point r1]))
(test-assert "rectangle operation 21" (=? (new-rectangle x: 100 y: 100 width: 503 height: 304)
                                          [$ with-top-left      r1 (new-point x: 100 y: 100)]))
(test-assert "rectangle operation 22" (=? (new-rectangle x: 3 y: 100 width:  97 height: 304)
                                          [$ with-top-right     r1 (new-point x: 100 y: 100)]))
(test-assert "rectangle operation 23" (=? (new-rectangle x: 100 y: 4 width: 503 height:  96)
                                          [$ with-bottom-left   r1 (new-point x: 100 y: 100)]))
(test-assert "rectangle operation 24" (=? (new-rectangle x: 3 y: 4 width: 97 height: 96)
                                          [$ with-bottom-right  r1 (new-point x: 100 y: 100)]))
(test-assert "rectangle operation 25" (=? (new-rectangle x: 100 y: 4 width: 503 height: 400)
                                          [$ with-left-center   r1 (new-point x: 100 y: 100)]))
(test-assert "rectangle operation 26" (=? (new-rectangle x: 100 y: 4 width:  97 height: 400)
                                          [$ with-right-center  r1 (new-point x: 100 y: 100)]))
(test-assert "rectangle operation 27" (=? (new-rectangle x: 3 y:   4 width: 600 height: 96)
                                          [$ with-bottom-center r1 (new-point x: 100 y: 100)]))
(test-assert "rectangle operation 28" (=? (new-rectangle x: 3 y: 100 width: 600 height: 304)
                                          [$ with-top-center    r1 (new-point x: 100 y: 100)]))
(test-assert "rectangle operation 29" (=? (new-point x: 100 y: 100)
                                          [$ constrain-point    r1 (new-point x: 100 y: 100)]))
(test-assert "rectangle operation 30" (=? (new-point x: 3 y: 4)
                                          [$ constrain-point    r1 (new-point x: 1 y: 1)]))
(test-assert "rectangle operation 31" (=? (new-point x: 3 y: 404)
                                          [$ constrain-point    r1 (new-point x: 1 y: 800)]))
(test-assert "rectangle operation 32" (=? (new-point x: 603 y: 30)
                                          [$ constrain-point    r1 (new-point x: 800 y: 30)]))
(test-assert "rectangle operation 33" (=? (new-point x: 603 y: 404)
                                          [$ constrain-point    r1 (new-point x: 800 y: 800)]))
(test-assert "rectangle operation 34" (=? (new-rectangle x: 15 y: 10 width: 590 height: 395)
                                          [$ intersection  r3 r4]))
(test-assert "rectangle operation 35"  (=? [$ intersection  r4 r3]
                                           [$ intersection  r3 r4]))
(test-assert "rectangle operation 36" (=? (new-rectangle x:  5 y:  5 width: 610 height: 405)
                                          [$ union         r3 r4]))
(test-assert "rectangle operation 37" (=? [$ union         r4 r3]
                                          [$ union         r3 r4]))
(test-assert "rectangle operation 38" [$ empty? (new-rectangle x: 2 y: 3 width: 0 height: 0)])
(test-assert "rectangle operation 39" (not [$ empty? (new-rectangle x: 2 y: 3 width: 1 height: 0)]))
(test-assert "rectangle operation 40" (not [$ empty? (new-rectangle x: 2 y: 3 width: 0 height: 1)]))
(test-assert "rectangle operation 41" [$ empty? (new-rectangle x: 2 y: 3 width: 0 height: 0)])
(test-assert "rectangle operation 42" [$ empty? [$ rect-from-2-points p1 p1]])
(test-assert "rectangle operation 43" (not [$ empty? r1]))
(test-assert "rectangle operation 44" (=? (new-point x: 200 y: 300)
                                          [$ closest-point-to
                                             (new-rectangle x: 200 y: 300 width: 80 height: 40) p1]))
(test-assert "rectangle operation 45" (=? (new-rectangle x: 26 y: 18 width: 600 height: 400)
                                          [$ translate-by r1 p1]))
(test-assert "rectangle operation 46" (=? (new-rectangle x:    3 y:    4 width:   3000 height:  2000)
                                          [$ scale-by r1  5]))
(test-assert "rectangle operation 47" (=? (new-rectangle x:    3 y:    4 width:  13800 height:  5600)
                                          [$ scale-by r1 p1]))
(test-assert "rectangle operation 48" (=? (new-rectangle x: 2403 y: 2404 width: 120000 height: 40000)
                                          [$ scale-by r1 r2]))
(test-assert "rectangle operation 49" (=? (new-rectangle x: 8 y: 9 width: 590 height: 390)
                                          [$ inset-by r1 5]))
(test-assert "rectangle operation 50" (=? (new-rectangle x: 7 y: 13 width: 596 height: 394)
                                          [$ inset-by r3 (new-point x: 2 y: 3)]))
(test-assert "rectangle operation 51" (=? (new-rectangle x: 1 y: 6 width: 608 height: 408)
                                          [$ expand-by r3 4]))
(test-assert "rectangle operation 52" (=? (new-rectangle x: 3 y: 7 width: 604 height: 406)
                                          [$ expand-by r3 (new-point x: 2 y: 3)]))


;==============================================================;
;;; COLOR

(define c1 (color-named 'blue))
(define c2 (rgb->color 12 124 65))

(test-assert "color operation 1" (color? c1))
(test-assert "color operation 2" (not (color? 'blue)))
(test-assert "color operation 3" (not [$ =? c1 c2]))

(test-assert "color operation 4" (=? (color-named 'black)
                                     (rgb->color   0   0   0)))
(test-assert "color operation 5" (=? (color-named 'white)
                                     (rgb->color 255 255 255)))

(test-error "color error"
           (new-color red: 255 green: 120 blue:  15))

(test-assert "color operation 6" (=? (new-color red: 0 green: 0 blue:  0.8)
                                     [$ mixed-with c1 c2 1]))
(test-assert "color operation 7" (=? (new-color red: .875 green: .875 blue: 0.975)
                                     [$ lighter c1 2]))
(test-assert "color operation 8" (=? (new-color red: 0.0 green: 0.0 blue: 0.1)
                                     [$ darker  c1 2]))
(test-assert "color operation 9" (=? (new-color red: 0.7 green: 0.6475 blue: 0.49)
                                     (hsb->color 45 .3 .7)))
(test-assert "color operation 10" (=? (new-color red: .5 green:  1 blue: .5)
                                      (hsb->color 120 .5 1)))
(test-assert "color operation 11" (=? (new-color red:  0 green:  0 blue: .5)
                                      (hsb->color 240 1 .5)))
(test-assert "color operation 12" (=? (rgb->color  255  0  0)
                                      (hsb->color  0  1  1)))

;==============================================================;
;;; EVENTs

(define e1 (make-event 'mouse-down '(x . 40) '(y . 100)))
(define e2 (make-event 'mouse-move '(x . 45) '(y .  98)))
(define e3 (make-event 'mouse-up   '(x . 46) '(y .  19)))
(define (lookup name alist)
  (cond
   ((assq name alist) => cdr)
   (else (error 'lookup "name not found" name alist))))
(define test-handler
  (lambda (evt)
    (list [$ name evt] (lookup 'x [$ args evt]) (lookup 'y [$ args evt]))))

[$ add-handler! event-handlers 'mouse-up   test-handler]
[$ add-handler! event-handlers 'mouse-down test-handler]
[$ add-handler! event-handlers 'mouse-move test-handler]

(test-equal "event operation 1"
            '(mouse-down 40 100)
            [$ handle-event event-handlers e1])
(test-equal "event operation 2"
            '(mouse-move 45  98)
            [$ handle-event event-handlers e2])
(test-equal "event operation 3"
            '(mouse-up   46  19)
            [$ handle-event event-handlers e3])
(test-equal "event operation 4"
            '(mouse-up   46  19)
            [$ handle-event event-handlers e3])


(test-end)
