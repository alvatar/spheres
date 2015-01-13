;;; Advanced Encryption Standard (AES) symmetric key block cipher.
;;; Marc Feeley
;;; License: lgpl/v2.1
;;; Modifications:
;;; Added optimize/debug declarations 2014, Alvaro Castro-Castilla
;;; Macros ported to rsc-macro-transformer 2014, Alvaro Castro-Castilla

;; All operations are performed on 16 bit unsigned integers.
;; Homogeneous vectors of 16 bit unsigned integers are used to store
;; 32 bit values (each 32 bit value occupies 2 entries of the vector
;; in little-endian format).

(cond-expand
 (optimize
  (declare (fixnum)))
 (else (void)))


(define (make-vect32 n)
  (make-u16vector (* 2 n)))

(define (list->vect32 lst)
  (list->u16vector lst))

(define (u16 lo hi)
  (+ (fxarithmetic-shift-left hi 8) lo))

;; (define-syntax ref32-L
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((u16vect (list-ref form 1))
;;            (i (list-ref form 2)))
;;        `(u16vector-ref ,u16vect (* ,i 2))))))
(define-macro (ref32-L u16vect i)
  `(u16vector-ref ,u16vect (* ,i 2)))

;; (define-syntax ref32-H
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((u16vect (list-ref form 1)) (i (list-ref form 2)))
;;        `(u16vector-ref ,u16vect (+ 1 (* ,i 2)))))))
(define-macro (ref32-H u16vect i)
  `(u16vector-ref ,u16vect (+ 1 (* ,i 2))))

;; (define-syntax ref32
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((var-lo (list-ref form 1))
;;            (var-hi (list-ref form 2))
;;            (u16vect (list-ref form 3))
;;            (i (list-ref form 4))
;;            (body (list-ref form 5)))
;;        `(let* ((,var-hi (* ,i 2))
;;                (,var-lo (u16vector-ref ,u16vect ,var-hi))
;;                (,var-hi (u16vector-ref ,u16vect (+ 1 ,var-hi))))
;;           ,body)))))
(define-macro (ref32 var-lo var-hi u16vect i body)
  `(let* ((,var-hi (* ,i 2))
          (,var-lo (u16vector-ref ,u16vect ,var-hi))
          (,var-hi (u16vector-ref ,u16vect (+ 1 ,var-hi))))
     ,body))

;; (define-syntax set32
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((lo (list-ref form 1)) (hi (list-ref form 2)) (u16vect (list-ref form 3)) (i (list-ref form 4)))
;;        `(let ((i (* ,i 2)))
;;           (u16vector-set! ,u16vect i ,lo)
;;           (u16vector-set! ,u16vect (+ 1 i) ,hi))))))
(define-macro (set32 lo hi u16vect i)
  `(let ((i (* ,i 2)))
     (u16vector-set! ,u16vect i ,lo)
     (u16vector-set! ,u16vect (+ 1 i) ,hi)))

;; (define-syntax
;;   xor32
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((dst-lo (list-ref form 1))
;;            (dst-hi (list-ref form 2))
;;            (x1-lo (list-ref form 3))
;;            (x1-hi (list-ref form 4))
;;            (x2-lo (list-ref form 5))
;;            (x2-hi (list-ref form 6))
;;            (body (list-ref form 7)))
;;        `(let ((,dst-lo (fxxor ,x1-lo ,x2-lo))
;;               (,dst-hi (fxxor ,x1-hi ,x2-hi)))
;;           ,body)))))
(define-macro (xor32 dst-lo dst-hi x1-lo x1-hi x2-lo x2-hi body)
  `(let ((,dst-lo (fxxor ,x1-lo ,x2-lo))
         (,dst-hi (fxxor ,x1-hi ,x2-hi)))
     ,body))

;; (define-syntax get32
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((lo (list-ref form 1))
;;            (hi (list-ref form 2))
;;            (u8vect (list-ref form 3))
;;            (i (list-ref form 4))
;;            (body (list-ref form 5)))
;;        `(let* ((index ,i)
;;                (,lo (u16 (u8vector-ref ,u8vect (+ index 3))
;;                          (u8vector-ref ,u8vect (+ index 2))))
;;                (,hi (u16 (u8vector-ref ,u8vect (+ index 1))
;;                          (u8vector-ref ,u8vect index))))
;;           ,body)))))
(define-macro (get32 lo hi u8vect i body)
  `(let* ((index ,i)
          (,lo (u16 (u8vector-ref ,u8vect (+ index 3))
                    (u8vector-ref ,u8vect (+ index 2))))
          (,hi (u16 (u8vector-ref ,u8vect (+ index 1))
                    (u8vector-ref ,u8vect index))))
     ,body))

;; (define-syntax put32
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((lo (list-ref form 1))
;;            (hi (list-ref form 2))
;;            (u8vect (list-ref form 3))
;;            (i (list-ref form 4)))
;;        `(let ((index ,i))
;;           (u8vector-set! ,u8vect index (fxarithmetic-shift-right ,hi 8))
;;           (u8vector-set! ,u8vect (+ index 1) (fxand 255 ,hi))
;;           (u8vector-set! ,u8vect (+ index 2) (fxarithmetic-shift-right ,lo 8))
;;           (u8vector-set! ,u8vect (+ index 3) (fxand 255 ,lo)))))))
(define-macro (put32 lo hi u8vect i)
  `(let ((index ,i))
     (u8vector-set!
      ,u8vect
      index
      (fxarithmetic-shift-right ,hi 8))
     (u8vector-set!
      ,u8vect
      (+ index 1)
      (fxand #xff ,hi))
     (u8vector-set!
      ,u8vect
      (+ index 2)
      (fxarithmetic-shift-right ,lo 8))
     (u8vector-set!
      ,u8vect
      (+ index 3)
      (fxand #xff ,lo))))


;;----------------------------------------------------------------------------
;; Useful tables

(define block-size 16)

(define (build-table v f0 f1)
  (let ((t (make-u16vector 512)))
    (let loop ((i 0))
      (if (< i 256)
          (let* ((i*2 (* i 2))
                 (x (vector-ref v i*2))
                 (y (vector-ref v (+ 1 i*2)))
                 (a (fxarithmetic-shift-right x 8))
                 (b (fxand #xff x))
                 (c (fxarithmetic-shift-right y 8))
                 (d (fxand #xff y)))
            (u16vector-set! t i*2       (f1 a b c d))
            (u16vector-set! t (+ 1 i*2) (f0 a b c d))
            (loop (+ i 1)))
          t))))

(define FSb
  (list->u8vector
   '(#x63 #x7C #x77 #x7B #xF2 #x6B #x6F #xC5
     #x30 #x01 #x67 #x2B #xFE #xD7 #xAB #x76
     #xCA #x82 #xC9 #x7D #xFA #x59 #x47 #xF0
     #xAD #xD4 #xA2 #xAF #x9C #xA4 #x72 #xC0
     #xB7 #xFD #x93 #x26 #x36 #x3F #xF7 #xCC
     #x34 #xA5 #xE5 #xF1 #x71 #xD8 #x31 #x15
     #x04 #xC7 #x23 #xC3 #x18 #x96 #x05 #x9A
     #x07 #x12 #x80 #xE2 #xEB #x27 #xB2 #x75
     #x09 #x83 #x2C #x1A #x1B #x6E #x5A #xA0
     #x52 #x3B #xD6 #xB3 #x29 #xE3 #x2F #x84
     #x53 #xD1 #x00 #xED #x20 #xFC #xB1 #x5B
     #x6A #xCB #xBE #x39 #x4A #x4C #x58 #xCF
     #xD0 #xEF #xAA #xFB #x43 #x4D #x33 #x85
     #x45 #xF9 #x02 #x7F #x50 #x3C #x9F #xA8
     #x51 #xA3 #x40 #x8F #x92 #x9D #x38 #xF5
     #xBC #xB6 #xDA #x21 #x10 #xFF #xF3 #xD2
     #xCD #x0C #x13 #xEC #x5F #x97 #x44 #x17
     #xC4 #xA7 #x7E #x3D #x64 #x5D #x19 #x73
     #x60 #x81 #x4F #xDC #x22 #x2A #x90 #x88
     #x46 #xEE #xB8 #x14 #xDE #x5E #x0B #xDB
     #xE0 #x32 #x3A #x0A #x49 #x06 #x24 #x5C
     #xC2 #xD3 #xAC #x62 #x91 #x95 #xE4 #x79
     #xE7 #xC8 #x37 #x6D #x8D #xD5 #x4E #xA9
     #x6C #x56 #xF4 #xEA #x65 #x7A #xAE #x08
     #xBA #x78 #x25 #x2E #x1C #xA6 #xB4 #xC6
     #xE8 #xDD #x74 #x1F #x4B #xBD #x8B #x8A
     #x70 #x3E #xB5 #x66 #x48 #x03 #xF6 #x0E
     #x61 #x35 #x57 #xB9 #x86 #xC1 #x1D #x9E
     #xE1 #xF8 #x98 #x11 #x69 #xD9 #x8E #x94
     #x9B #x1E #x87 #xE9 #xCE #x55 #x28 #xDF
     #x8C #xA1 #x89 #x0D #xBF #xE6 #x42 #x68
     #x41 #x99 #x2D #x0F #xB0 #x54 #xBB #x16)))

;; (define-syntax FSb-ref
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((i (list-ref form 1)))
;;        `(u8vector-ref FSb ,i)))))
(define-macro (FSb-ref i)
  `(u8vector-ref FSb ,i))

(define FT
  '#(#xC663 #x63A5 #xF87C #x7C84 #xEE77 #x7799 #xF67B #x7B8D
     #xFFF2 #xF20D #xD66B #x6BBD #xDE6F #x6FB1 #x91C5 #xC554
     #x6030 #x3050 #x0201 #x0103 #xCE67 #x67A9 #x562B #x2B7D
     #xE7FE #xFE19 #xB5D7 #xD762 #x4DAB #xABE6 #xEC76 #x769A
     #x8FCA #xCA45 #x1F82 #x829D #x89C9 #xC940 #xFA7D #x7D87
     #xEFFA #xFA15 #xB259 #x59EB #x8E47 #x47C9 #xFBF0 #xF00B
     #x41AD #xADEC #xB3D4 #xD467 #x5FA2 #xA2FD #x45AF #xAFEA
     #x239C #x9CBF #x53A4 #xA4F7 #xE472 #x7296 #x9BC0 #xC05B
     #x75B7 #xB7C2 #xE1FD #xFD1C #x3D93 #x93AE #x4C26 #x266A
     #x6C36 #x365A #x7E3F #x3F41 #xF5F7 #xF702 #x83CC #xCC4F
     #x6834 #x345C #x51A5 #xA5F4 #xD1E5 #xE534 #xF9F1 #xF108
     #xE271 #x7193 #xABD8 #xD873 #x6231 #x3153 #x2A15 #x153F
     #x0804 #x040C #x95C7 #xC752 #x4623 #x2365 #x9DC3 #xC35E
     #x3018 #x1828 #x3796 #x96A1 #x0A05 #x050F #x2F9A #x9AB5
     #x0E07 #x0709 #x2412 #x1236 #x1B80 #x809B #xDFE2 #xE23D
     #xCDEB #xEB26 #x4E27 #x2769 #x7FB2 #xB2CD #xEA75 #x759F
     #x1209 #x091B #x1D83 #x839E #x582C #x2C74 #x341A #x1A2E
     #x361B #x1B2D #xDC6E #x6EB2 #xB45A #x5AEE #x5BA0 #xA0FB
     #xA452 #x52F6 #x763B #x3B4D #xB7D6 #xD661 #x7DB3 #xB3CE
     #x5229 #x297B #xDDE3 #xE33E #x5E2F #x2F71 #x1384 #x8497
     #xA653 #x53F5 #xB9D1 #xD168 #x0000 #x0000 #xC1ED #xED2C
     #x4020 #x2060 #xE3FC #xFC1F #x79B1 #xB1C8 #xB65B #x5BED
     #xD46A #x6ABE #x8DCB #xCB46 #x67BE #xBED9 #x7239 #x394B
     #x944A #x4ADE #x984C #x4CD4 #xB058 #x58E8 #x85CF #xCF4A
     #xBBD0 #xD06B #xC5EF #xEF2A #x4FAA #xAAE5 #xEDFB #xFB16
     #x8643 #x43C5 #x9A4D #x4DD7 #x6633 #x3355 #x1185 #x8594
     #x8A45 #x45CF #xE9F9 #xF910 #x0402 #x0206 #xFE7F #x7F81
     #xA050 #x50F0 #x783C #x3C44 #x259F #x9FBA #x4BA8 #xA8E3
     #xA251 #x51F3 #x5DA3 #xA3FE #x8040 #x40C0 #x058F #x8F8A
     #x3F92 #x92AD #x219D #x9DBC #x7038 #x3848 #xF1F5 #xF504
     #x63BC #xBCDF #x77B6 #xB6C1 #xAFDA #xDA75 #x4221 #x2163
     #x2010 #x1030 #xE5FF #xFF1A #xFDF3 #xF30E #xBFD2 #xD26D
     #x81CD #xCD4C #x180C #x0C14 #x2613 #x1335 #xC3EC #xEC2F
     #xBE5F #x5FE1 #x3597 #x97A2 #x8844 #x44CC #x2E17 #x1739
     #x93C4 #xC457 #x55A7 #xA7F2 #xFC7E #x7E82 #x7A3D #x3D47
     #xC864 #x64AC #xBA5D #x5DE7 #x3219 #x192B #xE673 #x7395
     #xC060 #x60A0 #x1981 #x8198 #x9E4F #x4FD1 #xA3DC #xDC7F
     #x4422 #x2266 #x542A #x2A7E #x3B90 #x90AB #x0B88 #x8883
     #x8C46 #x46CA #xC7EE #xEE29 #x6BB8 #xB8D3 #x2814 #x143C
     #xA7DE #xDE79 #xBC5E #x5EE2 #x160B #x0B1D #xADDB #xDB76
     #xDBE0 #xE03B #x6432 #x3256 #x743A #x3A4E #x140A #x0A1E
     #x9249 #x49DB #x0C06 #x060A #x4824 #x246C #xB85C #x5CE4
     #x9FC2 #xC25D #xBDD3 #xD36E #x43AC #xACEF #xC462 #x62A6
     #x3991 #x91A8 #x3195 #x95A4 #xD3E4 #xE437 #xF279 #x798B
     #xD5E7 #xE732 #x8BC8 #xC843 #x6E37 #x3759 #xDA6D #x6DB7
     #x018D #x8D8C #xB1D5 #xD564 #x9C4E #x4ED2 #x49A9 #xA9E0
     #xD86C #x6CB4 #xAC56 #x56FA #xF3F4 #xF407 #xCFEA #xEA25
     #xCA65 #x65AF #xF47A #x7A8E #x47AE #xAEE9 #x1008 #x0818
     #x6FBA #xBAD5 #xF078 #x7888 #x4A25 #x256F #x5C2E #x2E72
     #x381C #x1C24 #x57A6 #xA6F1 #x73B4 #xB4C7 #x97C6 #xC651
     #xCBE8 #xE823 #xA1DD #xDD7C #xE874 #x749C #x3E1F #x1F21
     #x964B #x4BDD #x61BD #xBDDC #x0D8B #x8B86 #x0F8A #x8A85
     #xE070 #x7090 #x7C3E #x3E42 #x71B5 #xB5C4 #xCC66 #x66AA
     #x9048 #x48D8 #x0603 #x0305 #xF7F6 #xF601 #x1C0E #x0E12
     #xC261 #x61A3 #x6A35 #x355F #xAE57 #x57F9 #x69B9 #xB9D0
     #x1786 #x8691 #x99C1 #xC158 #x3A1D #x1D27 #x279E #x9EB9
     #xD9E1 #xE138 #xEBF8 #xF813 #x2B98 #x98B3 #x2211 #x1133
     #xD269 #x69BB #xA9D9 #xD970 #x078E #x8E89 #x3394 #x94A7
     #x2D9B #x9BB6 #x3C1E #x1E22 #x1587 #x8792 #xC9E9 #xE920
     #x87CE #xCE49 #xAA55 #x55FF #x5028 #x2878 #xA5DF #xDF7A
     #x038C #x8C8F #x59A1 #xA1F8 #x0989 #x8980 #x1A0D #x0D17
     #x65BF #xBFDA #xD7E6 #xE631 #x8442 #x42C6 #xD068 #x68B8
     #x8241 #x41C3 #x2999 #x99B0 #x5A2D #x2D77 #x1E0F #x0F11
     #x7BB0 #xB0CB #xA854 #x54FC #x6DBB #xBBD6 #x2C16 #x163A))

(define FT0
  (build-table FT (lambda (a b c d) (u16 b a)) (lambda (a b c d) (u16 d c))))

(define FT1
  (build-table FT (lambda (a b c d) (u16 a d)) (lambda (a b c d) (u16 c b))))

(define FT2
  (build-table FT (lambda (a b c d) (u16 d c)) (lambda (a b c d) (u16 b a))))

(define FT3
  (build-table FT (lambda (a b c d) (u16 c b)) (lambda (a b c d) (u16 a d))))

(define RSb
  (list->u8vector
   '(#x52 #x09 #x6A #xD5 #x30 #x36 #xA5 #x38
     #xBF #x40 #xA3 #x9E #x81 #xF3 #xD7 #xFB
     #x7C #xE3 #x39 #x82 #x9B #x2F #xFF #x87
     #x34 #x8E #x43 #x44 #xC4 #xDE #xE9 #xCB
     #x54 #x7B #x94 #x32 #xA6 #xC2 #x23 #x3D
     #xEE #x4C #x95 #x0B #x42 #xFA #xC3 #x4E
     #x08 #x2E #xA1 #x66 #x28 #xD9 #x24 #xB2
     #x76 #x5B #xA2 #x49 #x6D #x8B #xD1 #x25
     #x72 #xF8 #xF6 #x64 #x86 #x68 #x98 #x16
     #xD4 #xA4 #x5C #xCC #x5D #x65 #xB6 #x92
     #x6C #x70 #x48 #x50 #xFD #xED #xB9 #xDA
     #x5E #x15 #x46 #x57 #xA7 #x8D #x9D #x84
     #x90 #xD8 #xAB #x00 #x8C #xBC #xD3 #x0A
     #xF7 #xE4 #x58 #x05 #xB8 #xB3 #x45 #x06
     #xD0 #x2C #x1E #x8F #xCA #x3F #x0F #x02
     #xC1 #xAF #xBD #x03 #x01 #x13 #x8A #x6B
     #x3A #x91 #x11 #x41 #x4F #x67 #xDC #xEA
     #x97 #xF2 #xCF #xCE #xF0 #xB4 #xE6 #x73
     #x96 #xAC #x74 #x22 #xE7 #xAD #x35 #x85
     #xE2 #xF9 #x37 #xE8 #x1C #x75 #xDF #x6E
     #x47 #xF1 #x1A #x71 #x1D #x29 #xC5 #x89
     #x6F #xB7 #x62 #x0E #xAA #x18 #xBE #x1B
     #xFC #x56 #x3E #x4B #xC6 #xD2 #x79 #x20
     #x9A #xDB #xC0 #xFE #x78 #xCD #x5A #xF4
     #x1F #xDD #xA8 #x33 #x88 #x07 #xC7 #x31
     #xB1 #x12 #x10 #x59 #x27 #x80 #xEC #x5F
     #x60 #x51 #x7F #xA9 #x19 #xB5 #x4A #x0D
     #x2D #xE5 #x7A #x9F #x93 #xC9 #x9C #xEF
     #xA0 #xE0 #x3B #x4D #xAE #x2A #xF5 #xB0
     #xC8 #xEB #xBB #x3C #x83 #x53 #x99 #x61
     #x17 #x2B #x04 #x7E #xBA #x77 #xD6 #x26
     #xE1 #x69 #x14 #x63 #x55 #x21 #x0C #x7D)))

;; (define-syntax RSb-ref
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((i (list-ref form 1)))
;;        `(u8vector-ref RSb ,i)))))
(define-macro (RSb-ref i)
  `(u8vector-ref RSb ,i))

(define RT
  '#(#x51F4 #xA750 #x7E41 #x6553 #x1A17 #xA4C3 #x3A27 #x5E96
     #x3BAB #x6BCB #x1F9D #x45F1 #xACFA #x58AB #x4BE3 #x0393
     #x2030 #xFA55 #xAD76 #x6DF6 #x88CC #x7691 #xF502 #x4C25
     #x4FE5 #xD7FC #xC52A #xCBD7 #x2635 #x4480 #xB562 #xA38F
     #xDEB1 #x5A49 #x25BA #x1B67 #x45EA #x0E98 #x5DFE #xC0E1
     #xC32F #x7502 #x814C #xF012 #x8D46 #x97A3 #x6BD3 #xF9C6
     #x038F #x5FE7 #x1592 #x9C95 #xBF6D #x7AEB #x9552 #x59DA
     #xD4BE #x832D #x5874 #x21D3 #x49E0 #x6929 #x8EC9 #xC844
     #x75C2 #x896A #xF48E #x7978 #x9958 #x3E6B #x27B9 #x71DD
     #xBEE1 #x4FB6 #xF088 #xAD17 #xC920 #xAC66 #x7DCE #x3AB4
     #x63DF #x4A18 #xE51A #x3182 #x9751 #x3360 #x6253 #x7F45
     #xB164 #x77E0 #xBB6B #xAE84 #xFE81 #xA01C #xF908 #x2B94
     #x7048 #x6858 #x8F45 #xFD19 #x94DE #x6C87 #x527B #xF8B7
     #xAB73 #xD323 #x724B #x02E2 #xE31F #x8F57 #x6655 #xAB2A
     #xB2EB #x2807 #x2FB5 #xC203 #x86C5 #x7B9A #xD337 #x08A5
     #x3028 #x87F2 #x23BF #xA5B2 #x0203 #x6ABA #xED16 #x825C
     #x8ACF #x1C2B #xA779 #xB492 #xF307 #xF2F0 #x4E69 #xE2A1
     #x65DA #xF4CD #x0605 #xBED5 #xD134 #x621F #xC4A6 #xFE8A
     #x342E #x539D #xA2F3 #x55A0 #x058A #xE132 #xA4F6 #xEB75
     #x0B83 #xEC39 #x4060 #xEFAA #x5E71 #x9F06 #xBD6E #x1051
     #x3E21 #x8AF9 #x96DD #x063D #xDD3E #x05AE #x4DE6 #xBD46
     #x9154 #x8DB5 #x71C4 #x5D05 #x0406 #xD46F #x6050 #x15FF
     #x1998 #xFB24 #xD6BD #xE997 #x8940 #x43CC #x67D9 #x9E77
     #xB0E8 #x42BD #x0789 #x8B88 #xE719 #x5B38 #x79C8 #xEEDB
     #xA17C #x0A47 #x7C42 #x0FE9 #xF884 #x1EC9 #x0000 #x0000
     #x0980 #x8683 #x322B #xED48 #x1E11 #x70AC #x6C5A #x724E
     #xFD0E #xFFFB #x0F85 #x3856 #x3DAE #xD51E #x362D #x3927
     #x0A0F #xD964 #x685C #xA621 #x9B5B #x54D1 #x2436 #x2E3A
     #x0C0A #x67B1 #x9357 #xE70F #xB4EE #x96D2 #x1B9B #x919E
     #x80C0 #xC54F #x61DC #x20A2 #x5A77 #x4B69 #x1C12 #x1A16
     #xE293 #xBA0A #xC0A0 #x2AE5 #x3C22 #xE043 #x121B #x171D
     #x0E09 #x0D0B #xF28B #xC7AD #x2DB6 #xA8B9 #x141E #xA9C8
     #x57F1 #x1985 #xAF75 #x074C #xEE99 #xDDBB #xA37F #x60FD
     #xF701 #x269F #x5C72 #xF5BC #x4466 #x3BC5 #x5BFB #x7E34
     #x8B43 #x2976 #xCB23 #xC6DC #xB6ED #xFC68 #xB8E4 #xF163
     #xD731 #xDCCA #x4263 #x8510 #x1397 #x2240 #x84C6 #x1120
     #x854A #x247D #xD2BB #x3DF8 #xAEF9 #x3211 #xC729 #xA16D
     #x1D9E #x2F4B #xDCB2 #x30F3 #x0D86 #x52EC #x77C1 #xE3D0
     #x2BB3 #x166C #xA970 #xB999 #x1194 #x48FA #x47E9 #x6422
     #xA8FC #x8CC4 #xA0F0 #x3F1A #x567D #x2CD8 #x2233 #x90EF
     #x8749 #x4EC7 #xD938 #xD1C1 #x8CCA #xA2FE #x98D4 #x0B36
     #xA6F5 #x81CF #xA57A #xDE28 #xDAB7 #x8E26 #x3FAD #xBFA4
     #x2C3A #x9DE4 #x5078 #x920D #x6A5F #xCC9B #x547E #x4662
     #xF68D #x13C2 #x90D8 #xB8E8 #x2E39 #xF75E #x82C3 #xAFF5
     #x9F5D #x80BE #x69D0 #x937C #x6FD5 #x2DA9 #xCF25 #x12B3
     #xC8AC #x993B #x1018 #x7DA7 #xE89C #x636E #xDB3B #xBB7B
     #xCD26 #x7809 #x6E59 #x18F4 #xEC9A #xB701 #x834F #x9AA8
     #xE695 #x6E65 #xAAFF #xE67E #x21BC #xCF08 #xEF15 #xE8E6
     #xBAE7 #x9BD9 #x4A6F #x36CE #xEA9F #x09D4 #x29B0 #x7CD6
     #x31A4 #xB2AF #x2A3F #x2331 #xC6A5 #x9430 #x35A2 #x66C0
     #x744E #xBC37 #xFC82 #xCAA6 #xE090 #xD0B0 #x33A7 #xD815
     #xF104 #x984A #x41EC #xDAF7 #x7FCD #x500E #x1791 #xF62F
     #x764D #xD68D #x43EF #xB04D #xCCAA #x4D54 #xE496 #x04DF
     #x9ED1 #xB5E3 #x4C6A #x881B #xC12C #x1FB8 #x4665 #x517F
     #x9D5E #xEA04 #x018C #x355D #xFA87 #x7473 #xFB0B #x412E
     #xB367 #x1D5A #x92DB #xD252 #xE910 #x5633 #x6DD6 #x4713
     #x9AD7 #x618C #x37A1 #x0C7A #x59F8 #x148E #xEB13 #x3C89
     #xCEA9 #x27EE #xB761 #xC935 #xE11C #xE5ED #x7A47 #xB13C
     #x9CD2 #xDF59 #x55F2 #x733F #x1814 #xCE79 #x73C7 #x37BF
     #x53F7 #xCDEA #x5FFD #xAA5B #xDF3D #x6F14 #x7844 #xDB86
     #xCAAF #xF381 #xB968 #xC43E #x3824 #x342C #xC2A3 #x405F
     #x161D #xC372 #xBCE2 #x250C #x283C #x498B #xFF0D #x9541
     #x39A8 #x0171 #x080C #xB3DE #xD8B4 #xE49C #x6456 #xC190
     #x7BCB #x8461 #xD532 #xB670 #x486C #x5C74 #xD0B8 #x5742))

(define RT0
  (build-table RT (lambda (a b c d) (u16 b a)) (lambda (a b c d) (u16 d c))))

(define RT1
  (build-table RT (lambda (a b c d) (u16 a d)) (lambda (a b c d) (u16 c b))))

(define RT2
  (build-table RT (lambda (a b c d) (u16 d c)) (lambda (a b c d) (u16 b a))))

(define RT3
  (build-table RT (lambda (a b c d) (u16 c b)) (lambda (a b c d) (u16 a d))))

(define RCON
  (list->vect32
   '(#x0000 #x0100
     #x0000 #x0200
     #x0000 #x0400
     #x0000 #x0800
     #x0000 #x1000
     #x0000 #x2000
     #x0000 #x4000
     #x0000 #x8000
     #x0000 #x1B00
     #x0000 #x3600)))

;;;----------------------------------------------------------------------------

(define-structure aes-context
  ;; uid: aes-context-5e6282ec-d8bb-48ef-8fba-9c48cfe1b44c
  keysize ;; size of key in bits (128, 192, or 256)
  erk     ;; encryption round key
  drk     ;; decryption round key
  nr      ;; number of rounds
  KT0     ;; u8vectors of length 256
  KT1
  KT2
  KT3)

;; (define-syntax xorAA
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((var-lo (list-ref form 1))
;;            (var-hi (list-ref form 2))
;;            (X-L (list-ref form 3))
;;            (X-H (list-ref form 4))
;;            (i (list-ref form 5))
;;            (Y-L (list-ref form 6))
;;            (Y-H (list-ref form 7))
;;            (body (list-ref form 8)))
;;        `(let* ((,var-lo
;;                 (fxxor ,X-L (fxxor (ref32-L RCON ,i) (u16 (FSb-ref (fxarithmetic-shift-right ,Y-H 8)) (FSb-ref (fxand 255 ,Y-L))))))
;;                (,var-hi
;;                 (fxxor ,X-H (fxxor (ref32-H RCON ,i) (u16 (FSb-ref (fxarithmetic-shift-right ,Y-L 8)) (FSb-ref (fxand 255 ,Y-H)))))))
;;           ,body)))))
(define-macro (xorAA var-lo var-hi X-L X-H i Y-L Y-H body)
  `(let* ((,var-lo
           (fxxor
            ,X-L
            (fxxor
             (ref32-L RCON ,i)
             (u16 (FSb-ref (fxarithmetic-shift-right ,Y-H 8))
                  (FSb-ref (fxand #xff ,Y-L))))))
          (,var-hi
           (fxxor
            ,X-H
            (fxxor
             (ref32-H RCON ,i)
             (u16 (FSb-ref (fxarithmetic-shift-right ,Y-L 8))
                  (FSb-ref (fxand #xff ,Y-H)))))))
     ,body))

;; (define-syntax xorBB
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((var-lo (list-ref form 1))
;;            (var-hi (list-ref form 2))
;;            (X-L (list-ref form 3))
;;            (X-H (list-ref form 4))
;;            (Y-L (list-ref form 5))
;;            (Y-H (list-ref form 6))
;;            (body (list-ref form 7)))
;;        `(let* ((,var-lo (fxxor ,X-L (u16 (FSb-ref (fxand 255 ,Y-L)) (FSb-ref (fxarithmetic-shift-right ,Y-L 8)))))
;;                (,var-hi (fxxor ,X-H (u16 (FSb-ref (fxand 255 ,Y-H)) (FSb-ref (fxarithmetic-shift-right ,Y-H 8))))))
;;           ,body)))))
(define-macro (xorBB var-lo var-hi X-L X-H Y-L Y-H body)
  `(let* ((,var-lo
           (fxxor
            ,X-L
            (u16 (FSb-ref (fxand #xff ,Y-L))
                 (FSb-ref (fxarithmetic-shift-right ,Y-L 8)))))
          (,var-hi
           (fxxor
            ,X-H
            (u16 (FSb-ref (fxand #xff ,Y-H))
                 (FSb-ref (fxarithmetic-shift-right ,Y-H 8))))))
     ,body))

(define (u8vector->aes-context key) ;; key: u8vector of length 16, 24 or 32
  (let ((len (u8vector-length key)))
    (if (not (or (= len 16)
                 (= len 24)
                 (= len 32)))
        (error "key must be a u8vector of length 16, 24 or 32")
        (let* ((keysize
                (* len 8))
               (nr
                (cond ((= keysize 256) 14)
                      ((= keysize 192) 12)
                      (else            10)))
               (RK
                (make-vect32 64))
               (SK
                (make-vect32 64))
               (KT0
                (make-vect32 256))
               (KT1
                (make-vect32 256))
               (KT2
                (make-vect32 256))
               (KT3
                (make-vect32 256)))
          (let loop ((i 0))
            (if (< i (quotient keysize 32))
                (get32 X-L X-H key (* i 4)
                (begin
                  (set32 X-L X-H RK i)
                  (loop (+ i 1))))))
          (case nr
            ((10)
             (let loop ((i 0))
               (if (< i 10)
                   (let ((k (* i 4)))
                     (ref32 RK0-L RK0-H RK k
                     (ref32 RK1-L RK1-H RK (+ k 1)
                     (ref32 RK2-L RK2-H RK (+ k 2)
                     (ref32 RK3-L RK3-H RK (+ k 3)
                     (xorAA RK4-L RK4-H RK0-L RK0-H i RK3-L RK3-H
                     (xor32 RK5-L RK5-H RK1-L RK1-H RK4-L RK4-H
                     (xor32 RK6-L RK6-H RK2-L RK2-H RK5-L RK5-H
                     (xor32 RK7-L RK7-H RK3-L RK3-H RK6-L RK6-H
                     (begin
                       (set32 RK4-L RK4-H RK (+ k 4))
                       (set32 RK5-L RK5-H RK (+ k 5))
                       (set32 RK6-L RK6-H RK (+ k 6))
                       (set32 RK7-L RK7-H RK (+ k 7))
                       (loop (+ i 1)))))))))))))))
            ((12)
             (let loop ((i 0))
               (if (< i 8)
                   (let ((k (* i 6)))
                     (ref32 RK0-L RK0-H RK k
                     (ref32 RK1-L RK1-H RK (+ k 1)
                     (ref32 RK2-L RK2-H RK (+ k 2)
                     (ref32 RK3-L RK3-H RK (+ k 3)
                     (ref32 RK4-L RK4-H RK (+ k 4)
                     (ref32 RK5-L RK5-H RK (+ k 5)
                     (xorAA RK6-L RK6-H RK0-L RK0-H i RK5-L RK5-H
                     (xor32 RK7-L RK7-H RK1-L RK1-H RK6-L RK6-H
                     (xor32 RK8-L RK8-H RK2-L RK2-H RK7-L RK7-H
                     (xor32 RK9-L RK9-H RK3-L RK3-H RK8-L RK8-H
                     (xor32 RK10-L RK10-H RK4-L RK4-H RK9-L RK9-H
                     (xor32 RK11-L RK11-H RK5-L RK5-H RK10-L RK10-H
                     (begin
                       (set32 RK6-L RK6-H RK (+ k 6))
                       (set32 RK7-L RK7-H RK (+ k 7))
                       (set32 RK8-L RK8-H RK (+ k 8))
                       (set32 RK9-L RK9-H RK (+ k 9))
                       (set32 RK10-L RK10-H RK (+ k 10))
                       (set32 RK11-L RK11-H RK (+ k 11))
                       (loop (+ i 1)))))))))))))))))))
            ((14)
             (let loop ((i 0))
               (if (< i 7)
                   (let ((k (* i 8)))
                     (ref32 RK0-L RK0-H RK k
                     (ref32 RK1-L RK1-H RK (+ k 1)
                     (ref32 RK2-L RK2-H RK (+ k 2)
                     (ref32 RK3-L RK3-H RK (+ k 3)
                     (ref32 RK4-L RK4-H RK (+ k 4)
                     (ref32 RK5-L RK5-H RK (+ k 5)
                     (ref32 RK6-L RK6-H RK (+ k 6)
                     (ref32 RK7-L RK7-H RK (+ k 7)
                     (xorAA RK8-L RK8-H RK0-L RK0-H i RK7-L RK7-H
                     (xor32 RK9-L RK9-H RK1-L RK1-H RK8-L RK8-H
                     (xor32 RK10-L RK10-H RK2-L RK2-H RK9-L RK9-H
                     (xor32 RK11-L RK11-H RK3-L RK3-H RK10-L RK10-H
                     (xorBB RK12-L RK12-H RK4-L RK4-H RK11-L RK11-H
                     (xor32 RK13-L RK13-H RK5-L RK5-H RK12-L RK12-H
                     (xor32 RK14-L RK14-H RK6-L RK6-H RK13-L RK13-H
                     (xor32 RK15-L RK15-H RK7-L RK7-H RK14-L RK14-H
                     (begin
                       (set32 RK8-L RK8-H RK (+ k 8))
                       (set32 RK9-L RK9-H RK (+ k 9))
                       (set32 RK10-L RK10-H RK (+ k 10))
                       (set32 RK11-L RK11-H RK (+ k 11))
                       (set32 RK12-L RK12-H RK (+ k 12))
                       (set32 RK13-L RK13-H RK (+ k 13))
                       (set32 RK14-L RK14-H RK (+ k 14))
                       (set32 RK15-L RK15-H RK (+ k 15))
                       (loop (+ i 1))))))))))))))))))))))))
          (let loop ((i 0))
            (if (< i 256)
                (let ((j (FSb-ref i)))
                  (ref32 L H RT0 j (set32 L H KT0 i))
                  (ref32 L H RT1 j (set32 L H KT1 i))
                  (ref32 L H RT2 j (set32 L H KT2 i))
                  (ref32 L H RT3 j (set32 L H KT3 i))
                  (loop (+ i 1)))))
          (let ((j 0)
                (k (* nr 4)))
            (ref32 L H RK k       (set32       L H SK j))
            (ref32 L H RK (+ k 1) (set32 L H SK (+ j 1)))
            (ref32 L H RK (+ k 2) (set32 L H SK (+ j 2)))
            (ref32 L H RK (+ k 3) (set32 L H SK (+ j 3))))
          (let loop ((i 1))
            (if (< i nr)
                (let ((j (* i 4))
                      (k (* (- nr i) 4)))
                  (define (f offs)
                    (ref32 L H RK (+ k offs)
                    (let* ((i0 (fxarithmetic-shift-right H 8))
                           (i1 (fxand #xff H))
                           (i2 (fxarithmetic-shift-right L 8))
                           (i3 (fxand #xff L)))
                      (ref32 T0-L T0-H KT0 i0
                      (ref32 T1-L T1-H KT1 i1
                      (xor32 T2-L T2-H T0-L T0-H T1-L T1-H
                      (ref32 T3-L T3-H KT2 i2
                      (xor32 T4-L T4-H T3-L T3-H T2-L T2-H
                      (ref32 T5-L T5-H KT3 i3
                      (xor32 T6-L T6-H T5-L T5-H T4-L T4-H
                      (set32 T6-L T6-H SK (+ j offs))))))))))))
                  (f 0)
                  (f 1)
                  (f 2)
                  (f 3)
                  (loop (+ i 1)))))
          (let ((j (* nr 4))
                (k 0))
            (ref32 L H RK k       (set32 L H SK j))
            (ref32 L H RK (+ k 1) (set32 L H SK (+ j 1)))
            (ref32 L H RK (+ k 2) (set32 L H SK (+ j 2)))
            (ref32 L H RK (+ k 3) (set32 L H SK (+ j 3))))
          (make-aes-context keysize RK SK nr KT0 KT1 KT2 KT3)))))

;;;----------------------------------------------------------------------------

;;! Electronic Codebook (ECB) Mode Encryption.

;; (define-syntax FCOMB1
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((var-lo (list-ref form 1))
;;            (var-hi (list-ref form 2))
;;            (i (list-ref form 3))
;;            (Y0-H (list-ref form 4))
;;            (Y1-H (list-ref form 5))
;;            (Y2-L (list-ref form 6))
;;            (Y3-L (list-ref form 7))
;;            (body (list-ref form 8)))
;;        `(let ((,var-lo
;;                (fxxor (ref32-L RK ,i)
;;                       (fxxor (ref32-L FT0 (fxarithmetic-shift-right ,Y0-H 8))
;;                              (fxxor (ref32-L FT1 (fxand 255 ,Y1-H))
;;                                     (fxxor (ref32-L FT2 (fxarithmetic-shift-right ,Y2-L 8)) (ref32-L FT3 (fxand 255 ,Y3-L)))))))
;;               (,var-hi
;;                (fxxor (ref32-H RK ,i)
;;                       (fxxor (ref32-H FT0 (fxarithmetic-shift-right ,Y0-H 8))
;;                              (fxxor (ref32-H FT1 (fxand 255 ,Y1-H))
;;                                     (fxxor (ref32-H FT2 (fxarithmetic-shift-right ,Y2-L 8)) (ref32-H FT3 (fxand 255 ,Y3-L)))))))
;;               (,i (+ ,i 1)))
;;           ,body)))))
(define-macro (FCOMB1 var-lo var-hi i Y0-H Y1-H Y2-L Y3-L body)
  `(let ((,var-lo
          (fxxor
           (ref32-L RK ,i)
           (fxxor
            (ref32-L FT0 (fxarithmetic-shift-right ,Y0-H 8))
            (fxxor
             (ref32-L FT1 (fxand #xff ,Y1-H))
             (fxxor
              (ref32-L FT2 (fxarithmetic-shift-right ,Y2-L 8))
              (ref32-L FT3 (fxand #xff ,Y3-L)))))))
         (,var-hi
          (fxxor
           (ref32-H RK ,i)
           (fxxor
            (ref32-H FT0 (fxarithmetic-shift-right ,Y0-H 8))
            (fxxor
             (ref32-H FT1 (fxand #xff ,Y1-H))
             (fxxor
              (ref32-H FT2 (fxarithmetic-shift-right ,Y2-L 8))
              (ref32-H FT3 (fxand #xff ,Y3-L)))))))
         (,i
          (+ ,i 1)))
     ,body))

;; (define-syntax FCOMB2
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((var-lo (list-ref form 1))
;;            (var-hi (list-ref form 2))
;;            (i (list-ref form 3))
;;            (Y0-H (list-ref form 4))
;;            (Y1-H (list-ref form 5))
;;            (Y2-L (list-ref form 6))
;;            (Y3-L (list-ref form 7))
;;            (body (list-ref form 8)))
;;        `(let ((,var-lo (fxxor (ref32-L RK ,i) (u16 (FSb-ref (fxand 255 ,Y3-L)) (FSb-ref (fxarithmetic-shift-right ,Y2-L 8)))))
;;               (,var-hi (fxxor (ref32-H RK ,i) (u16 (FSb-ref (fxand 255 ,Y1-H)) (FSb-ref (fxarithmetic-shift-right ,Y0-H 8)))))
;;               (,i (+ ,i 1)))
;;           ,body)))))
(define-macro (FCOMB2 var-lo var-hi i Y0-H Y1-H Y2-L Y3-L body)
  `(let ((,var-lo
          (fxxor
           (ref32-L RK ,i)
           (u16 (FSb-ref (fxand #xff ,Y3-L))
                (FSb-ref (fxarithmetic-shift-right ,Y2-L 8)))))
         (,var-hi
          (fxxor
           (ref32-H RK ,i)
           (u16 (FSb-ref (fxand #xff ,Y1-H))
                (FSb-ref (fxarithmetic-shift-right ,Y0-H 8)))))
         (,i
          (+ ,i 1)))
     ,body))

;; (define-syntax
;;   FROUND
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((i (list-ref form 1))
;;            (X0-L (list-ref form 2))
;;            (X0-H (list-ref form 3))
;;            (X1-L (list-ref form 4))
;;            (X1-H (list-ref form 5))
;;            (X2-L (list-ref form 6))
;;            (X2-H (list-ref form 7))
;;            (X3-L (list-ref form 8))
;;            (X3-H (list-ref form 9))
;;            (Y0-L (list-ref form 10))
;;            (Y0-H (list-ref form 11))
;;            (Y1-L (list-ref form 12))
;;            (Y1-H (list-ref form 13))
;;            (Y2-L (list-ref form 14))
;;            (Y2-H (list-ref form 15))
;;            (Y3-L (list-ref form 16))
;;            (Y3-H (list-ref form 17))
;;            (body (list-ref form 18)))
;;        `(FCOMB1 ,X0-L
;;                 ,X0-H
;;                 ,i
;;                 ,Y0-H
;;                 ,Y1-H
;;                 ,Y2-L
;;                 ,Y3-L
;;                 (FCOMB1 ,X1-L
;;                         ,X1-H
;;                         ,i
;;                         ,Y1-H
;;                         ,Y2-H
;;                         ,Y3-L
;;                         ,Y0-L
;;                         (FCOMB1 ,X2-L ,X2-H ,i ,Y2-H ,Y3-H ,Y0-L ,Y1-L (FCOMB1 ,X3-L ,X3-H ,i ,Y3-H ,Y0-H ,Y1-L ,Y2-L ,body))))))))
(define-macro (FROUND i
                      X0-L X0-H X1-L X1-H X2-L X2-H X3-L X3-H
                      Y0-L Y0-H Y1-L Y1-H Y2-L Y2-H Y3-L Y3-H
                      body)
  `(FCOMB1 ,X0-L ,X0-H ,i ,Y0-H ,Y1-H ,Y2-L ,Y3-L
   (FCOMB1 ,X1-L ,X1-H ,i ,Y1-H ,Y2-H ,Y3-L ,Y0-L
   (FCOMB1 ,X2-L ,X2-H ,i ,Y2-H ,Y3-H ,Y0-L ,Y1-L
   (FCOMB1 ,X3-L ,X3-H ,i ,Y3-H ,Y0-H ,Y1-L ,Y2-L
   ,body)))))


(define (aes-encrypt-ecb context input istart output ostart)
  (let ((RK (aes-context-erk context)))
    (get32
     X0-L X0-H input istart
     (get32
      X1-L X1-H input (+ istart 4)
      (get32
       X2-L X2-H input (+ istart 8)
       (get32
        X3-L X3-H input (+ istart 12)
        (xor32
         X0-L X0-H X0-L X0-H (ref32-L RK 0) (ref32-H RK 0)
         (xor32
          X1-L X1-H X1-L X1-H (ref32-L RK 1) (ref32-H RK 1)
          (xor32
           X2-L X2-H X2-L X2-H (ref32-L RK 2) (ref32-H RK 2)
           (xor32
            X3-L X3-H X3-L X3-H (ref32-L RK 3) (ref32-H RK 3)
            (let ((i 4))
              (FROUND i
                      Y0-L Y0-H Y1-L Y1-H Y2-L Y2-H Y3-L Y3-H
                      X0-L X0-H X1-L X1-H X2-L X2-H X3-L X3-H
                      (let loop ((i i)
                                 (k (- (aes-context-nr context) 2))
                                 (X0-L X0-L) (X0-H X0-H)
                                 (X1-L X1-L) (X1-H X1-H)
                                 (X2-L X2-L) (X2-H X2-H)
                                 (X3-L X3-L) (X3-H X3-H)
                                 (Y0-L Y0-L) (Y0-H Y0-H)
                                 (Y1-L Y1-L) (Y1-H Y1-H)
                                 (Y2-L Y2-L) (Y2-H Y2-H)
                                 (Y3-L Y3-L) (Y3-H Y3-H))
                        (if (> k 0)
                            (FROUND i
                                    X0-L X0-H X1-L X1-H X2-L X2-H X3-L X3-H
                                    Y0-L Y0-H Y1-L Y1-H Y2-L Y2-H Y3-L Y3-H
                                    (FROUND i
                                            Y0-L Y0-H Y1-L Y1-H Y2-L Y2-H Y3-L Y3-H
                                            X0-L X0-H X1-L X1-H X2-L X2-H X3-L X3-H
                                            (loop i
                                                  (- k 2)
                                                  X0-L X0-H
                                                  X1-L X1-H
                                                  X2-L X2-H
                                                  X3-L X3-H
                                                  Y0-L Y0-H
                                                  Y1-L Y1-H
                                                  Y2-L Y2-H
                                                  Y3-L Y3-H)))
                            (FCOMB2 X0-L X0-H i Y0-H Y1-H Y2-L Y3-L
                                    (FCOMB2 X1-L X1-H i Y1-H Y2-H Y3-L Y0-L
                                            (FCOMB2 X2-L X2-H i Y2-H Y3-H Y0-L Y1-L
                                                    (FCOMB2 X3-L X3-H i Y3-H Y0-H Y1-L Y2-L
                                                            (begin
                                                              (put32 X0-L X0-H output ostart)
                                                              (put32 X1-L X1-H output (+ ostart 4))
                                                              (put32 X2-L X2-H output (+ ostart 8))
                                                              (put32 X3-L X3-H output (+ ostart 12)))))))))))))))))))))

;;----------------------------------------------------------------------------

;;! Electronic Codebook (ECB) Mode Decryption.

;; (define-syntax RCOMB1
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((var-lo (list-ref form 1))
;;            (var-hi (list-ref form 2))
;;            (i (list-ref form 3))
;;            (Y0-H (list-ref form 4))
;;            (Y1-H (list-ref form 5))
;;            (Y2-L (list-ref form 6))
;;            (Y3-L (list-ref form 7))
;;            (body (list-ref form 8)))
;;        `(let ((,var-lo
;;                (fxxor (ref32-L RK ,i)
;;                       (fxxor (ref32-L RT0 (fxarithmetic-shift-right ,Y0-H 8))
;;                              (fxxor (ref32-L RT1 (fxand 255 ,Y1-H))
;;                                     (fxxor (ref32-L RT2 (fxarithmetic-shift-right ,Y2-L 8)) (ref32-L RT3 (fxand 255 ,Y3-L)))))))
;;               (,var-hi
;;                (fxxor (ref32-H RK ,i)
;;                       (fxxor (ref32-H RT0 (fxarithmetic-shift-right ,Y0-H 8))
;;                              (fxxor (ref32-H RT1 (fxand 255 ,Y1-H))
;;                                     (fxxor (ref32-H RT2 (fxarithmetic-shift-right ,Y2-L 8)) (ref32-H RT3 (fxand 255 ,Y3-L)))))))
;;               (,i (+ ,i 1)))
;;           ,body)))))
(define-macro (RCOMB1 var-lo var-hi i Y0-H Y1-H Y2-L Y3-L body)
  `(let ((,var-lo
          (fxxor
           (ref32-L RK ,i)
           (fxxor
            (ref32-L RT0 (fxarithmetic-shift-right ,Y0-H 8))
            (fxxor
             (ref32-L RT1 (fxand #xff ,Y1-H))
             (fxxor
              (ref32-L RT2 (fxarithmetic-shift-right ,Y2-L 8))
              (ref32-L RT3 (fxand #xff ,Y3-L)))))))
         (,var-hi
          (fxxor
           (ref32-H RK ,i)
           (fxxor
            (ref32-H RT0 (fxarithmetic-shift-right ,Y0-H 8))
            (fxxor
             (ref32-H RT1 (fxand #xff ,Y1-H))
             (fxxor
              (ref32-H RT2 (fxarithmetic-shift-right ,Y2-L 8))
              (ref32-H RT3 (fxand #xff ,Y3-L)))))))
         (,i
          (+ ,i 1)))
     ,body))

;; (define-syntax RCOMB2
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((var-lo (list-ref form 1))
;;            (var-hi (list-ref form 2))
;;            (i (list-ref form 3))
;;            (Y0-H (list-ref form 4))
;;            (Y1-H (list-ref form 5))
;;            (Y2-L (list-ref form 6))
;;            (Y3-L (list-ref form 7))
;;            (body (list-ref form 8)))
;;        `(let ((,var-lo (fxxor (ref32-L RK ,i) (u16 (RSb-ref (fxand 255 ,Y3-L)) (RSb-ref (fxarithmetic-shift-right ,Y2-L 8)))))
;;               (,var-hi (fxxor (ref32-H RK ,i) (u16 (RSb-ref (fxand 255 ,Y1-H)) (RSb-ref (fxarithmetic-shift-right ,Y0-H 8)))))
;;               (,i (+ ,i 1)))
;;           ,body)))))
(define-macro (RCOMB2 var-lo var-hi i Y0-H Y1-H Y2-L Y3-L body)
  `(let ((,var-lo
          (fxxor
           (ref32-L RK ,i)
           (u16 (RSb-ref (fxand #xff ,Y3-L))
                (RSb-ref (fxarithmetic-shift-right ,Y2-L 8)))))
         (,var-hi
          (fxxor
           (ref32-H RK ,i)
           (u16 (RSb-ref (fxand #xff ,Y1-H))
                (RSb-ref (fxarithmetic-shift-right ,Y0-H 8)))))
         (,i
          (+ ,i 1)))
     ,body))

;; (define-syntax RROUND
;;   (rsc-macro-transformer
;;    (lambda (form env)
;;      (let ((i (list-ref form 1)) (X0-L (list-ref form 2))
;;            (X0-H (list-ref form 3)) (X1-L (list-ref form 4))
;;            (X1-H (list-ref form 5)) (X2-L (list-ref form 6))
;;            (X2-H (list-ref form 7)) (X3-L (list-ref form 8))
;;            (X3-H (list-ref form 9)) (Y0-L (list-ref form 10))
;;            (Y0-H (list-ref form 11)) (Y1-L (list-ref form 12))
;;            (Y1-H (list-ref form 13)) (Y2-L (list-ref form 14))
;;            (Y2-H (list-ref form 15)) (Y3-L (list-ref form 16))
;;            (Y3-H (list-ref form 17)) (body (list-ref form 18)))
;;        `(RCOMB1 ,X0-L
;;                 ,X0-H
;;                 ,i
;;                 ,Y0-H
;;                 ,Y3-H
;;                 ,Y2-L
;;                 ,Y1-L
;;                 (RCOMB1 ,X1-L
;;                         ,X1-H
;;                         ,i
;;                         ,Y1-H
;;                         ,Y0-H
;;                         ,Y3-L
;;                         ,Y2-L
;;                         (RCOMB1 ,X2-L ,X2-H ,i ,Y2-H ,Y1-H ,Y0-L ,Y3-L
;;                                 (RCOMB1 ,X3-L ,X3-H ,i ,Y3-H ,Y2-H ,Y1-L ,Y0-L ,body))))))))
(define-macro (RROUND i
                      X0-L X0-H X1-L X1-H X2-L X2-H X3-L X3-H
                      Y0-L Y0-H Y1-L Y1-H Y2-L Y2-H Y3-L Y3-H
                      body)
  `(RCOMB1 ,X0-L ,X0-H ,i ,Y0-H ,Y3-H ,Y2-L ,Y1-L
   (RCOMB1 ,X1-L ,X1-H ,i ,Y1-H ,Y0-H ,Y3-L ,Y2-L
   (RCOMB1 ,X2-L ,X2-H ,i ,Y2-H ,Y1-H ,Y0-L ,Y3-L
   (RCOMB1 ,X3-L ,X3-H ,i ,Y3-H ,Y2-H ,Y1-L ,Y0-L
   ,body)))))

(define (aes-decrypt-ecb context input istart output ostart)
  (let ((RK (aes-context-drk context)))

    (get32 X0-L X0-H input istart
    (get32 X1-L X1-H input (+ istart 4)
    (get32 X2-L X2-H input (+ istart 8)
    (get32 X3-L X3-H input (+ istart 12)
    (xor32 X0-L X0-H X0-L X0-H (ref32-L RK 0) (ref32-H RK 0)
    (xor32 X1-L X1-H X1-L X1-H (ref32-L RK 1) (ref32-H RK 1)
    (xor32 X2-L X2-H X2-L X2-H (ref32-L RK 2) (ref32-H RK 2)
    (xor32 X3-L X3-H X3-L X3-H (ref32-L RK 3) (ref32-H RK 3)
    (let ((i 4))
      (RROUND i
              Y0-L Y0-H Y1-L Y1-H Y2-L Y2-H Y3-L Y3-H
              X0-L X0-H X1-L X1-H X2-L X2-H X3-L X3-H

      (let loop ((i i)
                 (k (- (aes-context-nr context) 2))
                 (X0-L X0-L) (X0-H X0-H)
                 (X1-L X1-L) (X1-H X1-H)
                 (X2-L X2-L) (X2-H X2-H)
                 (X3-L X3-L) (X3-H X3-H)
                 (Y0-L Y0-L) (Y0-H Y0-H)
                 (Y1-L Y1-L) (Y1-H Y1-H)
                 (Y2-L Y2-L) (Y2-H Y2-H)
                 (Y3-L Y3-L) (Y3-H Y3-H))
        (if (> k 0)

            (RROUND i
                    X0-L X0-H X1-L X1-H X2-L X2-H X3-L X3-H
                    Y0-L Y0-H Y1-L Y1-H Y2-L Y2-H Y3-L Y3-H
            (RROUND i
                    Y0-L Y0-H Y1-L Y1-H Y2-L Y2-H Y3-L Y3-H
                    X0-L X0-H X1-L X1-H X2-L X2-H X3-L X3-H
            (loop i
                  (- k 2)
                  X0-L X0-H
                  X1-L X1-H
                  X2-L X2-H
                  X3-L X3-H
                  Y0-L Y0-H
                  Y1-L Y1-H
                  Y2-L Y2-H
                  Y3-L Y3-H)))

            (RCOMB2 X0-L X0-H i Y0-H Y3-H Y2-L Y1-L
            (RCOMB2 X1-L X1-H i Y1-H Y0-H Y3-L Y2-L
            (RCOMB2 X2-L X2-H i Y2-H Y1-H Y0-L Y3-L
            (RCOMB2 X3-L X3-H i Y3-H Y2-H Y1-L Y0-L
            (begin
              (put32 X0-L X0-H output ostart)
              (put32 X1-L X1-H output (+ ostart 4))
              (put32 X2-L X2-H output (+ ostart 8))
              (put32 X3-L X3-H output (+ ostart 12)))))))))))))))))))))

;;------------------------------------------------------------------------------

;;! Cipher Block Chaining (CBC) Mode Encryption.
(define (aes-encrypt-cbc context input istart output ostart iv nb-blocks)
  (let loop1 ((i 0))
    (if (< i nb-blocks)
        (let* ((x (* i block-size))
               (ipos (+ istart x))
               (opos (+ ostart x)))
          (let loop2 ((k (- block-size 1)))
            (if (>= k 0)
                (begin
                  (u8vector-set!
                   iv
                   k
                   (fxxor
                    (u8vector-ref iv k)
                    (u8vector-ref input (+ ipos k))))
                  (loop2 (- k 1)))))
          (aes-encrypt-ecb context iv 0 iv 0)
          (subu8vector-move!
           iv
           0
           block-size
           output
           opos)
          (loop1 (+ i 1)))
        iv)))

;;------------------------------------------------------------------------------

;;! Cipher Block Chaining (CBC) Mode Decryption.
(define (aes-decrypt-cbc context input istart output ostart iv nb-blocks)
  (let loop1 ((i 0))
    (if (< i nb-blocks)
        (let* ((x (* i block-size))
               (ipos (+ istart x))
               (opos (+ ostart x)))
          (aes-decrypt-ecb context input ipos output opos)
          (let loop2 ((k (- block-size 1)))
            (if (>= k 0)
                (begin
                  (u8vector-set!
                   output
                   (+ opos k)
                   (fxxor
                    (u8vector-ref iv k)
                    (u8vector-ref output (+ opos k))))
                  (loop2 (- k 1)))))
          (subu8vector-move!
           input
           ipos
           (+ ipos block-size)
           iv
           0)
          (loop1 (+ i 1)))
        iv)))

;;;----------------------------------------------------------------------------

(define* (aes-encrypt-subu8vector
          u8vect
          start
          end
          key
          (iv (make-u8vector block-size 0)))
  (let* ((context (u8vector->aes-context key))
         (len (- end start))
         (nb-blocks (quotient len block-size))
         (output (make-u8vector len)))
    (aes-encrypt-cbc context u8vect start output 0 iv nb-blocks)
    output))

(define* (aes-decrypt-subu8vector
          u8vect
          start
          end
          key
          (iv (make-u8vector block-size 0)))
  (let* ((context (u8vector->aes-context key))
         (len (- end start)))
    (if (not (= 0 (modulo len block-size)))
        (error "length must be a multiple of 16")
        (let* ((nb-blocks (quotient len block-size))
               (output (make-u8vector len)))
          (aes-decrypt-cbc context u8vect start output 0 iv nb-blocks)
          output))))

(define* (aes-encrypt-u8vector
          u8vect
          key
          (iv (make-u8vector block-size 0)))
  (aes-encrypt-subu8vector u8vect 0 (u8vector-length u8vect) key iv))

(define* (aes-decrypt-u8vector
          u8vect
          key
          (iv (make-u8vector block-size 0)))
  (aes-decrypt-subu8vector u8vect 0 (u8vector-length u8vect) key iv))

