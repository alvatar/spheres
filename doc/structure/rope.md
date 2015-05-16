rope
====
This is a straightforward Scheme implementation of Ropes.

It's been built & tested on CHICKEN, Chibi, Gauche & Gambit, but should
be easy to get working in any R5/R7RS-like implementation (use the
`rope.sld` and `rope-r7rs.scm` files as a base).

Installation
------------
To install for CHICKEN, use `chicken-install`:

    $ git clone git://bitbucket.org/evhan/rope.git
    $ cd rope
    $ chicken-install -test

API
---
    empty-rope ; rope
    
    (current-maximum-leaf-length [integer]) ; parameter
    
    (rope [string ...]) ; => rope
    (string->rope string) ; => rope
    (rope->string rope) ; => string
    
    (rope? object) ; => boolean
    (rope=? rope1 rope2 [ropeN ...]) ; => boolean
    (rope-null? rope) ; => boolean
    (rope-length rope) ; => integer
    
    (rope-ref rope i) ; => character
    (subrope rope start [end]) ; => rope
    
    (rope-append [rope ...]) ; => rope
    (rope-concatenate list) ; => rope
    (rope-reverse rope) ; => rope
    
    (rope-depth rope) ; => integer
    (rope-balanced? rope) ; => boolean
    (rope-balance rope) ; => rope
    
    (rope-fold f a rope1 [ropeN ...]) ; => object
    (rope-for-each f rope1 [ropeN ...]) ; => void
    
    (read-rope [port [length]]) ; => rope
    (make-rope-iterator rope) ; => procedure
    (open-input-rope rope) ; => port
    (open-output-rope) ; => port
    (get-output-rope [port]) ; => rope

References
----------
* *Ropes, An Alternative to Strings*  
  H. Boehm, R. Atkinson, M. Plass  
  Software Practice and Experience 25, Dec 1995, pp. 1315-1330
* CL-Rope  
  Peter Gijsels  
  <http://common-lisp.net/project/cl-rope/>
* rope.plt  
  Danny Yoo  
  <http://planet.racket-lang.org/display.ss?package=rope.plt&owner=dyoo>
* SGI Rope Implementation  
  <http://www.sgi.com/tech/stl/Rope.html>  
  <http://www.sgi.com/tech/stl/ropeimpl.html>
* data-rope  
  Pierre-Etienne Meunier  
  <http://hackage.haskell.org/packages/archive/data-rope/latest/doc/html/Data-Rope.html>

Author
------
Evan Hanson <evhan@foldling.org>

License
-------
BSD. See LICENSE for details.
