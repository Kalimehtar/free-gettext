#lang info
(define collection "gettext")
(define deps '("base" "srfi-lite-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/gettext.scrbl" ())))
(define pkg-desc "Independent implementation of GNU gettext with BSD license")
(define version "1.10")
(define pkg-authors '(kalimehtar))
