#lang info

(define version "0.1")
(define collection 'multi)

(define deps '(["base" #:version "6.3"]
               ["pollen" #:version "2.2"]))
(define build-deps '("scribble-doc"
                     "racket-doc"
                     "scribble-lib"))
