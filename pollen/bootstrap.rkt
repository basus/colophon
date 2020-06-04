#lang racket

;; Bootstrap related functions
(provide (all-defined-out))
(define (valid-column num)
  (if (and (>= num 0) (<= num 12))
      num
      (raise
       (format "~a is not a valid column number. Must be between 0 and 12." num)
       #t)))

(define (xs num)
  (format "col-xs-~a" (valid-column num)))
(define (sm num)
  (format "col-sm-~a" (valid-column num)))
(define (md num)
  (format "col-md-~a" (valid-column num)))
(define (lg num)
  (format "col-lg-~a" (valid-column num)))

(define (columns a b c d)
  (string-join (list (xs a) (sm b) (md c) (lg d)) " "))

(define (xs-off num)
  (format "offset-xs-~a" (valid-column num)))
(define (sm-off num)
  (format "offset-sm-~a" (valid-column num)))
(define (md-off num)
  (format "offset-md-~a" (valid-column num)))
(define (lg-off num)
  (format "offset-lg-~a" (valid-column num)))

(define (offsets a b c d)
  (string-join (list (xs-off a) (sm-off b) (md-off c) (lg-off d)) " "))

;; Some standard column settings
(define centered
  (string-join (list (columns 12 12 8 8) "mx-auto") " "))
(define full (columns 12 12 12 12))
(define three-quarters (columns 12 9 9 9))
(define two-thirds (columns 12 8 8 8))
(define half (columns 12 6 6 6))
(define one-third (columns 4 4 4 4))
(define one-quarter (columns 12 3 3 3))
(define small-left (columns 12 4 4 4))
(define small-right (columns 12 8 8 8))
