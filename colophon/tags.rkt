#lang racket

(require (only-in markdown parse-markdown)
         pollen/core pollen/decode pollen/unstable/pygments
         txexpr
         "predicates.rkt")

;; This file contains 2 things:
;;
;; 1. tag functions that aren't a part of the HTML5
;;    specification but are useful for writing Pollen documents.
;; 2. Utility functions for dealing with tags


;; Useful Tag Functions

(define (b . text)
  `(strong ,@text))

(define (codeblock #:line-numbers? [line-numbers? #f]
                   lang . lines)
  (apply highlight
         #:python-executable "python3"
         #:line-numbers? line-numbers?
         lang lines)
  )

(define more-tag 'more)

(define (md . text)
  `(section ,@(parse-markdown (string-join text " " ))))

(define (root . items)
  (define (clean-break e) (decode-linebreaks e "\n"))
  (define (breakless-paragraphs items)
    (decode-paragraphs items 'p #:linebreak-proc clean-break))

  (decode (txexpr 'root '() items)
          #:txexpr-elements-proc breakless-paragraphs
          #:block-txexpr-proc wrap-hanging-quotes
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:exclude-tags '(style script)))

;; Utility Functions on Tags

(define (remove-tag t txs)
  (filter (not-tag? t) txs))


(provide (all-defined-out))
