#lang racket

(require racket/format racket/path
         txexpr
         colophon/common colophon/components colophon/predicates
         pollen/cache pollen/core pollen/decode pollen/file)

(require "../pollen.rkt")

(module setup racket/base
  (provide (all-defined-out))
  (require pollen/setup)
  (define block-tags (cons 'more default-block-tags)))

(provide (all-defined-out))
(provide (all-from-out "../pollen.rkt"))
