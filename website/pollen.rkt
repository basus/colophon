#lang racket

(require (only-in markdown parse-markdown)
         (prefix-in link: colophon/link)
         (prefix-in tag: colophon/tags)
         colophon/predicates colophon/components
         racket/string
         txexpr
         pollen/core pollen/decode )

(define (navbar)
  `(header ,(make-navigation "Colophon" "Posts" "Drafts" "Series")))

(provide (all-defined-out))

(require pollen/html5)
(provide (all-from-out pollen/html5))

(require colophon/components)
(provide (all-from-out colophon/components))

(require colophon/tags)
(provide (all-from-out colophon/tags))
