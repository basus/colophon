#lang racket

(require colophon/components)

(define (navbar)
  `(header ,(make-navigation "Colophon" "Posts" "Drafts" "Series")))

(provide (all-defined-out))

(require pollen/html5)
(provide (all-from-out pollen/html5))

(provide (all-from-out colophon/components))

(require colophon/tags)
(provide (all-from-out colophon/tags))
