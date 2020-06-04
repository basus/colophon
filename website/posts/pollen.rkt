#lang racket

(require racket/format
         txexpr xml/path
          pollen/core pollen/decode )
(require "../pollen.rkt")

;; Generate the top-matter. For posts, this is the title, subtitle, and relevant
;; dates (started, modification and publication).
(define (make-top metas)
  (let* ((title    (select-from-metas 'title metas))
        (subtitle  (select-from-metas 'subtitle metas))
        (started   (select-from-metas 'started metas))
        (published (select-from-metas 'published metas))
        (modified  (select-from-metas 'modified metas))

        (h1
         (if title `(h1 ((id "title")) ,title) "" ))
        (h2
         (if subtitle `(h2 ((id "subtitle")) ,subtitle) ""))
        (start-msg
         (if started (format "This post was started on ~a." started) ""))
        (modified-msg
         (if modified (format "It was last modified on ~a." modified) ""))
        (published-msg
         (if published (format "It was published on ~a." modified)
             "This post is an unpublished draft.")))

    `(,h1
      ,h2
      (p ((id "meta"))
         ,(string-join (list start-msg modified-msg published-msg)))
      (hr)
      )))

(provide (all-defined-out))
(provide (all-from-out "../pollen.rkt"))
