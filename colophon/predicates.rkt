#lang racket

(require racket/path txexpr pollen/file)

(define (is-tag? t)
  (λ (x) (and (txexpr? x) (equal? t (get-tag x)))))

(define (not-tag? t)
  (λ (x) (and (txexpr? x) (not (equal? t (get-tag x))))))

(define (prefix-path? path prefix)
  (string-prefix?
   (string-downcase (path->string (file-name-from-path path)))
   prefix ))

(define (pollen-source? path)
  (or (preproc-source? path)
      (markup-source? path)
      (markdown-source? path)
      (null-source? path)))

(define (indexable-source? path)
  (and (file-exists? path)
       (pollen-source? path)
       (not (prefix-path? path "index"))
       (not (prefix-path? path "template"))))

(provide (all-defined-out))
