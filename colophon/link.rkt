#lang racket

(require pollen/core)

(define (directory d)
  (let* (
         [name (string-downcase d)]
         [href (format "/~a/" name)]
         [here-path (select-from-metas 'here-path (current-metas))]
         [attrs (if (member name (explode-path here-path))
                    `((href ,href) (class "current") )
                    `((href ,href) ))])
    `(li (a ,attrs ,name))))

(define (title name)
  `(li (a ((href "/")) (strong ,(string-downcase name)))))

(provide (all-defined-out))
