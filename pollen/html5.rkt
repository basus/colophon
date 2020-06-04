#lang racket

(require racket/list racket/string
         sugar/define
         txexpr xml/path
         pollen/core )

;; Meta tags
(module meta racket
  (provide (all-defined-out))

  (define (charset [ch "utf-8"])
    `(meta ((charset ,ch))))

  (define (http-equiv [type "content-type"] [content "text/html; charset=UTF-8"])
    `(meta ((http-equiv ,type) (content ,content))))

  (define (name name content)
    `(meta ((name ,name) (content ,content))))

  (define (application-name name)
    (name "application-name" name))

  (define (author name)
    (name "author" name))

  (define (description desc)
    (name "description" desc))

  (define (generator [gen "Racket + Pollen + Colophon"])
    (name "generator" gen ))

  (define (keywords kws)
    (cond
      [(string? kws) (name "keywords" kws)]
      [(list? kws) (name (string-join kws ", "))]))

  (define (viewport [content "width=device-width, initial-scale=1.0"])
    (name "viewport" content))

  (define (defaults)
    (list (charset) (viewport) (generator))))

(require 'meta)
(provide (prefix-out meta: (all-from-out 'meta)))

;; Tags that can go in the head section
(module head racket
  (require pollen/core)
  (provide (all-defined-out))

  (define (title [str (select-from-metas 'title (current-metas))])
    `(title ,str))

  (define (stylesheet url)
    `(link ((href ,url) (rel "stylesheet"))))

  (define (stylesheets . urls)
    (map stylesheet urls))

)
(require 'head)
(provide (prefix-out head: (all-from-out 'head)))

(define+provide (a url . text)
  `(a ((href ,url)) ,@text))
