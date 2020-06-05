#lang racket

(require (only-in markdown parse-markdown)
         (prefix-in link: colophon/link)
         racket/string
         txexpr xml/path
         pollen/core pollen/decode pollen/template)

(define (root . items)
  (define (clean-break e) (decode-linebreaks e "\n"))
  (define (breakless-paragraphs items)
    (decode-paragraphs items 'p #:linebreak-proc clean-break))

  (decode (txexpr 'root '() items)
          #:txexpr-elements-proc breakless-paragraphs
          #:block-txexpr-proc wrap-hanging-quotes
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:exclude-tags '(style script)))

(define (md . text)
  `(section ,@(parse-markdown (string-join text " " ))))

(define (navigation name . entries)
  (define first (link:title name) )
  (define links
    (map link:directory entries))
  `(nav (ul ,first ,@links)))

;; Default code snippets, mostly for inclusion in templates
(define (default-head)
  `(head
    ,@(meta:defaults)
    ,(head:title)
    ,@(head:stylesheets "/css/fonts.css" "/css/style.css" "/css/gruvbox.css")))

(define (default-navigation)
  `(header ,(navigation "Colophon" "Posts" "Drafts" "Series")))

(define (body-with #:id [id #f]
                   #:class [cls #f]
                   #:navigation [nav (default-navigation)]
                   #:contents contents)
  (let ((attrs (match* (id cls)
                 [ [#f #f] '() ]
                 [ [id #f] `((id ,id)) ]
                 [ [#f cls] `((class ,cls)) ]
                 [ [id cls] `((class ,cls) (id ,id)) ]
                 )))
  `(body ,attrs ,nav ,contents)))

(provide (all-defined-out))

(require pollen/html5)
(provide (all-from-out pollen/html5))

(require colophon/components)
(provide (all-from-out colophon/components))
