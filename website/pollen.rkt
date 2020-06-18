#lang racket

(require (only-in markdown parse-markdown)
         (prefix-in link: colophon/link)
         racket/string
         txexpr xml/path
         colophon/common
         pollen/core pollen/decode pollen/template)

(define (root . items)
  (define (clean-break e) (decode-linebreaks e "\n"))
  (define (breakless-paragraphs items)
    (decode-paragraphs items 'p #:linebreak-proc clean-break))

  (define remove-more
    (match-lambda
      [(txexpr more-tag _ _) '()]
      [tx (wrap-hanging-quotes tx)]))

  (decode (txexpr 'root '() items)
          #:txexpr-elements-proc breakless-paragraphs
          #:block-txexpr-proc wrap-hanging-quotes
          #:string-proc (compose1 smart-quotes smart-dashes)
          #:exclude-tags '(style script)))

(define not-more? (λ (x) (and (txexpr? x) (not (equal? 'more (get-tag x))))))

(define (remove-more doc)
  (let*-values
      ([(content) (select* 'root doc)]
       [(body) (filter not-more? content)])
    body))

(define (md . text)
  `(section ,@(parse-markdown (string-join text " " ))))

(define (navigation name . entries)
  (define first (link:title name) )
  (define links
    (map link:directory entries))
  `(nav (ul ,first ,@links)))

;; Default code snippets, mostly for inclusion in templates
(define (head-with #:title [title (head:title)]
                   #:fonts [fonts "/css/fonts.css"]
                   #:style [style "/css/style.css"]
                   #:theme [theme "/css/theme.css"])
  `(head
    ,@(meta:defaults)
    ,title
    ,@(head:stylesheets fonts style theme)))

(define (default-head) (head-with))

(define (default-navigation)
  `(header ,(navigation "Colophon" "Posts" "Drafts" "Series")))

(define (body-with #:id [id #f]
                   #:theme-variant [thvar #f]
                   #:class [cls #f]
                   #:navigation [nav (default-navigation)]
                   #:contents contents)
  (let* ((th (if thvar (string-append "theme-" thvar) #f))

         (clss (match* (cls th)
                [ [#f #f] #f ]
                [ [#f th] th ]
                [ [cl #f] cl ]
                [ [cl th] (string-join (list th cl) " ")]))

        (attrs (match* (id clss)
                 [ [#f #f] '() ]
                 [ [id #f] `((id ,id)) ]
                 [ [#f clss] `((class ,clss)) ]
                 [ [id clss] `((class ,clss) (id ,id)) ]
                 )))
  `(body ,attrs ,nav ,contents)))

(provide (all-defined-out))

(require pollen/html5)
(provide (all-from-out pollen/html5))

(require colophon/components)
(provide (all-from-out colophon/components))
