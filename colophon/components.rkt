#lang racket

(require txexpr
         pollen/cache pollen/core pollen/file pollen/html5
         (prefix-in tag: "tags.rkt")
         (prefix-in link: "link.rkt")
         "predicates.rkt")

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
         (if published (format "It was published on ~a." published)
             "This post is an unpublished draft.")))

    `(,h1
      ,h2
      (p ((id "meta"))
         ,(string-join (list start-msg modified-msg published-msg)))
      (hr)
      )))

(define (make-summary metas doc output-path
                      #:break-tag [break-tag tag:more-tag])
  (let* ((title     (select-from-metas 'title metas))
         (subtitle  (select-from-metas 'subtitle metas))
         (started   (select-from-metas 'started metas))
         (published (select-from-metas 'published metas))
         (modified  (select-from-metas 'modified metas))
         (excerpt   (select-from-metas 'excerpt metas))
         (post      (path->string output-path))

         (title-txt
          (if title `(h2 ((class "post-title"))
                         (a ((href ,post)) ,title)) ""))
         (start-txt
          (if started (format "Started on ~a." started) ""))
         (modified-txt
          (if modified (format "Last modified on ~a." modified) ""))
         (published-txt
          (if published (format "Published on ~a." published) ""))
         (excerpt-paras
          (if excerpt excerpt (make-excerpt doc)))
         (meta-txt
          `(p ((class "post-meta"))
             ,(string-join (list start-txt modified-txt published-txt))))
         (more
          `(a ((href ,post) (class "post-more")) "Read more →"))
         )

    `(div ((class "post-summary"))
          ,title-txt
          ,meta-txt
          ,@excerpt-paras
          ,more
          (hr))
    ))

(define (make-excerpt doc #:break-tag [break-tag tag:more-tag])
  (takef (get-elements doc)
         (not-tag? break-tag)))

;; Generate an index of posts. This relies on the Pollen cache but should be
;; made more intelligent later.
(define (make-index d)
  (define (summarize f)
    (let* ([source-path f]
           [metas (cached-metas source-path)]
           [doc (cached-doc source-path)]
           [output-path (->output-path source-path)] )
      (make-summary metas doc output-path)))

  (define (sort-by-modification p1 p2)
    (> (file-or-directory-modify-seconds	p1)
       (file-or-directory-modify-seconds	p2) ))

  (let* ((unsorted-files (filter indexable-source? (directory-list d #:build? #t)))
         (sorted-files (sort unsorted-files sort-by-modification))
         (summaries (map summarize sorted-files)) )
    `(section ,@summaries))
  )

;; Generate a navigation bar
(define (make-navigation name . entries)
  (define first (link:title name) )
  (define links
    (map link:directory entries))
  `(nav (ul ,first ,@links)))

(define (default-navigation)
  `(header ,(make-navigation "Colophon" "Dir1" "Dir2" "Dir3")))

;; Generate the <head> tag
(define (head-with #:title [title (head:title)]
                   #:fonts [fonts "/css/fonts.css"]
                   #:style [style "/css/style.css"]
                   #:theme [theme "/css/theme.css"])
  `(head
    ,@(meta:defaults)
    ,title
    ,@(head:stylesheets fonts style theme)))

(define (default-head) (head-with))

;; Generate the <body> tag
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
