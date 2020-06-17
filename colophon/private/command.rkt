#lang racket/base

(require racket/cmdline)

(module+ raco
  (define command-name (with-handlers ([exn:fail? (lambda (exn) #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name))

(define current-server-port (make-parameter 8080))
(define current-output-dir-name (make-parameter "_site"))
(define template (make-parameter null))

(define (dispatch command-name)
  (let ((rest (cdr current-command-line-arguments)))
        (case command-name
          [(#f "help") (handle-help)]
          [("render")  (handle-render rest)]
          [("build")   (handle-build)]    ; Build the current project, parses its own args
          [("start")   (handle-start)]
          [("cache")   (handle-cache)]
          [("version") (handle-version)]
          [else (handle-unknown command-name)])
        ))

(define (handle-help)
  (displayln
   (format "Colophon commands:
help                   Show this message.
render path ...        Render one or more source filepaths.
build [src]  [dst]     Build the source directory and place it at dst.
                          The default source is the current directory.
                          The default destination is ~a.
start [dir] [port]     Starts project server in dir. The default is current dir.
                          The default port is ~a.
cache                  Preload the cache.
version                Print the version."
           current-output-dir-name current-server-port )))

(define (handle-build)
  ;; (define-values (input output)
  ;;   (command-line
  ;;    #:program "colophon build"
  ;;    #:once-each
  ;;    [("-t" "--template") t
  ;;                         "Render the input file with the given template"
  ;;                         (template t)]
  ;;    #:args ( )
  ;;    ))

  (displayln "Colophon does not currently support building projects."))

(define (handle-render)
  (displayln "Colophon does not currently support rendering files."))

(define (handle-version)
  (displayln "Colophon is installed correctly."))

(define (handle-start)
  (displayln "Colophon does not currently have a project server."))

(define (handle-cache)
  (displayln "Colophon does not currently have a cache."))

(define (handle-unknown command)
  (displayln (format "`~a` is an unknown command." command))
  (display "These are the available commands.")
  (handle-help)
  (exit 1))

