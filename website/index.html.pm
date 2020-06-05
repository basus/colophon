#lang pollen
◊(define-meta title "Colophon" )

◊h1{Colophon: Experiments in Static Site Generation}

Colophon is a ◊em{very experimental} static site generator built atop the
 ◊a["https://racket-lang.org"]{Racket programming language} and the
 ◊a["https://pollenpub.com"]{Pollen publishing system}.

Colophon's goal is to be a tool for ◊em{thinking and writing}, as well as for
publication. This is in contrast to other static site generators like
◊a["https://github.com/greghendershott/frog"]{Frog},
◊a["https://jekyllrb.com/"]{Jekyll}, or ◊a["https://gohugo.io/"]{Hugo}, where
the focus is on publication, rather than creation. Colophon is inspired by the
work of ◊a["https://joeldueck.com/"]{Joel Dueck}, epecially
◊a["https://thelocalyarn.com/code/doc/trunk/repo-www/home.wiki"]{◊em{ The Local
Yarn} }.

◊md{
Colophon currently has the following components:

- Libraries providing convenience functions to write documents in Pollen. Mainly
  this includes functions that implement a growing subset of HTML5 tags in an
  intelligent, Racket-friendly manner.

- Colophon itself. This is Racket code that implements features of a static site
  (for example, generating index pages for posts and tags). Much of this code
  depends on Pollen, or at least
  [tagged X-expressions](https://docs.racket-lang.org/txexpr/index.html).

- A website, built with Pollen and Colophon. This is an example of Colophon in
  action, used to test features and guide development.

}

◊h1{Recent}

◊(make-index "posts/")
