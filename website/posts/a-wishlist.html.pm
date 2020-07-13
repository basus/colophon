#lang pollen

◊(define-meta title "A Wishlist"
              started "2020-07-13"
              published "2020-07-13"
)

A few days ago, ◊a["https://cs.brown.edu/~sk/"]{Prof. Shriram Krishnamurthi}
started writing on ◊a["https://parentheticallyspeaking.org/about/"]{a website}
using his own Racket-based website generator, including a detailed
◊a["https://parentheticallyspeaking.org/about/"]{About} page. I particularly
liked the section about why the site is not a blog, which includes some thoughts
about how he wants the site to develop. That got me thinking that I should write
down what I want from Colophon. I'll try to update this post as and when the
particular features are implemented (or discarded).

◊more{}

In no particular order:

◊ul{

◊li{ ◊b{Code snippets.}

◊a["basic-syntax-highlighting.html"]{Basic syntax highlighting} is a start, but
since I'd like to use Colophon to write technical articles, there's a lot more
I'd like to add. I'd like to have a button to copy code to the clipboard. I'd
also like to link intelligently to GitHub (or some other code hosting), though
I'm not sure what that looks like. I can also imagine compiling some snippets to
◊a["https://webassembly.org"]{WebAssembly} and then running the result in the
browser, though that might take a lot of work. }

◊li{ ◊b{Powerful Collections and Indexes.}

This deserves its own post, but I want Colophon to support more than the
traditional, reverse-chronological, blog format. That format has a recency bias
that I only think is useful if your site really is a ◊em{web log}. Otherwise, it
should be possible to collect and display writing in a variety of different
ways. This implies some sort of organization systems (tags, categories,
collections, series, or multiple of these). However, since Colophon should be as
static as possible, I also want to avoid regenerating indexes every time
something changes.}

◊li{ ◊b{Tracking Changes and Revisions.}

Related to the above, Colophon should support more deliberative writing and that
means more than just tracking draft and publication status. Ideally, Colophon
would plug into some kind of standard version control system, and allow readers
to look back through versions of an article. Prior to that, there should be a
way to mark changes, provide change summaries, and have them show up in the
output in an easily identifiable, non-intrusive way.}

◊li{ ◊b{Notes and Asides.}

Revision notes are just one of a variety of notes and asides that a Colophon
author might include as part of their posts. These notes should be displayed in
a way that makes them easy to read, but is also unobtrusive. I think
◊a["https://ilovetypography.com/2020/07/11/black-print-first-african-america-printer-publishers/"]{I
Love Typography} makes very good use of sidenotes (as well as
◊a["https://edwardtufte.github.io/tufte-css/"]{Tufte CSS} and its derivatives).
Tooltip notes, like what ◊a["https://marco.org"]{Marco Arment} uses, would also
be a good alternative, especially on small screens (perhaps even to the point of
justifying some JavaScript). What I do ◊em{not} want is note markers as links
that take readers to the actual note at the bottom of the screen, away from the
relevant text. }

◊li{ ◊b{Intelligent Excerpts.}

One of my pet peeves is how bad the excerpts are for some popular publishing
platforms. At the very least, an excerpt should not cut off in the middle of a
sentence. Colophon should support better excerpts, both for the purpose of
sharing on social media plaforms, but also for generating readable indexes. I'm
undecided on the mechanism. Colophon currently supports a ◊code{more} tag that
pulls out preceding text as an excerpt, and a ◊code{description} field in an
article's metadata. Supporting a ◊code{excerpt} tag in the article's text seems
reasonable, and I need to figure out something intelligent to do when the author
doesn't specify an excerpt. }

◊li{ ◊b{Rich Hyperlinks.}

In my opinion, most writing on the web doesn't use hyperlinks nearly as much as
they should. Colophon should encourage lots of rich hyperlinks, both internally
to other posts on the same site, and externally to other sites. Part of this is
presentational (changing the styling or using icons for certain types of links),
and some of it is book-keeping (generating paragraph-level permalinks for
specific referencing, or automatically linking certain phrases). Those are just
two ideas, there are probably other interesting things I'll come up with over
time. }

◊li{ ◊b{RSS support.}

Yes, RSS still exists, and it's still my preferred way to read blogs. I will
probably use the ◊a["https://nullprogram.com/blog/2013/09/23/"]{better Atom
format} (rather than RSS 2.0) and implement a feed generator in the future.
However, given that Colophon's goal is to explicitly be for non-blog-like
websites, I need to think about how to do this properly. Instead of republishing
an entire article to the feed whenever something changes, maybe only a change
summary should be published? This probably also affects indexes in some way I
haven't thought about yet. }

◊li{ ◊b{A Theming System.}

I like pretty things, especially when they match. For now, I'm mainly concerned
with color themes for different page elements. One particular thing I want is to
support theme ◊em{variants}, so that a theme could have, say, light and dark
variants. A basic version of this exists already, but would require some clean
up and streamlining. I'm not sure if I want this to be exposed to the reader as
an option, or only be configurable at build time. Thinking ahead, CSS now
supports variables, it may be possible to support different fonts and other
niceties. }

}

I don't have a particular order in which I'd like these things to be
implemented. In fact, most of them will probably be implemented piecemeal, and
rebuilt several times before I get to a version I'm happy with. Colophon is
going to be in experimental status for a long time (perhaps indefinitely), but
I'll scratch particular itches whenever they get itchy enough.