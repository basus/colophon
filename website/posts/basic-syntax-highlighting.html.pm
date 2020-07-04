#lang pollen

◊(define-meta title "Basic Syntax Highlighting"
              started "2020-06-29"
              published "2020-06-29"
)

One of the big reasons I'm diving into the "write your website generator"
rabbit-hole is that I can have posts with nice syntax-highlighted blocks of code
without onerous copy-pasting between ◊a["https://spacemacs.org"]{Spacemacs} and
WordPress (or some other blogging platform), and with the flexbility to add
better styling or other features to the code in question. So for today, we'll
try to set up syntax highlighting for code blocks in Colophon.

◊more{}

Luckily for us, ◊a["https://pollenpub.com"]{Pollen} (on which Colophon is based)
already has ◊a["https://docs.racket-lang.org/pollen/mini-tutorial.html"]{support
for syntax highlighting}, either via ◊a["http://pygments.org/"]{Pygments} or
◊a["https://highlightjs.org/"]{Highlight.js}. We want Colophon to produce
static, self-contained HTML (as much as possible), so Pygments is the better
choice.

To start off, we follow the
◊a["https://docs.racket-lang.org/pollen/mini-tutorial.html"]{Pollen
instructions} and install Python 3 and Pygments. I'm on a Mac and macOS Catalina
ships with an older Python 2.7. I used ◊a["https://brew.sh"]{Homebrew} to
install Python 3 and then used ◊a["https://pypi.org/project/pip/"]{pip} to
install Pygments. I aliased the bare commands to the Python 3 versions, and made
sure that the ◊code{pygmentize} utility was on my ◊code{PATH}:

◊codeblock['bash]{
    # Set up Python3 using aliases
    alias python=$(which python3)
    alias pip=$(which pip3)
    PATH=$PATH:$HOME/Library/Python/3.7/bin
}

Unfortunately, while this sets up Python 3 and Pygments for use from a shell, it
takes a little more work to get it working with Pollen---◊code{alias} commands
won't work outside of a shell, and the Pollen support for Pygments doesn't use
the ◊code{pygmentize} utility.

But once again, Pollen gives us an out: the Pollen helper code provides a
◊a["https://docs.racket-lang.org/pollen/Pygments.html#%28def._%28%28lib._pollen%2Funstable%2Fpygments..rkt%29._highlight%29%29"]{◊code{highlight}}
function that takes a ◊code{python-executable} argument which can point to the
correct version of Python. I could provide the value ◊code{"python3"} for that
argument every time I write out a code block, but that would get repetitive and
clunky, just the thing we want to avoid. Instead, I wrote a basic
◊a["https://docs.racket-lang.org/pollen/programming-pollen.html"]{tag function}
that sets the ◊code{python-executable} and passes on everything else to the
underlying ◊code{highlight} function:

◊codeblock[#:line-numbers? #t 'racket]{
(define (codeblock #:line-numbers? [line-numbers? #t]
                   lang . lines)
  (apply highlight
         #:python-executable "python3"
         #:line-numbers? line-numbers?
         lang lines)
  )
}

I provided ◊code{#:line-numbers?} argument so that individual code blocks can
decide whether or not to show line numbers. The ◊code{highlight} function also
has arguments for providing an outer CSS class and a set of lines to highlight.
I can expose those later if needed.

With that in place, Pollen and Pygments together turn the code blocks into HTML
with different parts for different pieces of syntax. The last remaining thing is
to add some CSS so that the HTML is colored and styled correctly. Pygments
supports a bunch of different styles and will generate CSS for a particular
style with the following incantation:

◊codeblock['bash]{
  pygmentize -S <style name> -f html -a .highlight
}

I'm also working on a theming system for Colophon (spoilers!), but for now,
pasting into the resulting CSS into the style file is good enough.

Bringing everything together, basic syntax highlighting works. In fact, this
post uses the ◊code{codeblock}s to provide the highlighted snippets above. There
are a few rough edges. I'd like to bring Pygment styles into the theming system
I'm planning for Colophon. Also, because of the way the Pollen wrapper around
Pygment works, line numbers can't be toggled on a per block basis---either all
blocks have line numbers, or all don't. As you can imagine, this is
inconvenient, and will have to be fixed long term. I will have to sit down and
redo the Pollen-Pygments interface at some in the not-too-distant future, but I
think this is enough for a few hours' work.