#lang pollen

◊(define-meta title "Extracting Reusable Code"
   started "2020-11-19"
   published "2020-11-19"
   )

After a long hiatus, I'm putting some work into Colophon again. When I left off
some months ago, I had a decently working static site generation setup,
including niceties like syntax highlighting and basic index generation. But a
large part of the functionality was in the ◊code{pollen.rkt} file for this
website, not in the code for Colophon itself. My hope is that Colophon will be
usable for interesting websites in general, and that it will be possible to get
a usable starting site with a single command. So I spent a couple of hours
pulling out functionality from the website-specific code into Colophon proper.

◊more{}

As part of developing the website, I had written a bunch of helper functions
that generate parts of a page, such as the ◊code{<head>} and ◊code{<body>} tags.
The major part of the refactoring was pulling these functions out into the
◊code{components} submodule inside of Colophon. In addition, there are some new
utility functions in the ◊code{predicates} submodule, and a new ◊code{tags}
submodule for smaller functions.

After this refactoring, the ◊code{pollen.rkt} file for the website only defines
the navigation bar for the site, and exports things from Colophon for the page
templates to work properly. It should now be possible to start a new Colophon
site by just creating templates and pages you want, and importing the Colophon
functionality you need into a ◊code{pollen.rkt} file (and re-exporting what you
need).

This isn't quite user-friendly yet. There are a bunch of defaults for the
various components that should be
◊a["https://beautifulracket.com/explainer/parameters.html"]{properly
parameterized}. There is also the beginnings of a theme system tied into the
components, that should be pulled out and exposed properly. And there is
currently no user interface to actually set up or build a Colophon site. I'm
hoping to add these features over the next few weeks, so that I can eventually
run a command like ◊code{raco colophon new} and get a new, but usable, Colophon
site.