# Colophon: An Extensible Static Site Generator

Colophon is a static site generator built atop the [Pollen publishing
system](https://pollenpub.com).

Colophon's goal is to be a tool for __thinking and writing__, as well as for
publication. This is in contrast to other static site generators like
[Frog](https://github.com/greghendershott/frog),
[Jekyll](https://jekyllrb.com/), or [Hugo](https://gohugo.io/). This package
also provides several libraries that help with writing websites with Pollen. It
is heavily inspired by and based the work of [Joel Dueck](https://joeldueck.com/),
epecially [The Notebook](https://thenotepad.org/).

## Components

Colophon currently has the following components

  - Libraries providing convenience functions to write documents in Pollen.
    Mainly this includes functions that implement a growing subset of HTML5 tags
    in an intelligent, Racket-friendly manner.Convenience functions over to work
    with Pollen code. This is found in the `pollen` subdirectory.
  - Code for Colophon itself, found in the `colophon` subdirectory. Though most
    of this works with Pollen code, it implements features that are specific to
    a static site (for example, generating index pages for blog posts or tags).
  - [A website](https://colophon.basus.me), built with Pollen and Colophon,
    found in the `website` directory. This is an example of Colophon in action,
    and is used to guide development.
