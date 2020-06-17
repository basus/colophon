#lang pollen

◊(define-meta title "Key Principles and Features"
              started "2020-05-15"
              modified "2020-06-03"
              published "2020-06-05"
)

Here are some thoughts on principles guiding the development of Colophon and the
features they imply.

◊more{}

◊h2{Reuse Existing Tools Where Possible}

While Colophon is meant to be an experimental and new system, existing tools are
used whenever possible. This allows us to use high-quality, existing systems instead of
spending time and energy rolling our own (probably inferior) versions. For
example, the UNIX `make` tool is used to check dependencies and generate
incremental builds, and the ◊a["http://www.html-tidy.org/"]{◊code{tidy}} tool is used to clean
up generated HTML prior to publication.

◊h2{Minimal Magic and Least Surprise}

Using Colophon should be transparent and unsurprising. Colophon has very little
"baked in" behavior. Configuration options are easy to access, understand and
override. Instead of having configuration or fall back options backed into the
source code, all configuration is read from a configuration file which may be
overriden by users as needed. Instead of encoding metadata into filenames,
Colophon encourages putting all metadata related to a page or post into the
corresponding source file.

◊h2{Simple Mapping from Input to Outputs}

Other static site generators depend on having a particular directory structure
for input, and can produce a very different output site structure. By contrast,
Colophon keeps a much simpler mapping from input to output.

The output directory closely mirrors the input directory, and is separate from
it. Each file in the input should correspond to an output file in the same
location, just in the output directory. For example, to fix an error in
◊code{/blog/2020/02/hello-world.html}, one can look only in
◊code{/blog/2020/02/hello-world.html.pm} in the input directory, rather than
something like ◊code{_posts/2020-02-01-hello-world.html}.

After generating the output directory using ◊code{colophon build}, it should
possible to run ◊code{rsync}, with no exclusions, from the output directory and
deploy the website. Both the input and output should be amenable to version
control for archiving and backup.

