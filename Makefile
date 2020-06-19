.POSIX:
.PHONY: render clean

# Locations
INPUT = website
OUTPUT = _website
POSTDIR = $(INPUT)/posts
POSTS = $(wildcard $(POSTDIR)/*.index.html.pm)

# External tool configurations
TIDY_CONFIG = tidy.config
TIDY = tidy -modify -config $(TIDY_CONFIG)

default: render publish index.html index.html.pm

render:
	raco pollen render -r $(INPUT)

publish:
	raco pollen publish $(INPUT) $(OUTPUT)
	rm $(OUTPUT)/template.html
	$(TIDY) $(OUTPUT)/*.html || true
	$(TIDY) $(OUTPUT)/$(POSTDIR)/*.html || true

clean:
	raco pollen reset

index.html: $(INPUT)/index.html.pm $(POSTS)
	raco pollen render -f $(INPUT)/index.html.pm

posts/index.html: POSTS
	raco pollen render -f $(POSTDIR)/index.html.pm
