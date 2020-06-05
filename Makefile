.POSIX:
.PHONY: render clean

# Locations
INPUT = website
OUTPUT = _website
POSTS = posts

# External tool configurations
TIDY_CONFIG = tidy.config
TIDY = tidy -modify -config $(TIDY_CONFIG)

default: render publish

render:
	raco pollen render $(INPUT)

publish:
	raco pollen publish $(INPUT) $(OUTPUT)
	$(TIDY) $(OUTPUT)/*.html || true
	$(TIDY) $(OUTPUT)/$(POSTS)/*.html || true

clean:
	raco pollen reset

