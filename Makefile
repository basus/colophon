.POSIX:
.PHONY: render clean

default: render publish

render:
	raco pollen render website

publish:
	raco pollen publish website _website
	tidy -quiet -modify -indent --wrap 100 --wrap-attributes no --tidy-mark no _website/*.html || true

clean:
	raco pollen reset

