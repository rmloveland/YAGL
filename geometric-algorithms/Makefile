PROGRAM := convex-hull.pl

all: run

run: hull

hull:
	perl $(PROGRAM)

profile:
	perl -d:NYTProf $(PROGRAM) && nytprofhtml --open
