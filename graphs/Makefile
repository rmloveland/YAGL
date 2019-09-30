all: dot

dot: perl
	dot -O -Tjpg graph.dot

profile:
	perl -d:NYTProf main.pl && nytprofhtml --open

perl:
	perl main.pl

tags:
	etags --lang=perl *.pm

clean:
	rm TAGS

test:
	prove
