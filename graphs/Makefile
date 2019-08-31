all: dot

dot: perl
	dot -O -Tjpg graph.dot 

profile: random
	perl -d:NYTProf main.pl graph.csv && nytprofhtml --open

perl: random
	perl main.pl graph.csv

random:
	perl generate_random_graphs.pl > graph.csv

tags: clean
	etags *.pl

clean:
	rm TAGS

test:
	prove
