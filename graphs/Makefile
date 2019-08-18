all: dot

dot: perl
	dot -O -Tjpg graph.dot 

profile: random
	perl -d:NYTProf graph.pl graph.csv && nytprofhtml --open

perl: random
	perl graph.pl graph.csv

random:
	perl generate_random_graphs.pl > graph.csv

tags:
	etags *.pl
