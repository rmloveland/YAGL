all: dot

dot: perl
	dot -O -Tjpg graph.dot 

perl: random
	perl graph.pl graph.csv

random:
	perl generate_random_graphs.pl > graph.csv
