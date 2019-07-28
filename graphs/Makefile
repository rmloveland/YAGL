all: dot

dot: perl
	"C:\Program Files (x86)\Graphviz2.38\bin\dot.exe" -O -Tjpg graph.dot 

perl: random
	perl graph.pl graph.csv

random:
	perl generate_random_graphs.pl > graph.csv
