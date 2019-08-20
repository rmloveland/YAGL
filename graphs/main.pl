#!/usr/bin/env perl

use strict;
use warnings;
use experimentals;
use Data::Dumper;
use lib '.';
require TinyGraph;

sub main {
    my $g = TinyGraph->new;
    my @path;

    die qq[Usage: $0 FILE\n] unless scalar @ARGV >= 1;

    my $file = shift @ARGV;
    $g->build_graph_from_file($file);

    my @nodes = $g->get_nodes;

    my $i     = int rand @nodes;
    my $j     = int rand @nodes;
    my $start = $nodes[$i];
    my $end   = $nodes[$j];

    say qq[Looking for a path from '$start' to '$end' ...];

    # Unweighted, undirected graphs.
    @path = $g->find_path_between( $start, $end );
    my $viz = $g->to_graphviz( \@path );

    # Weighted, undirected graphs,
    # @path = $g->dijkstra( $start, $end );
    # my $viz = $g->to_weighted_graphviz( \@path );

    say qq[Found a path from '$start' to '$end'];
    say Dumper \@path;

    open my $fh, '>', 'graph.dot' or die $!;
    say $fh $viz;
    close $fh;
}

main();
