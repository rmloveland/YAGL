#!/usr/bin/env perl

use strict;
use warnings;
use experimentals;
use Data::Dumper;
use lib '.';
require YAGL;

sub main {
    my $g = YAGL->new;
    my @path;

    $g->generate_random_vertices(
        { n => 124, p => 0.1, max_weight => 100_000 } );

    # Uncomment this if you want to re-run using the last graph.
    # This can be useful for testing.
    # $g->read_csv('foo.csv');

    $g->add_vertex('abc123');
    $g->add_vertex('xyz789');
    $g->add_edge( 'abc123', 'xyz789', { weight => 1_000_000 } );

    $g->add_vertex('I_AM_A_TEST');
    $g->add_edge( 'I_AM_A_TEST', 'abc123', { weight => 12345 } );

    # Write the graph out to a CSV file.
    $g->write_csv('foo.csv');

    my @vertices = $g->get_vertices;

    my $i     = int rand @vertices;
    my $j     = int rand @vertices;
    my $start = $vertices[$i];
    my $end   = $vertices[$j];

    say qq[Looking for a path from '$start' to '$end' ...];

    # Unweighted, undirected graphs.
    @path = $g->find_path_between( $start, $end );

    for my $vertex (@path) {
        $g->set_vertex_color( $vertex, 'red' );
    }
    my $viz = $g->to_graphviz;

    say qq[Found a path from '$start' to '$end'];
    say Dumper \@path;

    open my $fh, '>', 'graph.dot' or die $!;
    say $fh $viz;
    close $fh;
}

main();
