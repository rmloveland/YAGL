#!perl

use strict;
use warnings;
use feature qw/ say /;
use lib '../lib';
require YAGL;

sub main {
    my $g = YAGL->new;
    my @path;

    $g->generate_random_vertices(
        { n => 124, p => 0.001, max_weight => 1000 } );

    # Uncomment this if you want to re-run using the last graph.
    # This can be useful for testing.
    # $g->read_csv('mst.csv');

    # Write the graph out to a CSV file.
    $g->write_csv('mst.csv');

    my @vertices = $g->get_vertices;

    my $i     = int rand @vertices;
    my $start = $vertices[$i];

    my $mst = $g->mst;

    my $g_viz = $g->to_graphviz;
    open my $g_fh, '>', 'graph.dot' or die $!;
    say $g_fh $g_viz;
    close $g_fh;
    my $g_cmd = qq[dot -Tsvg -O graph.dot];
    system $g_cmd;

    if ($mst) {
        say qq[Found an MST];
        my $mst_viz = $mst->to_graphviz;
        open my $mst_fh, '>', 'mst.dot' or die $!;
        say $mst_fh $mst_viz;
        close $mst_fh;
        my $mst_cmd = qq[dot -Tsvg -O mst.dot];
        system $mst_cmd;
    }
    else {
        open my $mst_fh, '>', 'mst.dot' or die $!;
        say $mst_fh '';
        close $mst_fh;
        my $mst_cmd = qq[dot -Tsvg -O mst.dot];
        system $mst_cmd;
    }
}

main();
