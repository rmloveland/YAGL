#!perl

use strict;
use warnings;
use lib '.';
use Test::More tests => 1;
use TinyGraph;

my $g = TinyGraph->new;
$g->generate_random_vertices( { n => 124, p => 0.1, max_weight => 100 } );

my @v1 = $g->get_vertices;

my $tmpfile = 'tmp.csv';
$g->write_to_csv_file($tmpfile);

my $h = TinyGraph->new;
$h->read_from_csv_file($tmpfile);

my @v2 = $h->get_vertices;

is_deeply( \@v1, \@v2,
    'Graph built from CSV file has same vertices as its parent' );
