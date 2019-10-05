#!perl

use strict;
use warnings;
use lib '.';
use Test::More tests => 1;
use YAGL;

my $g = YAGL->new;
$g->generate_random_vertices( { n => 124, p => 0.1, max_weight => 100 } );

my @v1 = $g->get_vertices;

my $tmpfile = 'tmp.csv';
$g->write_csv($tmpfile);

my $h = YAGL->new;
$h->read_csv($tmpfile);

my @v2 = $h->get_vertices;

is_deeply( \@v1, \@v2,
    'Graph built from CSV file has same vertices as its parent' );
