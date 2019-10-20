#!perl

use strict;
use warnings;
use lib '.';
use Test::More tests => 2;
use YAGL;

my $g = YAGL->new;
$g->generate_random_vertices( { n => 124, p => 0.1, max_weight => 000 } );
ok(!$g->is_weighted, "Graph G is unweighted, as expected.");

my $h = YAGL->new;
$h->generate_random_vertices({ n => 124, p => 0.1, max_weight => 100_000 } );
ok($h->is_weighted > 0, "Graph H is weighted, as expected.")