#!perl

use strict;
use warnings;
use experimentals;
use lib '.';
use Test::More tests => 1;
use Cwd;
use TinyGraph;

my $g = TinyGraph->new;

my @expected = (
    [ 'a', 'b', { weight => 1 } ],
    [ 'b', 'c', { weight => 2 } ],
    [ 'c', 'a', { weight => 3 } ],
);

$g->add_edges(@expected);

my @got = $g->get_edges;

is( @expected, @got, 'Adding a list of vertices works' );
