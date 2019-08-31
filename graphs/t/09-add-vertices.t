#!perl

use strict;
use warnings;
use experimentals;
use lib '.';
use Test::More tests => 1;
use Cwd;
use Smart::Match;
use TinyGraph;

my $g = TinyGraph->new;

my @to_add = qw/a b c d e f/;

$g->add_vertices(@to_add);

my @added = $g->get_vertices;

is_deeply( \@to_add, \@added, 'Adding a list of vertices works' );