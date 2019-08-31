#!perl

use strict;
use warnings;
use experimentals;
use lib '.';
use Test::More tests => 1;
use Cwd;
use TinyGraph;

my $g   = TinyGraph->new;
my $cwd = getcwd;
$g->read_graph_from_csv_file("$cwd/t/10-get-edges.csv");

my @expected = (
    [
        'gt7079', 'zw9308',
        {
            'weight' => 96
        }
    ],
    [
        'zw9308', 'kn534',
        {
            'weight' => 78
        }
    ],
    [
        'zw9308', 'yl4524',
        {
            'weight' => 84
        }
    ],
    [
        'abc123', 'xyz789',
        {
            'weight' => 1000000
        }
    ],
    [
        'abc123',
        'I_AM_A_TEST',
        {
            'weight' => 12345
        }
    ]
);

my @got = $g->get_edges;

is_deeply( \@got, \@expected,
    qq[Getting the list of edges with ' get_edges ' works as expected] );
