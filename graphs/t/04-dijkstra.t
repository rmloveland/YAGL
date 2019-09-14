#!perl

use strict;
use warnings;
use lib '.';
use Test::More tests => 1;
use Cwd;
use TinyGraph;

my $g   = TinyGraph->new;
my $cwd = getcwd;

$g->read_from_csv_file("$cwd/t/04-dijkstra.csv");

my $start = 'da1705';
my $end   = 'gk1114';

my @got = $g->dijkstra( $start, $end );

my $expected = [
    {
        'distance' => 0,
        'vertex'   => 'da1705'
    },
    {
        'distance' => 27,
        'vertex'   => 'je793'
    },
    {
        'vertex'   => 'qe5674',
        'distance' => 80
    },
    {
        'distance' => 166,
        'vertex'   => 'fj5687'
    },
    {
        'distance' => 251,
        'vertex'   => 'ft3255'
    },
    {
        'vertex'   => 'le2845',
        'distance' => 273
    },
    {
        'distance' => 367,
        'vertex'   => 'oh4681'
    },
    {
        'distance' => 412,
        'vertex'   => 'hz6259'
    },
    {
        'vertex'   => 'ey9821',
        'distance' => 469
    },
    {
        'distance' => 553,
        'vertex'   => 'yo6017'
    },
    {
        'vertex'   => 'qx2734',
        'distance' => 577
    },
    {
        'distance' => 605,
        'vertex'   => 'os1043'
    },
    {
        'distance' => 676,
        'vertex'   => 'gk1114'
    }
];

is_deeply( $expected, \@got, "Dijkstra's algorithm works as expected" );
