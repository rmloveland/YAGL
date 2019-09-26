#!perl

use strict;
use warnings;
use lib '.';
use Test::More tests => 1;
use Cwd;
use TinyGraph;
use Data::Dumper;

my $cwd = getcwd;
my $g   = TinyGraph->new;

my $removed = 'sk132';

$g->read_csv("$cwd/t/06-edge-between.csv");

my $connected = $g->edge_between( 'ds2196', 'io9729' );

is( $connected, 1, 'Checking if vertices are connected works' );
