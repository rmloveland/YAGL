#!perl

use strict;
use warnings;
use lib '.';
use Test::More tests => 2;
use Cwd;
use TinyGraph;
use Data::Dumper;

my $cwd = getcwd;
my $g   = TinyGraph->new;

my $removed = 'sk132';

$g->read_graph_from_csv_file("$cwd/t/05-delete-vertices.csv");

my @before = $g->get_vertices;
$g->remove_vertex($removed);
my $connected = $g->edge_between( $removed, 'io9729' );

isnt( $connected, 1,
    'Deleting a single vertex works - check if edges connect' );

my @after = $g->get_vertices;
isnt( \@before, \@after,
    'Deleting a single vertex works - compare vertex lists' );
