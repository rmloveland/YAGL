#!perl

use strict;
use warnings;
use lib '.';
use Test::More tests => 1;
use TinyGraph;

my $g = TinyGraph->new;

my $is = $g->isa('TinyGraph');

isa_ok( $g, 'TinyGraph' );
