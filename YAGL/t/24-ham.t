#!perl

use strict;
use warnings;
use lib 'lib';
use Test::More tests => 2;
use Cwd;
use lib '../lib';
use YAGL;

# Test 1 - Closed Hamiltonian walk.

my $g   = YAGL->new;
my $cwd = getcwd;

$g->read_csv("$cwd/t/24-ham-00.csv");

my $expected_1 =
  [ 'd', 'b', 'c', 'e', 'l', 'm', 'j', 'k', 'i', 'h', 'g', 'a', 'f' ];
my @got_1 = $g->hamiltonian_walk( closed => 1 );

is_deeply( \@got_1, $expected_1,
    "Found a closed Hamiltonian path as expected." );

# Test 2 - Open Hamiltonian walk.

my $h = YAGL->new;

$h->read_csv("$cwd/t/24-ham-01.csv");

my $expected_2 =
  [ 'hk1881', 'es4065', 'cl9661', 'rh4438', 'pt3513', 'tk3568', 'vo4916' ];
my @got_2 = $h->hamiltonian_walk( closed => undef );

is_deeply( \@got_2, $expected_2,
    "Found an open Hamiltonian path as expected." );
