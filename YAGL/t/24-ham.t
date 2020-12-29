#!perl

use strict;
use warnings;
use feature qw/ say /;
use lib 'lib';
use Test::More tests => 6;
use Cwd;
use YAGL;

my $cwd = getcwd;

# Test 1 - Closed Hamiltonian walk.

my $g = YAGL->new;

$g->read_csv("$cwd/t/24-ham-00.csv");

my $expected_1 = [
    ['a', 'g', 'h', 'i', 'k', 'j', 'm', 'l', 'e', 'c', 'b', 'd', 'f'],
    ['a', 'f', 'd', 'b', 'c', 'e', 'l', 'm', 'j', 'k', 'i', 'h', 'g']
];
my @got_1 = $g->hamiltonian_walks(closed => 1);

is_deeply(\@got_1, $expected_1,
    "Found several closed Hamiltonian walks, as expected.");

# Test 2 - Find all open Hamiltonian walks on a linear tree.  This
# only works for open walks, and should fail for closed walks, as well
# as non-linear trees (see below).

my $g2 = YAGL->new;

$g2->read_csv("$cwd/t/24-ham-01.csv");

my $expected_2
  = [['hk1881', 'es4065', 'cl9661', 'rh4438', 'pt3513', 'tk3568', 'vo4916']];
my @got_2 = $g2->hamiltonian_walks(closed => undef);

is_deeply(\@got_2, $expected_2,
    "Found an open Hamiltonian walk on a linear tree, as expected.");

# As mentioned above, we should not find any Hamiltonian walks on a
# non-linear tree, that is, a tree with any vertices with degree
# higher than 1.  To test this, we add another leaf to the tree from
# the previous test to make the tree non-linear.

$g2->add_edge('tk3568', 'rl12345', {weight => 13});

my $expected_2_prime = [];
my @got_2_prime      = $g2->hamiltonian_walks;

is_deeply(\@got_2, $expected_2,
    "Did _not_ find an open Hamiltonian walk on a (non-linear) tree, as expected."
);

# Test 3 - Find all closed Hamiltonian walks on a tree (that is, none).

my $g3 = YAGL->new;

$g3->read_csv("$cwd/t/24-ham-01.csv");

my $expected_3 = [];
my @got_3 = $g3->hamiltonian_walks(closed => 1);

is_deeply(\@got_3, $expected_3,
    "Did _not_ find any closed Hamiltonian walks on a tree, as expected.");

# Test 4 - Find the closed Hamiltonian walks on a known graph from fig. 44-1 in Sedgewick 2e.  (There should be 2.)

my $g4 = YAGL->new;
$g4->read_csv(qq[$cwd/t/24-ham-02.csv]);

my $expected_4 = [
    ['a', 'f', 'd', 'b', 'c', 'e', 'l', 'm', 'j', 'k', 'i', 'h', 'g'],
    ['a', 'g', 'h', 'i', 'k', 'j', 'm', 'l', 'e', 'c', 'b', 'd', 'f']
];
my @got_4 = $g4->hamiltonian_walks(closed => 1);

is_deeply(\@got_4, $expected_4,
    "Found two closed Hamiltonian walks on the graph from fig. 44-1 in Sedgewick 2e, as expected."
);

# Test 5 - Find the open Hamiltonian walks on a known graph from fig. 44-1 in Sedgewick 2e.  (There should be 5.)

my $g5 = YAGL->new;
$g5->read_csv(qq[$cwd/t/24-ham-02.csv]);

my $expected_5 = [
    ['a', 'f', 'd', 'b', 'c', 'e', 'g', 'h', 'i', 'k', 'j', 'l', 'm'],
    ['a', 'f', 'd', 'b', 'c', 'e', 'g', 'h', 'i', 'k', 'j', 'm', 'l'],
    ['a', 'f', 'd', 'b', 'c', 'e', 'l', 'm', 'j', 'k', 'i', 'h', 'g'],
    ['a', 'g', 'h', 'i', 'k', 'j', 'm', 'l', 'e', 'c', 'b', 'd', 'f'],
    ['a', 'g', 'h', 'i', 'k', 'j', 'm', 'l', 'e', 'f', 'd', 'b', 'c']
];

my @got_5 = $g5->hamiltonian_walks;

is_deeply(\@got_5, $expected_5,
    "Found five open Hamiltonian walks on the graph from fig. 44-1 in Sedgewick 2e, as expected."
);


# Local Variables:
# compile-file: "cd .. && perl 24-ham.t"
# End:
