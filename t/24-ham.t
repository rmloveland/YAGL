#!perl

use strict;
use warnings;
use lib 'lib';
use Test::More tests => 9;
use Cwd;
use YAGL;

my $cwd = getcwd;

# Test 1 - Closed Hamiltonian walk.

my $g = YAGL->new;

$g->read_csv("$cwd/t/24-ham-00.csv");

my $expected_1 = [
    ['a', 'g', 'h', 'i', 'k', 'j', 'm', 'l', 'e', 'c', 'b', 'd', 'f'],
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

# Test 3 - As mentioned above, we should not find any Hamiltonian walks on a
# non-linear tree, that is, a tree with any vertices with degree
# higher than 1.  To test this, we add another leaf to the tree from
# the previous test to make the tree non-linear.

$g2->add_edge('tk3568', 'rl12345', {weight => 13});

my $expected_2_prime = [];
my @got_2_prime      = $g2->hamiltonian_walks;

is_deeply(\@got_2, $expected_2,
    "Did _not_ find an open Hamiltonian walk on a (non-linear) tree, as expected."
);

# Test 4 - Find all closed Hamiltonian walks on a tree (that is, none).

my $g3 = YAGL->new;

$g3->read_csv("$cwd/t/24-ham-01.csv");

my $expected_3 = [];
my @got_3      = $g3->hamiltonian_walks(closed => 1);

is_deeply(\@got_3, $expected_3,
    "Did _not_ find any closed Hamiltonian walks on a tree, as expected.");

# Test 5 - Find the closed Hamiltonian walks on a known graph from fig. 44-1 in Sedgewick 2e.  (There should be 2.)

my $g4 = YAGL->new;
$g4->read_csv(qq[$cwd/t/24-ham-02.csv]);

my $expected_4 = [
    ['a', 'f', 'd', 'b', 'c', 'e', 'l', 'm', 'j', 'k', 'i', 'h', 'g'],
];
my @got_4 = $g4->hamiltonian_walks(closed => 1);

is_deeply(\@got_4, $expected_4,
    "Found one (1) closed Hamiltonian walk on the graph from fig. 44-1 in Sedgewick 2e, as expected."
);

# Test 6 - Find the open Hamiltonian walks on a known graph from fig. 44-1 in Sedgewick 2e.  (There should be 5.)

my $g5 = YAGL->new;
$g5->read_csv(qq[$cwd/t/24-ham-02.csv]);

my $expected_5 = [
    ['a', 'f', 'd', 'b', 'c', 'e', 'g', 'h', 'i', 'k', 'j', 'l', 'm'],
    ['a', 'f', 'd', 'b', 'c', 'e', 'g', 'h', 'i', 'k', 'j', 'm', 'l'],
    ['a', 'f', 'd', 'b', 'c', 'e', 'l', 'm', 'j', 'k', 'i', 'h', 'g'],
    ['a', 'g', 'h', 'i', 'k', 'j', 'm', 'l', 'e', 'f', 'd', 'b', 'c']
];

my @got_5 = $g5->hamiltonian_walks;

is_deeply(\@got_5, $expected_5,
    "Found four (4) open Hamiltonian walks on the graph from fig. 44-1 in Sedgewick 2e, as expected."
);

# Test 7 - Smallest uniquely hamiltonian graph with minimum degree at least 3
# In other words, it has exactly 1 Hamiltonian walk.
# https://mathoverflow.net/questions/255784/what-is-the-smallest-uniquely-hamiltonian-graph-with-minimum-degree-at-least-3/

my $g6 = YAGL->new;
$g6->read_lst(qq[$cwd/t/24-ham-03.lst]);

my $expected_6 = [
          [
            '0',
            '7',
            '15',
            '6',
            '11',
            '14',
            '1',
            '8',
            '17',
            '5',
            '12',
            '9',
            '3',
            '13',
            '4',
            '10',
            '2',
            '16'
          ],
        ];

my @got_6 = $g6->hamiltonian_walks(closed => 1);

is_deeply(\@got_6, $expected_6,
    "Found one (1) closed Hamiltonian walk on the smallest uniquely Hamiltonian graph with minimum degree at least 3."
);

# Test 7 - Smallest uniquely hamiltonian graph with minimum degree at least 3
# In other words, it has exactly 1 Hamiltonian walk.
# https://mathoverflow.net/questions/255784/what-is-the-smallest-uniquely-hamiltonian-graph-with-minimum-degree-at-least-3/

my $g7 = YAGL->new;
$g7->read_lst(qq[$cwd/t/24-ham-03.lst]);

my $expected_7 = [
    [
        '0',  '7', '15', '6',  '11', '14', '1', '8', '17', '5',
        '12', '9', '3',  '13', '4',  '10', '2', '16',
    ],
    [
        '0',  '16', '2', '10', '4',  '13', '3', '9',  '12', '5',
        '17', '8',  '1', '14', '11', '6',  '15', '7'
    ],
];

my @got_7 = $g7->hamiltonian_walks(closed => 1, allow_reversals => 1);

is_deeply(\@got_7, $expected_7,
    "Found two (2) closed Hamiltonian walks (when reverse walks allowed) on the smallest uniquely Hamiltonian graph with minimum degree at least 3."
);

=head2 Test 8. The K5 graph - restricting to one solution

L<https://hog.grinvin.org/ViewGraphInfo.action?id=462>

=cut

my $g8 = YAGL->new;
$g8->read_lst("$cwd/t/28-graph_462.lst");

# Total (non-distinct) Hamiltonian circuits in complete graph Kn is (n−1)!
# https://math.stackexchange.com/questions/249817/how-many-hamiltonian-cycles-are-there-in-a-complete-graph-k-n-n-geq-3-why
my $expected_8 = [
    ['1', '2', '3', '4', '5'],
];

my @got_8 = $g8->hamiltonian_walks(closed => 1, n_solutions => 1);

is_deeply(\@got_8, $expected_8,
          "Limited output to one (1) Hamiltonian walk in graph 462 (a.k.a. K5) from House of Graphs, as expected.");

# Local Variables:
# compile-command: "cd .. && perl t/24-ham.t"
# End:
