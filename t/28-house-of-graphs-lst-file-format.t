#!perl

use strict;
use warnings;
use feature qw/ say /;
use lib 'lib';
use Test::More tests => 4;
use YAGL;
use Cwd;
use Data::Dumper;

my $cwd = getcwd;

=head2 Test 1. Simple 7 node undirected, unweighted Hamiltonian graph

L<https://hog.grinvin.org/ViewGraphInfo.action?id=520>

=cut

my $g = YAGL->new;
$g->read_lst("$cwd/t/28-graph_520.lst");

my $expected = [
          [
            '1',
            '2',
            '6',
            '4',
            '5',
            '7',
            '3'
          ],
          [
            '1',
            '3',
            '7',
            '5',
            '4',
            '6',
            '2'
          ]
        ];

my @got = $g->hamiltonian_walks(closed => 1, allow_reversals => 1);

is_deeply(\@got, $expected,
          "Found two (2) Hamiltonian walks in graph 520 (7 vertex ring graph) from House of Graphs, as expected.");

=head2 Test 2. The K5 graph

L<https://hog.grinvin.org/ViewGraphInfo.action?id=462>

=cut

my $g2 = YAGL->new;
$g2->read_lst("$cwd/t/28-graph_462.lst");
$g2->draw('28-graph_462');

# Total (non-distinct) Hamiltonian circuits in complete graph Kn is (n−1)!
# https://math.stackexchange.com/questions/249817/how-many-hamiltonian-cycles-are-there-in-a-complete-graph-k-n-n-geq-3-why
my $expected_2 = [
    ['1', '2', '3', '4', '5'],
    ['1', '2', '3', '5', '4'],
    ['1', '2', '4', '3', '5'],
    ['1', '2', '4', '5', '3'],
    ['1', '2', '5', '3', '4'],
    ['1', '2', '5', '4', '3'],
    ['1', '3', '2', '4', '5'],
    ['1', '3', '2', '5', '4'],
    ['1', '3', '4', '2', '5'],
    ['1', '3', '4', '5', '2'],
    ['1', '3', '5', '2', '4'],
    ['1', '3', '5', '4', '2'],
    ['1', '4', '2', '3', '5'],
    ['1', '4', '2', '5', '3'],
    ['1', '4', '3', '2', '5'],
    ['1', '4', '3', '5', '2'],
    ['1', '4', '5', '2', '3'],
    ['1', '4', '5', '3', '2'],
    ['1', '5', '2', '3', '4'],
    ['1', '5', '2', '4', '3'],
    ['1', '5', '3', '2', '4'],
    ['1', '5', '3', '4', '2'],
    ['1', '5', '4', '2', '3'],
    ['1', '5', '4', '3', '2']
];

my @got_2 = $g2->hamiltonian_walks(closed => 1, allow_reversals => 1);

# say Dumper \@got_2;

is_deeply(\@got_2, $expected_2,
          "Found Hamiltonian walks in graph 462 (a.k.a. K5) from House of Graphs, as expected.");

=head2 Test 3. Beineke non-line graph G7

L<https://hog.grinvin.org/ViewGraphInfo.action?id=21093>

=cut

my $g3 = YAGL->new;
$g3->read_lst("$cwd/t/28-graph_21093.lst");
$g3->draw('28-graph_21093');

my @got_3 = $g3->hamiltonian_walks(closed => 1, allow_reversals => 1);

my $expected_3 = [
    ['1', '2', '6', '3', '4', '5'],
    ['1', '2', '6', '4', '3', '5'],
    ['1', '5', '3', '4', '6', '2'],
    ['1', '5', '4', '3', '6', '2']
];

is_deeply(\@got_3, $expected_3,
          "Found Hamiltonian walks in graph 21093 (Beineke non-line G7) from House of Graphs, as expected.");

=head2 Test 4. The unique triangulation with 11 vertices

L<https://hog.grinvin.org/ViewGraphInfo.action?id=19203>

=cut

my $g4 = YAGL->new;
$g4->read_lst("$cwd/t/28-graph_19203.lst");
$g4->draw('28-graph_19203');

my @got_4 = $g4->hamiltonian_walks(closed => 1, allow_reversals => 1);

my $expected_4 = [
    ['1', '2',  '9',  '7',  '5',  '4',  '6',  '8',  '10', '3',  '11'],
    ['1', '2',  '9',  '7',  '5',  '4',  '6',  '8',  '11', '3',  '10'],
    ['1', '2',  '9',  '7',  '5',  '4',  '6',  '10', '3',  '8',  '11'],
    ['1', '2',  '9',  '7',  '5',  '4',  '6',  '10', '8',  '3',  '11'],
    ['1', '2',  '9',  '7',  '5',  '4',  '6',  '10', '8',  '11', '3'],
    ['1', '2',  '9',  '7',  '5',  '4',  '6',  '11', '3',  '8',  '10'],
    ['1', '2',  '9',  '7',  '5',  '4',  '6',  '11', '8',  '3',  '10'],
    ['1', '2',  '9',  '7',  '5',  '4',  '6',  '11', '8',  '10', '3'],
    ['1', '2',  '9',  '7',  '5',  '4',  '10', '3',  '8',  '6',  '11'],
    ['1', '2',  '9',  '7',  '5',  '4',  '10', '6',  '8',  '3',  '11'],
    ['1', '2',  '9',  '7',  '5',  '4',  '10', '6',  '8',  '11', '3'],
    ['1', '2',  '9',  '7',  '5',  '4',  '10', '6',  '11', '8',  '3'],
    ['1', '2',  '9',  '7',  '5',  '4',  '10', '8',  '6',  '11', '3'],
    ['1', '2',  '9',  '7',  '5',  '4',  '11', '3',  '8',  '6',  '10'],
    ['1', '2',  '9',  '7',  '5',  '4',  '11', '6',  '8',  '3',  '10'],
    ['1', '2',  '9',  '7',  '5',  '4',  '11', '6',  '8',  '10', '3'],
    ['1', '2',  '9',  '7',  '5',  '4',  '11', '6',  '10', '8',  '3'],
    ['1', '2',  '9',  '7',  '5',  '4',  '11', '8',  '6',  '10', '3'],
    ['1', '2',  '9',  '7',  '5',  '10', '3',  '8',  '6',  '4',  '11'],
    ['1', '2',  '9',  '7',  '5',  '10', '4',  '6',  '8',  '3',  '11'],
    ['1', '2',  '9',  '7',  '5',  '10', '4',  '6',  '8',  '11', '3'],
    ['1', '2',  '9',  '7',  '5',  '10', '4',  '6',  '11', '8',  '3'],
    ['1', '2',  '9',  '7',  '5',  '10', '4',  '11', '6',  '8',  '3'],
    ['1', '2',  '9',  '7',  '5',  '10', '6',  '4',  '11', '8',  '3'],
    ['1', '2',  '9',  '7',  '5',  '10', '8',  '6',  '4',  '11', '3'],
    ['1', '2',  '9',  '7',  '5',  '11', '3',  '8',  '6',  '4',  '10'],
    ['1', '2',  '9',  '7',  '5',  '11', '4',  '6',  '8',  '3',  '10'],
    ['1', '2',  '9',  '7',  '5',  '11', '4',  '6',  '8',  '10', '3'],
    ['1', '2',  '9',  '7',  '5',  '11', '4',  '6',  '10', '8',  '3'],
    ['1', '2',  '9',  '7',  '5',  '11', '4',  '10', '6',  '8',  '3'],
    ['1', '2',  '9',  '7',  '5',  '11', '6',  '4',  '10', '8',  '3'],
    ['1', '2',  '9',  '7',  '5',  '11', '8',  '6',  '4',  '10', '3'],
    ['1', '2',  '9',  '7',  '10', '3',  '8',  '6',  '4',  '5',  '11'],
    ['1', '2',  '9',  '7',  '10', '4',  '5',  '11', '6',  '8',  '3'],
    ['1', '2',  '9',  '7',  '10', '5',  '4',  '6',  '8',  '3',  '11'],
    ['1', '2',  '9',  '7',  '10', '5',  '4',  '6',  '8',  '11', '3'],
    ['1', '2',  '9',  '7',  '10', '5',  '4',  '6',  '11', '8',  '3'],
    ['1', '2',  '9',  '7',  '10', '5',  '4',  '11', '6',  '8',  '3'],
    ['1', '2',  '9',  '7',  '10', '5',  '11', '4',  '6',  '8',  '3'],
    ['1', '2',  '9',  '7',  '10', '6',  '4',  '5',  '11', '8',  '3'],
    ['1', '2',  '9',  '7',  '10', '8',  '6',  '4',  '5',  '11', '3'],
    ['1', '2',  '9',  '7',  '11', '3',  '8',  '6',  '4',  '5',  '10'],
    ['1', '2',  '9',  '7',  '11', '4',  '5',  '10', '6',  '8',  '3'],
    ['1', '2',  '9',  '7',  '11', '5',  '4',  '6',  '8',  '3',  '10'],
    ['1', '2',  '9',  '7',  '11', '5',  '4',  '6',  '8',  '10', '3'],
    ['1', '2',  '9',  '7',  '11', '5',  '4',  '6',  '10', '8',  '3'],
    ['1', '2',  '9',  '7',  '11', '5',  '4',  '10', '6',  '8',  '3'],
    ['1', '2',  '9',  '7',  '11', '5',  '10', '4',  '6',  '8',  '3'],
    ['1', '2',  '9',  '7',  '11', '6',  '4',  '5',  '10', '8',  '3'],
    ['1', '2',  '9',  '7',  '11', '8',  '6',  '4',  '5',  '10', '3'],
    ['1', '2',  '9',  '10', '3',  '8',  '6',  '4',  '5',  '7',  '11'],
    ['1', '2',  '9',  '10', '4',  '5',  '7',  '11', '6',  '8',  '3'],
    ['1', '2',  '9',  '10', '5',  '7',  '11', '4',  '6',  '8',  '3'],
    ['1', '2',  '9',  '10', '6',  '4',  '5',  '7',  '11', '8',  '3'],
    ['1', '2',  '9',  '10', '7',  '5',  '4',  '6',  '8',  '3',  '11'],
    ['1', '2',  '9',  '10', '7',  '5',  '4',  '6',  '8',  '11', '3'],
    ['1', '2',  '9',  '10', '7',  '5',  '4',  '6',  '11', '8',  '3'],
    ['1', '2',  '9',  '10', '7',  '5',  '4',  '11', '6',  '8',  '3'],
    ['1', '2',  '9',  '10', '7',  '5',  '11', '4',  '6',  '8',  '3'],
    ['1', '2',  '9',  '10', '7',  '11', '5',  '4',  '6',  '8',  '3'],
    ['1', '2',  '9',  '10', '8',  '6',  '4',  '5',  '7',  '11', '3'],
    ['1', '2',  '9',  '11', '3',  '8',  '6',  '4',  '5',  '7',  '10'],
    ['1', '2',  '9',  '11', '4',  '5',  '7',  '10', '6',  '8',  '3'],
    ['1', '2',  '9',  '11', '5',  '7',  '10', '4',  '6',  '8',  '3'],
    ['1', '2',  '9',  '11', '6',  '4',  '5',  '7',  '10', '8',  '3'],
    ['1', '2',  '9',  '11', '7',  '5',  '4',  '6',  '8',  '3',  '10'],
    ['1', '2',  '9',  '11', '7',  '5',  '4',  '6',  '8',  '10', '3'],
    ['1', '2',  '9',  '11', '7',  '5',  '4',  '6',  '10', '8',  '3'],
    ['1', '2',  '9',  '11', '7',  '5',  '4',  '10', '6',  '8',  '3'],
    ['1', '2',  '9',  '11', '7',  '5',  '10', '4',  '6',  '8',  '3'],
    ['1', '2',  '9',  '11', '7',  '10', '5',  '4',  '6',  '8',  '3'],
    ['1', '2',  '9',  '11', '8',  '6',  '4',  '5',  '7',  '10', '3'],
    ['1', '2',  '10', '3',  '8',  '6',  '4',  '5',  '7',  '9',  '11'],
    ['1', '2',  '10', '4',  '5',  '7',  '9',  '11', '6',  '8',  '3'],
    ['1', '2',  '10', '5',  '7',  '9',  '11', '4',  '6',  '8',  '3'],
    ['1', '2',  '10', '6',  '4',  '5',  '7',  '9',  '11', '8',  '3'],
    ['1', '2',  '10', '7',  '9',  '11', '5',  '4',  '6',  '8',  '3'],
    ['1', '2',  '10', '8',  '6',  '4',  '5',  '7',  '9',  '11', '3'],
    ['1', '2',  '10', '9',  '7',  '5',  '4',  '6',  '8',  '3',  '11'],
    ['1', '2',  '10', '9',  '7',  '5',  '4',  '6',  '8',  '11', '3'],
    ['1', '2',  '10', '9',  '7',  '5',  '4',  '6',  '11', '8',  '3'],
    ['1', '2',  '10', '9',  '7',  '5',  '4',  '11', '6',  '8',  '3'],
    ['1', '2',  '10', '9',  '7',  '5',  '11', '4',  '6',  '8',  '3'],
    ['1', '2',  '10', '9',  '7',  '11', '5',  '4',  '6',  '8',  '3'],
    ['1', '2',  '10', '9',  '11', '7',  '5',  '4',  '6',  '8',  '3'],
    ['1', '2',  '11', '3',  '8',  '6',  '4',  '5',  '7',  '9',  '10'],
    ['1', '2',  '11', '4',  '5',  '7',  '9',  '10', '6',  '8',  '3'],
    ['1', '2',  '11', '5',  '7',  '9',  '10', '4',  '6',  '8',  '3'],
    ['1', '2',  '11', '6',  '4',  '5',  '7',  '9',  '10', '8',  '3'],
    ['1', '2',  '11', '7',  '9',  '10', '5',  '4',  '6',  '8',  '3'],
    ['1', '2',  '11', '8',  '6',  '4',  '5',  '7',  '9',  '10', '3'],
    ['1', '2',  '11', '9',  '7',  '5',  '4',  '6',  '8',  '3',  '10'],
    ['1', '2',  '11', '9',  '7',  '5',  '4',  '6',  '8',  '10', '3'],
    ['1', '2',  '11', '9',  '7',  '5',  '4',  '6',  '10', '8',  '3'],
    ['1', '2',  '11', '9',  '7',  '5',  '4',  '10', '6',  '8',  '3'],
    ['1', '2',  '11', '9',  '7',  '5',  '10', '4',  '6',  '8',  '3'],
    ['1', '2',  '11', '9',  '7',  '10', '5',  '4',  '6',  '8',  '3'],
    ['1', '2',  '11', '9',  '10', '7',  '5',  '4',  '6',  '8',  '3'],
    ['1', '3',  '8',  '6',  '4',  '5',  '7',  '9',  '10', '2',  '11'],
    ['1', '3',  '8',  '6',  '4',  '5',  '7',  '9',  '11', '2',  '10'],
    ['1', '3',  '8',  '6',  '4',  '5',  '7',  '10', '2',  '9',  '11'],
    ['1', '3',  '8',  '6',  '4',  '5',  '7',  '10', '9',  '2',  '11'],
    ['1', '3',  '8',  '6',  '4',  '5',  '7',  '10', '9',  '11', '2'],
    ['1', '3',  '8',  '6',  '4',  '5',  '7',  '11', '2',  '9',  '10'],
    ['1', '3',  '8',  '6',  '4',  '5',  '7',  '11', '9',  '2',  '10'],
    ['1', '3',  '8',  '6',  '4',  '5',  '7',  '11', '9',  '10', '2'],
    ['1', '3',  '8',  '6',  '4',  '5',  '10', '2',  '9',  '7',  '11'],
    ['1', '3',  '8',  '6',  '4',  '5',  '10', '7',  '9',  '2',  '11'],
    ['1', '3',  '8',  '6',  '4',  '5',  '10', '7',  '9',  '11', '2'],
    ['1', '3',  '8',  '6',  '4',  '5',  '10', '7',  '11', '9',  '2'],
    ['1', '3',  '8',  '6',  '4',  '5',  '10', '9',  '7',  '11', '2'],
    ['1', '3',  '8',  '6',  '4',  '5',  '11', '2',  '9',  '7',  '10'],
    ['1', '3',  '8',  '6',  '4',  '5',  '11', '7',  '9',  '2',  '10'],
    ['1', '3',  '8',  '6',  '4',  '5',  '11', '7',  '9',  '10', '2'],
    ['1', '3',  '8',  '6',  '4',  '5',  '11', '7',  '10', '9',  '2'],
    ['1', '3',  '8',  '6',  '4',  '5',  '11', '9',  '7',  '10', '2'],
    ['1', '3',  '8',  '6',  '4',  '10', '2',  '9',  '7',  '5',  '11'],
    ['1', '3',  '8',  '6',  '4',  '10', '5',  '7',  '9',  '2',  '11'],
    ['1', '3',  '8',  '6',  '4',  '10', '5',  '7',  '9',  '11', '2'],
    ['1', '3',  '8',  '6',  '4',  '10', '5',  '7',  '11', '9',  '2'],
    ['1', '3',  '8',  '6',  '4',  '10', '5',  '11', '7',  '9',  '2'],
    ['1', '3',  '8',  '6',  '4',  '10', '7',  '5',  '11', '9',  '2'],
    ['1', '3',  '8',  '6',  '4',  '10', '9',  '7',  '5',  '11', '2'],
    ['1', '3',  '8',  '6',  '4',  '11', '2',  '9',  '7',  '5',  '10'],
    ['1', '3',  '8',  '6',  '4',  '11', '5',  '7',  '9',  '2',  '10'],
    ['1', '3',  '8',  '6',  '4',  '11', '5',  '7',  '9',  '10', '2'],
    ['1', '3',  '8',  '6',  '4',  '11', '5',  '7',  '10', '9',  '2'],
    ['1', '3',  '8',  '6',  '4',  '11', '5',  '10', '7',  '9',  '2'],
    ['1', '3',  '8',  '6',  '4',  '11', '7',  '5',  '10', '9',  '2'],
    ['1', '3',  '8',  '6',  '4',  '11', '9',  '7',  '5',  '10', '2'],
    ['1', '3',  '8',  '6',  '10', '2',  '9',  '7',  '5',  '4',  '11'],
    ['1', '3',  '8',  '6',  '10', '4',  '5',  '7',  '9',  '2',  '11'],
    ['1', '3',  '8',  '6',  '10', '4',  '5',  '7',  '9',  '11', '2'],
    ['1', '3',  '8',  '6',  '10', '4',  '5',  '7',  '11', '9',  '2'],
    ['1', '3',  '8',  '6',  '10', '4',  '5',  '11', '7',  '9',  '2'],
    ['1', '3',  '8',  '6',  '10', '4',  '11', '5',  '7',  '9',  '2'],
    ['1', '3',  '8',  '6',  '10', '5',  '4',  '11', '7',  '9',  '2'],
    ['1', '3',  '8',  '6',  '10', '7',  '5',  '4',  '11', '9',  '2'],
    ['1', '3',  '8',  '6',  '10', '9',  '7',  '5',  '4',  '11', '2'],
    ['1', '3',  '8',  '6',  '11', '2',  '9',  '7',  '5',  '4',  '10'],
    ['1', '3',  '8',  '6',  '11', '4',  '5',  '7',  '9',  '2',  '10'],
    ['1', '3',  '8',  '6',  '11', '4',  '5',  '7',  '9',  '10', '2'],
    ['1', '3',  '8',  '6',  '11', '4',  '5',  '7',  '10', '9',  '2'],
    ['1', '3',  '8',  '6',  '11', '4',  '5',  '10', '7',  '9',  '2'],
    ['1', '3',  '8',  '6',  '11', '4',  '10', '5',  '7',  '9',  '2'],
    ['1', '3',  '8',  '6',  '11', '5',  '4',  '10', '7',  '9',  '2'],
    ['1', '3',  '8',  '6',  '11', '7',  '5',  '4',  '10', '9',  '2'],
    ['1', '3',  '8',  '6',  '11', '9',  '7',  '5',  '4',  '10', '2'],
    ['1', '3',  '8',  '10', '2',  '9',  '7',  '5',  '4',  '6',  '11'],
    ['1', '3',  '8',  '10', '4',  '6',  '11', '5',  '7',  '9',  '2'],
    ['1', '3',  '8',  '10', '5',  '4',  '6',  '11', '7',  '9',  '2'],
    ['1', '3',  '8',  '10', '6',  '4',  '5',  '7',  '9',  '2',  '11'],
    ['1', '3',  '8',  '10', '6',  '4',  '5',  '7',  '9',  '11', '2'],
    ['1', '3',  '8',  '10', '6',  '4',  '5',  '7',  '11', '9',  '2'],
    ['1', '3',  '8',  '10', '6',  '4',  '5',  '11', '7',  '9',  '2'],
    ['1', '3',  '8',  '10', '6',  '4',  '11', '5',  '7',  '9',  '2'],
    ['1', '3',  '8',  '10', '6',  '11', '4',  '5',  '7',  '9',  '2'],
    ['1', '3',  '8',  '10', '7',  '5',  '4',  '6',  '11', '9',  '2'],
    ['1', '3',  '8',  '10', '9',  '7',  '5',  '4',  '6',  '11', '2'],
    ['1', '3',  '8',  '11', '2',  '9',  '7',  '5',  '4',  '6',  '10'],
    ['1', '3',  '8',  '11', '4',  '6',  '10', '5',  '7',  '9',  '2'],
    ['1', '3',  '8',  '11', '5',  '4',  '6',  '10', '7',  '9',  '2'],
    ['1', '3',  '8',  '11', '6',  '4',  '5',  '7',  '9',  '2',  '10'],
    ['1', '3',  '8',  '11', '6',  '4',  '5',  '7',  '9',  '10', '2'],
    ['1', '3',  '8',  '11', '6',  '4',  '5',  '7',  '10', '9',  '2'],
    ['1', '3',  '8',  '11', '6',  '4',  '5',  '10', '7',  '9',  '2'],
    ['1', '3',  '8',  '11', '6',  '4',  '10', '5',  '7',  '9',  '2'],
    ['1', '3',  '8',  '11', '6',  '10', '4',  '5',  '7',  '9',  '2'],
    ['1', '3',  '8',  '11', '7',  '5',  '4',  '6',  '10', '9',  '2'],
    ['1', '3',  '8',  '11', '9',  '7',  '5',  '4',  '6',  '10', '2'],
    ['1', '3',  '10', '2',  '9',  '7',  '5',  '4',  '6',  '8',  '11'],
    ['1', '3',  '10', '4',  '6',  '8',  '11', '5',  '7',  '9',  '2'],
    ['1', '3',  '10', '5',  '4',  '6',  '8',  '11', '7',  '9',  '2'],
    ['1', '3',  '10', '6',  '8',  '11', '4',  '5',  '7',  '9',  '2'],
    ['1', '3',  '10', '7',  '5',  '4',  '6',  '8',  '11', '9',  '2'],
    ['1', '3',  '10', '8',  '6',  '4',  '5',  '7',  '9',  '2',  '11'],
    ['1', '3',  '10', '8',  '6',  '4',  '5',  '7',  '9',  '11', '2'],
    ['1', '3',  '10', '8',  '6',  '4',  '5',  '7',  '11', '9',  '2'],
    ['1', '3',  '10', '8',  '6',  '4',  '5',  '11', '7',  '9',  '2'],
    ['1', '3',  '10', '8',  '6',  '4',  '11', '5',  '7',  '9',  '2'],
    ['1', '3',  '10', '8',  '6',  '11', '4',  '5',  '7',  '9',  '2'],
    ['1', '3',  '10', '8',  '11', '6',  '4',  '5',  '7',  '9',  '2'],
    ['1', '3',  '10', '9',  '7',  '5',  '4',  '6',  '8',  '11', '2'],
    ['1', '3',  '11', '2',  '9',  '7',  '5',  '4',  '6',  '8',  '10'],
    ['1', '3',  '11', '4',  '6',  '8',  '10', '5',  '7',  '9',  '2'],
    ['1', '3',  '11', '5',  '4',  '6',  '8',  '10', '7',  '9',  '2'],
    ['1', '3',  '11', '6',  '8',  '10', '4',  '5',  '7',  '9',  '2'],
    ['1', '3',  '11', '7',  '5',  '4',  '6',  '8',  '10', '9',  '2'],
    ['1', '3',  '11', '8',  '6',  '4',  '5',  '7',  '9',  '2',  '10'],
    ['1', '3',  '11', '8',  '6',  '4',  '5',  '7',  '9',  '10', '2'],
    ['1', '3',  '11', '8',  '6',  '4',  '5',  '7',  '10', '9',  '2'],
    ['1', '3',  '11', '8',  '6',  '4',  '5',  '10', '7',  '9',  '2'],
    ['1', '3',  '11', '8',  '6',  '4',  '10', '5',  '7',  '9',  '2'],
    ['1', '3',  '11', '8',  '6',  '10', '4',  '5',  '7',  '9',  '2'],
    ['1', '3',  '11', '8',  '10', '6',  '4',  '5',  '7',  '9',  '2'],
    ['1', '3',  '11', '9',  '7',  '5',  '4',  '6',  '8',  '10', '2'],
    ['1', '10', '2',  '9',  '7',  '5',  '4',  '6',  '8',  '3',  '11'],
    ['1', '10', '2',  '9',  '7',  '5',  '4',  '6',  '8',  '11', '3'],
    ['1', '10', '2',  '9',  '7',  '5',  '4',  '6',  '11', '8',  '3'],
    ['1', '10', '2',  '9',  '7',  '5',  '4',  '11', '6',  '8',  '3'],
    ['1', '10', '2',  '9',  '7',  '5',  '11', '4',  '6',  '8',  '3'],
    ['1', '10', '2',  '9',  '7',  '11', '5',  '4',  '6',  '8',  '3'],
    ['1', '10', '2',  '9',  '11', '7',  '5',  '4',  '6',  '8',  '3'],
    ['1', '10', '2',  '11', '9',  '7',  '5',  '4',  '6',  '8',  '3'],
    ['1', '10', '3',  '8',  '6',  '4',  '5',  '7',  '9',  '2',  '11'],
    ['1', '10', '3',  '8',  '6',  '4',  '5',  '7',  '9',  '11', '2'],
    ['1', '10', '3',  '8',  '6',  '4',  '5',  '7',  '11', '9',  '2'],
    ['1', '10', '3',  '8',  '6',  '4',  '5',  '11', '7',  '9',  '2'],
    ['1', '10', '3',  '8',  '6',  '4',  '11', '5',  '7',  '9',  '2'],
    ['1', '10', '3',  '8',  '6',  '11', '4',  '5',  '7',  '9',  '2'],
    ['1', '10', '3',  '8',  '11', '6',  '4',  '5',  '7',  '9',  '2'],
    ['1', '10', '3',  '11', '8',  '6',  '4',  '5',  '7',  '9',  '2'],
    ['1', '10', '4',  '5',  '7',  '9',  '2',  '11', '6',  '8',  '3'],
    ['1', '10', '4',  '6',  '8',  '3',  '11', '5',  '7',  '9',  '2'],
    ['1', '10', '5',  '4',  '6',  '8',  '3',  '11', '7',  '9',  '2'],
    ['1', '10', '5',  '7',  '9',  '2',  '11', '4',  '6',  '8',  '3'],
    ['1', '10', '6',  '4',  '5',  '7',  '9',  '2',  '11', '8',  '3'],
    ['1', '10', '6',  '8',  '3',  '11', '4',  '5',  '7',  '9',  '2'],
    ['1', '10', '7',  '5',  '4',  '6',  '8',  '3',  '11', '9',  '2'],
    ['1', '10', '7',  '9',  '2',  '11', '5',  '4',  '6',  '8',  '3'],
    ['1', '10', '8',  '3',  '11', '6',  '4',  '5',  '7',  '9',  '2'],
    ['1', '10', '8',  '6',  '4',  '5',  '7',  '9',  '2',  '11', '3'],
    ['1', '10', '9',  '2',  '11', '7',  '5',  '4',  '6',  '8',  '3'],
    ['1', '10', '9',  '7',  '5',  '4',  '6',  '8',  '3',  '11', '2'],
    ['1', '11', '2',  '9',  '7',  '5',  '4',  '6',  '8',  '3',  '10'],
    ['1', '11', '2',  '9',  '7',  '5',  '4',  '6',  '8',  '10', '3'],
    ['1', '11', '2',  '9',  '7',  '5',  '4',  '6',  '10', '8',  '3'],
    ['1', '11', '2',  '9',  '7',  '5',  '4',  '10', '6',  '8',  '3'],
    ['1', '11', '2',  '9',  '7',  '5',  '10', '4',  '6',  '8',  '3'],
    ['1', '11', '2',  '9',  '7',  '10', '5',  '4',  '6',  '8',  '3'],
    ['1', '11', '2',  '9',  '10', '7',  '5',  '4',  '6',  '8',  '3'],
    ['1', '11', '2',  '10', '9',  '7',  '5',  '4',  '6',  '8',  '3'],
    ['1', '11', '3',  '8',  '6',  '4',  '5',  '7',  '9',  '2',  '10'],
    ['1', '11', '3',  '8',  '6',  '4',  '5',  '7',  '9',  '10', '2'],
    ['1', '11', '3',  '8',  '6',  '4',  '5',  '7',  '10', '9',  '2'],
    ['1', '11', '3',  '8',  '6',  '4',  '5',  '10', '7',  '9',  '2'],
    ['1', '11', '3',  '8',  '6',  '4',  '10', '5',  '7',  '9',  '2'],
    ['1', '11', '3',  '8',  '6',  '10', '4',  '5',  '7',  '9',  '2'],
    ['1', '11', '3',  '8',  '10', '6',  '4',  '5',  '7',  '9',  '2'],
    ['1', '11', '3',  '10', '8',  '6',  '4',  '5',  '7',  '9',  '2'],
    ['1', '11', '4',  '5',  '7',  '9',  '2',  '10', '6',  '8',  '3'],
    ['1', '11', '4',  '6',  '8',  '3',  '10', '5',  '7',  '9',  '2'],
    ['1', '11', '5',  '4',  '6',  '8',  '3',  '10', '7',  '9',  '2'],
    ['1', '11', '5',  '7',  '9',  '2',  '10', '4',  '6',  '8',  '3'],
    ['1', '11', '6',  '4',  '5',  '7',  '9',  '2',  '10', '8',  '3'],
    ['1', '11', '6',  '8',  '3',  '10', '4',  '5',  '7',  '9',  '2'],
    ['1', '11', '7',  '5',  '4',  '6',  '8',  '3',  '10', '9',  '2'],
    ['1', '11', '7',  '9',  '2',  '10', '5',  '4',  '6',  '8',  '3'],
    ['1', '11', '8',  '3',  '10', '6',  '4',  '5',  '7',  '9',  '2'],
    ['1', '11', '8',  '6',  '4',  '5',  '7',  '9',  '2',  '10', '3'],
    ['1', '11', '9',  '2',  '10', '7',  '5',  '4',  '6',  '8',  '3'],
    ['1', '11', '9',  '7',  '5',  '4',  '6',  '8',  '3',  '10', '2']
];

is_deeply(\@got_4, $expected_4,
          "Found Hamiltonian walks in graph 19203 (by Gunnar Brinkmann) from House of Graphs, as expected.");

# Local Variables:
# compile-command: "cd .. && perl t/28-house-of-graphs-lst-file-format.t"
# End:
