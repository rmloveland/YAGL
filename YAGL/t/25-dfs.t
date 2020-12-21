#!perl

use strict;
use warnings;
use lib 'lib';
use Test::More tests => 3;
use Cwd;
use YAGL;

my $cwd = getcwd;

# Test 1 - DFS on directed graph

my $g = YAGL->new(is_directed => 1);
$g->read_csv("$cwd/t/25-dfs-00.csv");
my @expected = qw/a b f e d g c j k l m/;
my @got;
$g->dfs('a', sub { push @got, $_[0] });

is_deeply(\@got, \@expected, "DFS on a directed graph works as expected.");

# Test 2 - DFS on an undirected graph (that also has a Hamiltonian path, btw)

my $h = YAGL->new;
$h->read_csv("$cwd/t/25-dfs-01.csv");

my @expected2 = qw/a b c d e f g h i j k/;
my @got2;
$h->dfs('a', sub { push @got2, $_[0] });

is_deeply(\@got2, \@expected2,
    "DFS on an undirected graph works as expected");

# Test 3 - DFS on the graph in Fig. 44-1 from Sedgewick, 2nd ed.

my $gg = YAGL->new;
$gg->read_csv("$cwd/t/25-dfs-02.csv");

my @expected3 = qw/a b c e f d g h i k j l m/;
my @got3;
$gg->dfs('a', sub { push @got3, $_[0] });

is_deeply(\@got3, \@expected3,
    "DFS on the graph from Fig. 44-1 in Sedgewick, 2nd ed. works as expected"
);

# Local Variables:
# compile-command: "cd .. && perl t/25-dfs.t"
# End:
