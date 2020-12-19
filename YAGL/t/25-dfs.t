#!perl

use strict;
use warnings;
use lib 'lib';
use Test::More tests => 2;
use Cwd;
use YAGL;

# Test 1 - DFS on directed graph

my $g = YAGL->new(is_directed => 1);
my $cwd = getcwd;

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
