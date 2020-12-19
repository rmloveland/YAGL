#!perl

use strict;
use warnings;
use lib 'lib';
use Test::More tests => 1;
use Cwd;
use YAGL;

# Test 1 - DFS on directed graph

my $g = YAGL->new(is_directed => 1);
my $cwd = getcwd;

$g->read_csv("$cwd/t/25-dfs-00.csv");

my @expected = qw/a b f e d g c j k l m/;

my @got;

$g->dfs('a', sub { push @got, $_[0] });

is_deeply(\@got, \@expected, "path as expected.");
