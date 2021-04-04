#!perl

use strict;
use warnings;
use lib 'lib';
use Test::More tests => 1;
use YAGL;
use Cwd;

my $cwd = getcwd;

my $g = YAGL->new;
$g->read_csv("$cwd/t/26-connected-components-00.csv");

# TODO(rml): Design a better output format.  This should probably be
# returning YAGL objects so the caller can do graph operations on them
# if they want to.
my @expected
  = ('XXX', 'b', 'a', 'f', 'e', 'd', 'g', 'c', 'XXX', 'j', 'k', 'l', 'm');
my @got = $g->connected_components;

is_deeply(\@got, \@expected,
    "Finding connected components of an unconnected graph");

# Local Variables:
# compile-command: "cd .. && perl t/26-connected-components.t"
# End:
