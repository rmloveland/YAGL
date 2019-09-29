#!perl

use strict;
use warnings;
use experimentals;
use lib '.';
use Test::More tests => 14;
use TinyGraph;

my @edges = (
    [ 's', 'a', { weight => 560 } ],
    [ 's', 'd', { weight => 529 } ],
    [ 'a', 'b', { weight => 112 } ],
    [ 'a', 'd', { weight => 843 } ],
    [ 'b', 'c', { weight => 690 } ],
    [ 'b', 'e', { weight => 891 } ],
    [ 'd', 'e', { weight => 492 } ],
    [ 'e', 'f', { weight => 35 } ],
);

my $g = TinyGraph->new;
$g->add_edges(@edges);

my $h = TinyGraph->new;
$h->add_edges(@edges);

# --------------------------------------------------------------------
# Are the graphs the same? (In the mathematical sense, not the
# "pointer to an object" sense.  We also test that the check works as
# expected in both directions.

my $equals_1 = $g->equals($h);
my $equals_2 = $h->equals($g);

ok( $equals_1 == 1, "G->equals(H) works as expected." );
ok( $equals_2 == 1, "H->equals(G) works as expected." );

# --------------------------------------------------------------------
# If we delete a vertex from one graph, it should not be equal
# anymore.  We also test that the check works as expected in both
# directions.

$g->remove_vertex('s');

my $equals_3 = $g->equals($h);
my $equals_4 = $h->equals($g);

ok( !defined $equals_3,
    "G->equals(H) fails as expected after removing a vertex from G." );
ok( !defined $equals_4,
    "H->equals(G) fails as expected after removing a vertex from G." );

# --------------------------------------------------------------------
# A graph K that is a subgraph of H should cause a test failure.

my @edges2 = (
    [ 's', 'a', { weight => 560 } ],
    [ 's', 'd', { weight => 529 } ],
    [ 'a', 'b', { weight => 112 } ],
    [ 'a', 'd', { weight => 843 } ],
);

my $k = TinyGraph->new;
$k->add_edges(@edges2);

my $k_eq_h = $k->equals($h);
my $h_eq_k = $h->equals($k);

ok( !defined $k_eq_h,
    "A graph K that is a subgraph of H fails as expected in K->equals(H)." );
ok( !defined $h_eq_k,
    "A graph K that is a subgraph of H fails as expected in H->equals(K)." );

# --------------------------------------------------------------------
# If we delete an edge from a graph HH attributes are different from H should fail
# the equality test.

my $hh = TinyGraph->new;
$hh->add_edges(@edges);

# First, we make sure it is truly the same as H.

my $hh_eq_h = $hh->equals($h);
my $h_eq_hh = $h->equals($hh);

ok(
    $hh_eq_h == 1,
"HH->equals(H) should pass, since they have the same sets of vertices and edges."
);
ok(
    $h_eq_hh == 1,
"H->equals(HH) should pass, since they have the same sets of vertices and edges."
);

# --------------------------------------------------------------------
# Next, we delete an edge from HH.  Now the tests should fail.

$hh->remove_edge( 's', 'a' );

my $hh_eq_h_now = $hh->equals($h);
my $h_eq_hh_now = $h->equals($hh);

ok( !defined $hh_eq_h_now,
    "HH->equals(H) should fail since we deleted an edge from HH." );
ok( !defined $h_eq_hh_now,
    "H->equals(HH) should fail since we deleted an edge from HH." );

# --------------------------------------------------------------------
# In this test we want to ensure that edge attributes are being tested
# for equality across copied objects correctly.

# First, we establish a baseline with equal objects.

my $hhh = $hh->clone;

ok( $hh->equals($hhh) == 1, "HH->equals(HHH) works as expected." );
ok( $hhh->equals($hh) == 1, "HHH->equals(HH) works as expected." );

# Now, we change some edge weights on one object, and test them again.
# The resulting equality tests should fail in both directions.

$hhh->set_edge_attribute( 's', 'a', { weight => 123 } );

ok( !defined $hh->equals($hhh),
    "HH->equals(HHH) fails as expected after changing edge attributes." );
ok( !defined $hhh->equals($hh),
    "HHH->equals(HH) fails as expected after changing edge attributes." );
