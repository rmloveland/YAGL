#!/usr/bin/env perl

use strict;
use warnings;
use experimentals;
use Smart::Match;
use Data::Dumper;

my $n        = 18;     # Number of nodes
my $p        = 0.3;    # Probability of 2 nodes NOT being connected.
my $max_dist = 100;    # Maximum distance between nodes.

my %seen;

sub make_node_name {
    my $n     = int rand 10000;
    my $chars = qq[a b c d e f g h i j k l m n o p q r s t u v w x y z];
    my @chars = split / /, $chars;

    my $i  = rand scalar @chars;
    my $c1 = $chars[$i];
    my $c2 = $chars[ rand scalar @chars ];

    return qq[$c1$c2$n];
}

for my $node ( 1 .. $n ) {
    my $name = make_node_name();
    redo if $seen{$name};
    $seen{$name}++;

    # say $name;
}

my @nodes = keys %seen;

my @pairs;

for my $node (@nodes) {
    my $maybe_neighbor = $nodes[ rand $#nodes ];
    next if $maybe_neighbor eq $node;
    my $connection_prob = rand 1;
    my $dist            = int rand $max_dist;
    if ( $connection_prob > $p ) {
        push @pairs, [ $node,           $maybe_neighbor, $dist ];
        push @pairs, [ $maybe_neighbor, $node,           $dist ];
    }
    redo
      if rand 1 > 0.8;    # Sometimes, add more neighbors to this node.
}

say qq[node,neighbor];
for my $pair (@pairs) {
    say $pair->[0], ",", $pair->[1], ",", $pair->[2];
}

__END__

node,neighbor
s,a
s,d
a,s
a,b
a,d
b,a
b,c
b,e
c,b
d,s
d,a
d,e
e,b
e,d
e,f
f,e
x,s
x,y
x,z
f,u
f,u
f,u
