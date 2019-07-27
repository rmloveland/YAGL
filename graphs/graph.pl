#!/usr/bin/env perl

use strict;
use warnings;
use experimentals;
use Data::Dumper;
use Smart::Match;
use List::Util qw/ uniq /;

my $graph = [
    [ 'f', ['e'] ],
    [ 'e', [ 'b', 'd', 'f' ] ],
    [ 'd', [ 's', 'a', 'e' ] ],
    [ 'c', ['b'] ],
    [ 'b', [ 'a', 'c', 'e' ] ],
    [ 'a', [ 's', 'b', 'd' ] ],
    [ 's', [ 'a', 'd' ] ],
];

$graph = [];

sub find_index {

    # Naive linear search
    my ( $wanted, $graph ) = @_;

    my $i = 0;
    for my $elem (@$graph) {
        my $head = $elem->[0];
        return $i if $head eq $wanted;
        $i++;
    }
    return;
}

sub get_neighbors {
    my ( $k, $graph ) = @_;

    my $index = find_index( $k, $graph );

    if ( defined $index ) {
        return $graph->[$index]->[1];
    }
    else {
        return;
    }
}

sub add_neighbor {
    ## String String -> State!
    my ( $k, $v, $graph ) = @_;

    # 1. Check if the key already exists
    my $index = find_index( $k, $graph );

    if ($index) {
        my $neighbors = $graph->[$index]->[1];
        for my $value (@$v) {
            push @$neighbors, $value;
        }
        $graph->[$index]->[1] = $neighbors;
    }
    else {
        push @$graph, [ $k, $v ];
    }
}

sub edge_between {
    my ( $a, $b, $graph ) = @_;

    my $neighbors = get_neighbors( $a, $graph );
    if ( $b ~~ @$neighbors ) {
        return 1;
    }
    else { return; }
}

sub walk_graph {
    my ( $current, $wanted, $sub, $graph ) = @_;

    state @queue;    # Nodes still to visit.
    state @path;
    state %seen;     # Nodes already seen.

    push @queue, $current;
    push @path,  $current;
    $seen{$current}++;

    while (@queue) {
        my $v = pop @queue;
        $sub->($v);
        push @path, $v unless $v ~~ @path;
        my $neighbors = get_neighbors( $v, $graph );
        for my $neighbor (@$neighbors) {
            if ( $neighbor eq $wanted ) {
                $sub->($neighbor);
                push @path, $neighbor;
                return @path;
            }
            push @queue, $neighbor unless $seen{$neighbor};
        }
    }
    return @path;
}

sub main {

    # add_neighbor( 's', [ 'b', 'e' ], $graph );

    add_neighbor( 's', [ 'a', 'd' ], $graph );
    add_neighbor( 'a', [ 's', 'b', 'd' ], $graph );
    add_neighbor( 'b', [ 'a', 'c', 'e' ], $graph );
    add_neighbor( 'c', ['b'], $graph );
    add_neighbor( 'd', [ 's', 'a', 'e' ], $graph );
    add_neighbor( 'e', [ 'b', 'd', 'f' ], $graph );
    add_neighbor( 'f', ['e'], $graph );

    add_neighbor( 'x', [ 'y', 'z' ], $graph );    # Add new item to graph

    add_neighbor( 'f', [ 'u', 'u', 'u' ], $graph ); # Update existing graph node

    # say Dumper $graph;

    my @path = walk_graph( 's', 'f', sub { say uc $_[0] }, $graph );

}

main();
