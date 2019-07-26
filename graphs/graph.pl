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

sub get_neighbor {
    my ( $k, $graph ) = @_;

    my $index = find_index( $k, $graph );

    if ( defined $index ) {
        return $graph->[$index];
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

    say STDERR qq[FOUND INDEX for $k at $index] if $index;

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

    say Dumper $graph;
}

main();
