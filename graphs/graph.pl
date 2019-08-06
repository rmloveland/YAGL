#!/usr/bin/env perl

use strict;
use warnings;
use experimentals;
use Data::Dumper;
use Smart::Match;
use List::Util qw/ uniq /;
use Text::CSV;

sub build_graph {
    my ( $f, $graph ) = @_;

    my $csv = Text::CSV->new( { binary => 1 } );

    open my $fh, "<:encoding(utf8)", $f or die "$f: $!";

    while ( my $line = $csv->getline($fh) ) {
        my @cols     = @$line;
        my $node     = $cols[0];
        my $neighbor = $cols[1];

        add_neighbor( $node, [$neighbor], $graph );
    }

    remove_node( 'node', $graph );    # Bug workaround
}

sub find_index {

    # Naive linear search
    my ( $wanted, $graph ) = @_;

    my $i = 0;
    for my $elem (@$graph) {

        # Definedness check here is necessary because we delete elements
        # from the graph by setting the element's index to undef.  In
        # other words, some graph indices can be undef.
        my $head = $elem->[0];
        return $i if defined $head && $head eq $wanted;
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

sub remove_node {
    my ( $node, $graph ) = @_;

    my $index = find_index( $node, $graph );

    $graph->[$index] = undef;
}

sub get_nodes {
    ## Graph -> List
    my $graph = shift;
    my @nodes;
    for my $node (@$graph) {
        push @nodes, $node->[0];
    }
    @nodes;
}

sub to_graphviz {
    ## Graph -> String
    my $graph = shift;
    my @buffer;
    my %seen;

    push @buffer, qq[graph { ];

    for my $node (@$graph) {
        next unless defined $node->[0];
        push @buffer, $node->[0];
        push @buffer, qq[ -- ];
        push @buffer, qq[ { ];
        my $neighbors = $node->[1];
        for my $node (@$neighbors) {
            push @buffer, $node;
        }
        push @buffer, qq[ } ];
    }

    push @buffer, qq[ } ];

    return join ' ', @buffer;
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

    my @path;     # Path so far
    my @queue;    # Nodes still to visit.
    my %seen;     # Nodes already seen.

    push @queue, $current;
    push @path,  $current;
    $seen{$current}++;

    while (@queue) {
        my $v = pop @queue;
        $sub->($v);
        push @path, $v unless $v ~~ @path;
        $seen{$v}++;
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
    my $graph = [];

    die qq[Usage: $0 FILE\n] unless scalar @ARGV >= 1;

    my $file = shift @ARGV;
    build_graph( $file, $graph );

    my @nodes = get_nodes($graph);

    my $start = 's';
    my $end   = 'e';
    my @path  = walk_graph( $start, $end, sub { say uc $_[0] }, $graph );

    my $gv = to_graphviz($graph);

    open my $fh, '>', 'graph.dot' or die $!;
    say $fh $gv;
    close $fh;
}

main();
