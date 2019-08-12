#!/usr/bin/env perl

use strict;
use warnings;
use experimentals;
use Data::Dumper;
use Smart::Match;
use List::Util qw/ uniq /;
use Text::CSV;

use constant DEBUG => undef;

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
    ## Graph, Aref -> String
    my $graph = shift;
    my $path  = shift;

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
            if ( $node ~~ @$path ) {
                push @buffer, qq{$node [color=red]};
            }
            else {
                push @buffer, $node;
            }
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

    return unless ( defined $a && defined $b );
    return 1 if $a eq $b;

    my $neighbors = get_neighbors( $a, $graph );
    if ( $b ~~ @$neighbors ) {
        return 1;
    }
    else { return; }
}

sub find_path_between {
    my ( $start, $end, $graph ) = @_;

    return () unless defined $start && defined $end;

    my @path;     # Path so far
    my @queue;    # Nodes still to visit.
    my %seen;     # Nodes already seen.
    my $found;    # Whether we have found the wanted node.
    my $st = {};  # Spanning tree, used to find paths.

    if ( $start eq $end ) {
        push @path, $start;
        return @path;
    }

    push @queue, $start;
    $seen{$start}++;

    while (@queue) {

        my $v = shift @queue;

        my $neighbors = get_neighbors( $v, $graph );

        for my $neighbor (@$neighbors) {
            next if $seen{$neighbor};
            st_add( $v, $neighbor, $st );
            if ( $neighbor eq $end ) {
                $found++;
                @path = st_walk( $start, $end, $st );
                return @path;
            }
            else {
                push @queue, $neighbor;
            }
            $seen{$neighbor}++;
        }
    }
    return $found ? @path : ();
}

sub st_walk {
    my ( $start, $end, $st ) = @_;

    my @path;

    push @path, $end;
    my $prev = $st->{$end}->{prev};
    while (1) {
        push @path, $prev;
        $prev = $st->{$prev}->{prev};
        if ( $prev eq $start ) {
            push @path, $start;
            last;
        }
        next;
    }
    return reverse @path;
}

sub st_add {
    ## String String HashRef -> State!
    my ( $node, $neighbor, $st ) = @_;    # Possibly unnecessary.
    $st->{$node}->{$neighbor} = 1;
    $st->{$neighbor}->{prev} = $node;
}

sub main {
    my $graph = [];

    die qq[Usage: $0 FILE\n] unless scalar @ARGV >= 1;

    my $file = shift @ARGV;
    build_graph( $file, $graph );

    my @nodes = get_nodes($graph);

    my $i     = int rand @nodes;
    my $j     = int rand @nodes;
    my $start = $nodes[$i];
    my $end   = $nodes[$j];

    say qq[I NEED A PATH FROM '$start' TO '$end'];
    my @path = find_path_between( $start, $end, $graph );
    say qq[FINAL PATH: ], Dumper \@path;

    my $gv = to_graphviz($graph);

    open my $fh, '>', 'graph.dot' or die $!;
    say $fh $gv;
    close $fh;
}

main();

__END__

perl graph.pl graph.csv.bak
I NEED A PATH FROM 'c' TO 'f'
FINAL PATH: $VAR1 = [
          'c',
          'b',
          'a',
          's',
          'd',
          'e',
          'f'
        ];
