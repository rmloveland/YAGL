package TinyGraph;

use strict;
use warnings;
use experimentals;
use Data::Dumper;
use Smart::Match;
use Text::CSV;
use Hash::PriorityQueue;

use constant DEBUG => undef;

our $attrs = {};

sub new {
    my $self  = shift;
    my $graph = [];

    bless $graph, $self;
    return $graph;
}

sub build_graph_from_file {
    ## Filename ArrayRef HashRef? -> State! IO!
    my ( $self, $f ) = @_;

    my $csv = Text::CSV->new( { binary => 1 } );

    open my $fh, "<:encoding(utf8)", $f or die "$f: $!";

    while ( my $line = $csv->getline($fh) ) {
        my @cols     = @$line;
        my $node     = $cols[0];
        my $neighbor = $cols[1];
        my $weight   = $cols[2];

        next unless defined $weight;

        $self->add_neighbor( $node, [$neighbor] );

        if ($attrs) {
            $self->add_attribute( $node, $neighbor, { weight => $weight },
                $attrs );
        }
    }
}

sub find_index {
    ## Int ArrayRef -> Int OR undef
    my ( $self, $wanted ) = @_;

    # Naive linear search, for now.
    my $i = 0;
    for my $elem (@$self) {

        # Definedness check here is necessary because we delete
        # elements from the graph by setting the element's index to
        # undef.  In other words, some graph indices can be undef.
        if ( defined $elem->[0] && $elem->[0] eq $wanted ) {
            return $i;
        }
        $i++;
    }
    return;
}

sub get_neighbors {
    ## String ArrayRef -> ArrayRef
    my ( $self, $k ) = @_;

    my $index = $self->find_index($k);

    if ( defined $index ) {
        return $self->[$index]->[1];
    }
    else {
        return;
    }
}

sub remove_node {
    ## String ArrayRef -> State!
    my ( $self, $node ) = @_;

    my $index = $self->find_index($node);

    $self->[$index] = undef;
}

sub get_nodes {
    ## ArrayRef -> Array
    my $self = shift;
    my @nodes;
    for my $node (@$self) {
        push @nodes, $node->[0];
    }
    return @nodes;
}

sub to_graphviz {
    ## ArrayRef ArrayRef HashRef? -> String
    my ( $self, $path ) = @_;

    my @buffer;
    my %seen;

    push @buffer, qq[graph { ];

    for my $node (@$self) {
        next unless defined $node->[0];
        push @buffer, $node->[0];
        push @buffer, qq[ -- ];
        push @buffer, qq[ { ];
        my $neighbors = $node->[1];
        for my $node (@$neighbors) {
            if ( $node ~~ @$path ) {
                push @buffer, qq{$node [style=filled fillcolor=red]};
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

sub to_weighted_graphviz {
    ## ArrayRef ArrayRef HashRef? -> String
    my ( $self, $path ) = @_;

    my @buffer;
    my %seen;
    my @path;

    @path = map { $_->{node} } @$path;

    push @buffer, qq[graph {\n];

    for my $node (@$self) {
        next unless defined $node->[0];
        my $v         = $node->[0];
        my $neighbors = $node->[1];
        for my $neighbor (@$neighbors) {
            my $pairkey     = $v . $neighbor;
            my $edge_weight = $attrs->{$pairkey}->{weight};

            if ( $neighbor ~~ @path ) {
                push @buffer,
qq{$v -- $neighbor [label="$edge_weight"] $neighbor [style=filled, color=red];\n};
            }
            else {
                push @buffer, qq{$v -- $neighbor [label="$edge_weight"];\n};
            }
        }
    }

    push @buffer, qq[ \n} ];

    return join ' ', @buffer;
}

sub add_neighbor {
    ## String String ArrayRef -> State!
    my ( $self, $k, $v ) = @_;

    my $index = $self->find_index($k);

    if ( defined $index ) {
        my $neighbors = $self->[$index]->[1];
        for my $value (@$v) {
            push @$neighbors, $value;
        }
        $self->[$index]->[1] = $neighbors;
    }
    else {
        push @$self, [ $k, $v ];
    }
}

sub edge_between {
    ## String String ArrayRef -> Boolean
    my ( $self, $a, $b ) = @_;

    return unless ( defined $a && defined $b );
    return 1 if $a eq $b;

    my $neighbors = $self->get_neighbors($a);
    if ( $b ~~ @$neighbors ) {
        return 1;
    }
    else { return; }
}

sub dijkstra {
    my ( $self, $start, $end ) = @_;

    return () unless defined $start && defined $end;

    my @path;
    my @queue;
    my %seen;
    my $heap = Hash::PriorityQueue->new;
    my $st   = {};

    $st->{$start}->{distance} = 0;
    $st->{$start}->{prev}     = undef;

    for my $node ( $self->get_nodes ) {
        next if $node eq $start;
        $st->{$node}->{distance} = 1_000_000;
        $st->{$node}->{prev}     = undef;
    }

    $heap->insert( $start, $st->{$start}->{distance} );

    while ( my $v = $heap->pop() ) {

        say q[-] x 68 if DEBUG;
        say qq[Looking at node '$v' with current distance: ],
          $st->{$v}->{distance}
          if DEBUG;

        my $neighbors = $self->get_neighbors($v);

        say qq[Neighbors list: ], Dumper $neighbors if DEBUG;

        for my $neighbor (@$neighbors) {

            next if $seen{$neighbor};
            $seen{$neighbor}++;

            say
              qq[Looking at neighbor '$neighbor' with current distance: ],
              $st->{$neighbor}->{distance}
              if DEBUG;

            my $v_distance        = $st->{$v}->{distance};
            my $neighbor_distance = $st->{$neighbor}->{distance};
            my $edge_weight       = $attrs->{ $v . $neighbor }->{weight};
            my $maybe_new_neighbor_distance = $v_distance + $edge_weight;

            say Dumper {
                v                           => $v,
                neighbor                    => $neighbor,
                v_distance                  => $v_distance,
                neighbor_distance           => $neighbor_distance,
                edge_weight                 => $edge_weight,
                maybe_new_neighbor_distance => $maybe_new_neighbor_distance,
              }
              if DEBUG;

            if ( $maybe_new_neighbor_distance < $neighbor_distance ) {
                my $old_distance = $st->{$neighbor}->{distance};
                $st->{$neighbor}->{distance} = $maybe_new_neighbor_distance;
                $st->{$neighbor}->{prev}     = $v;
                say
qq[Updated distance of neighbor '$neighbor' from $old_distance to ],
                  $st->{$neighbor}->{distance}
                  if DEBUG;
                delete $seen{$neighbor};
            }

            if ( $neighbor eq $end ) {
                @path = $self->st_weighted_walk( $start, $end, $st );
                return @path;
            }
            else {
                $heap->insert( $neighbor, $st->{$neighbor}->{distance} );
            }
        }

        $seen{$v}++;
    }
    return ();
}

sub st_weighted_walk {
    my ( $self, $start, $end, $st ) = @_;

    my @path;
    push @path, { node => $end, distance => $st->{$end}->{distance} };

    my $prev = $st->{$end}->{prev};
    while (1) {
        if ( $prev eq $start ) {

            push @path, { node => $prev, distance => $st->{$prev}->{distance} };
            last;
        }

        push @path, { node => $prev, distance => $st->{$prev}->{distance} };
        $prev = $st->{$prev}->{prev};
        next;
    }
    return reverse @path;
}

sub find_path_between {
    ## String String ArrayRef -> Array
    my ( $self, $start, $end ) = @_;

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

        my $neighbors = $self->get_neighbors($v);

        for my $neighbor (@$neighbors) {
            next if $seen{$neighbor};
            $self->st_add( $v, $neighbor, $st );
            if ( $neighbor eq $end ) {
                $found++;
                @path = $self->st_walk( $start, $end, $st );
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
    ## String String HashRef -> Array
    my ( $self, $start, $end, $st ) = @_;

    my @path;

    push @path, $end;
    my $prev = $st->{$end}->{prev};
    while (1) {
        if ( $prev eq $start ) {
            push @path, $start;
            last;
        }
        push @path, $prev;
        $prev = $st->{$prev}->{prev};
        next;
    }
    return reverse @path;
}

sub st_add {
    ## String String HashRef -> State!
    my ( $self, $node, $neighbor, $st ) = @_;
    $st->{$node}->{$neighbor} = 1;      # Possibly unnecessary.
    $st->{$neighbor}->{prev} = $node;
}

sub get_attributes {
    my ( $self, $start, $end, $attrs ) = @_;

    my $pairkey = $start . $end;
    return $attrs->{$pairkey};
}

sub add_attribute {
    ## String String HashRef -> State!
    # add_attribute('s', 'a', { weight => 12 });
    my ( $self, $start, $end, $new_attrs, $attrs ) = @_;

    my $pairkey1 = $start . $end;
    my $pairkey2 = $end . $start;

    # Attributes hashref already exists, so we add to it.  NOTE:
    # this is a hash so the update is destructive.
    for ( my ( $k, $v ) = each %$new_attrs ) {
        $attrs->{$pairkey1}->{$k} = $v;
        $attrs->{$pairkey2}->{$k} = $v;
    }
}

1;
