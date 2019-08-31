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

sub write_graph_to_csv_file {
    ## Filename -> State! IO!
    my ( $self, $f ) = @_;

    open my $fh, '>:encoding(utf8)', $f or die "Can't open file '$f': $!\n";

    say $fh qq[node,neighbor,weight];

    my @vertices = $self->get_vertices;

    for my $vertex (@vertices) {
        my $neighbors = $self->get_neighbors($vertex);
        for my $neighbor (@$neighbors) {
            my $weight = $attrs->{ $vertex . $neighbor }->{weight} || 0;
            my @cols   = ( $vertex, $neighbor, $weight );
            my $line   = join ',', @cols;
            say $fh $line;
        }
    }
    close $fh;
}

sub read_graph_from_csv_file {
    ## Filename -> State! IO!
    my ( $self, $f ) = @_;

    my $csv = Text::CSV->new( { binary => 1 } );

    open my $fh, "<:encoding(utf8)", $f or die "Can't open file '$f': $!\n";

    while ( my $line = $csv->getline($fh) ) {
        my @cols     = @$line;
        my $vertex   = $cols[0];
        my $neighbor = $cols[1];
        my $weight   = $cols[2];

        next unless defined $weight;
        next if $vertex eq 'node';

        $self->_add_neighbor( $vertex, [$neighbor] );

        if ($attrs) {
            $self->set_attribute( $vertex, $neighbor, { weight => $weight } );
        }
    }
}

sub _find_index {
    ## Int -> Int OR undef
    my ( $self, $wanted ) = @_;

    return unless defined $wanted;

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
    ## String -> ArrayRef
    my ( $self, $vertex ) = @_;

    my $index = $self->_find_index($vertex);

    if ( defined $index ) {
        return $self->[$index]->[1];
    }
    else {
        return;
    }
}

sub is_empty {
    ## -> Boolean
    my $self     = shift;
    my $count    = 0;
    my @vertices = $self->get_vertices;

    my $return = 1;

    for my $vertex (@vertices) {
        if ( defined $vertex ) {
            undef $return;
        }
    }
    return $return;
}

sub has_vertex {
    ## String -> Boolean
    my ( $self, $vertex ) = @_;

    return $self->_find_index($vertex);
}

sub remove_vertex {
    ## String -> State!
    my ( $self, $vertex ) = @_;

    my $neighbors = $self->get_neighbors($vertex);
    my $subname   = qq[remove_vertex()];

    for my $neighbor (@$neighbors) {
        my $neighbor_index = $self->_find_index($neighbor);
        next unless defined $neighbor_index;

        my $neighbor_neighbors = $self->[$neighbor_index]->[1];

        # Iterate over neighbors' adjacency lists, deleting mention of
        # this vertex.
        for ( my $i = 0 ; $i < @$neighbor_neighbors ; $i++ ) {
            my $neighbor_neighbor = $self->[$neighbor_index]->[1]->[$i];
            next unless defined $neighbor_neighbor;
            if ( $neighbor_neighbor eq $vertex ) {
                $self->[$neighbor_index]->[1]->[$i] = undef;
            }
        }
    }

    my $index = $self->_find_index($vertex);
    if ( defined $index ) {
        $self->[$index] = undef;
    }
}

sub get_vertices {
    ## -> Array
    my $self = shift;
    my @vertices;
    for my $vertex (@$self) {
        push @vertices, $vertex->[0];
    }
    return @vertices;
}

sub to_graphviz {
    ## ArrayRef -> String
    my ( $self, $path ) = @_;

    my @buffer;
    my %seen;

    push @buffer, qq[graph { ];

    for my $vertex (@$self) {
        next unless defined $vertex->[0];
        push @buffer, $vertex->[0];
        push @buffer, qq[ -- ];
        push @buffer, qq[ { ];
        my $neighbors = $vertex->[1];
        for my $vertex (@$neighbors) {
            if ( $vertex ~~ @$path ) {
                push @buffer, qq{$vertex [style=filled fillcolor=red]};
            }
            else {
                push @buffer, $vertex;
            }
        }
        push @buffer, qq[ } ];
    }

    push @buffer, qq[ } ];

    return join ' ', @buffer;
}

sub to_weighted_graphviz {
    ## ArrayRef -> String
    my ( $self, $path ) = @_;

    my @buffer;
    my %seen;
    my @path;

    @path = map { $_->{vertex} } @$path;

    push @buffer, qq[graph {\n];

    for my $vertex (@$self) {
        next unless defined $vertex->[0];
        my $v         = $vertex->[0];
        my $neighbors = $vertex->[1];
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

sub _add_neighbor {
    ## String ArrayRef HashRef -> State!
    my ( $self, $vertex, $neighbor, $data ) = @_;

    my $index = $self->_find_index($vertex);

    unless ( ref($neighbor) eq 'ARRAY' ) {
        my ( $package, $filename, $line ) = caller();
        die <<"EOF";
on line $line of file $filename:
  $package\:\:_add_neighbor('$vertex', '$neighbor', '$data'):
    expected arrayref, got '$neighbor'
EOF
    }

    if ( defined $index ) {
        my $neighbors = $self->[$index]->[1];
        for my $value (@$neighbor) {
            push @$neighbors, $value;
        }
        $self->[$index]->[1] = $neighbors;
    }
    else {
        push @$self, [ $vertex, $neighbor ];
    }

    if ($data) {
        $self->set_attribute( $vertex, $neighbor->[0], $data );
    }
}

sub edge_between {
    ## String String -> Boolean
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
    ## String String -> Array
    my ( $self, $start, $end ) = @_;

    return () unless defined $start && defined $end;

    my @path;
    my @queue;
    my %seen;
    my $heap = Hash::PriorityQueue->new;
    my $st   = {};

    $st->{$start}->{distance} = 0;
    $st->{$start}->{prev}     = undef;

    for my $vertex ( $self->get_vertices ) {
        next if $vertex eq $start;
        $st->{$vertex}->{distance} = 1_000_000;
        $st->{$vertex}->{prev}     = undef;
    }

    $heap->insert( $start, $st->{$start}->{distance} );

    while ( my $v = $heap->pop() ) {

        say q[-] x 68 if DEBUG;
        say qq[Looking at vertex '$v' with current distance: ],
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
                @path = $self->_st_weighted_walk( $start, $end, $st );
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

sub _st_weighted_walk {
    ## String String HashRef -> Array
    my ( $self, $start, $end, $st ) = @_;

    my @path;
    push @path, { vertex => $end, distance => $st->{$end}->{distance} };

    my $prev = $st->{$end}->{prev};
    while (1) {
        if ( $prev eq $start ) {

            push @path,
              { vertex => $prev, distance => $st->{$prev}->{distance} };
            last;
        }

        push @path, { vertex => $prev, distance => $st->{$prev}->{distance} };
        $prev = $st->{$prev}->{prev};
        next;
    }
    return reverse @path;
}

sub find_path_between {
    ## String String -> Array
    my ( $self, $start, $end ) = @_;

    return () unless defined $start && defined $end;

    my @path;     # Path so far
    my @queue;    # Vertices still to visit.
    my %seen;     # Vertices already seen.
    my $found;    # Whether we have found the wanted vertex.
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
            next unless defined $neighbor;
            next if $seen{$neighbor};
            $self->_st_add( $v, $neighbor, $st );
            if ( $neighbor eq $end ) {
                $found++;
                @path = $self->_st_walk( $start, $end, $st );
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

sub _st_walk {
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

sub _st_add {
    ## String String HashRef -> State!
    my ( $self, $vertex, $neighbor, $st ) = @_;
    $st->{$vertex}->{$neighbor} = 1;      # Possibly unnecessary.
    $st->{$neighbor}->{prev} = $vertex;
}

sub get_attributes {
    ## String String -> HashRef OR undef
    my ( $self, $start, $end ) = @_;
    my $pairkey = $start . $end;
    return $attrs->{$pairkey};
}

sub get_attribute {
    ## String String String -> Value OR undef
    my ( $self, $start, $end, $attribute ) = @_;

    my $pairkey = $start . $end;
    return $attrs->{$pairkey}->{$attribute};
}

sub set_attribute {
    ## String String HashRef -> State!
    # set_attribute('s', 'a', { weight => 12 });
    my ( $self, $start, $end, $new_attrs ) = @_;

    my $pairkey1 = $start . $end;
    my $pairkey2 = $end . $start;

    # Attributes hashref already exists, so we add to it.  NOTE:
    # this is a hash so the update is destructive.
    for ( my ( $k, $v ) = each %$new_attrs ) {
        next unless defined $k;
        next if $k eq '';
        $attrs->{$pairkey1}->{$k} = $v;
        $attrs->{$pairkey2}->{$k} = $v;
    }
}

sub is_complete {
    my $self = shift;

    my @vertices = $self->get_vertices;
    my $v        = pop @vertices;

    my $neighbors = $self->get_neighbors($v);

    @vertices = sort { ( $a || '' ) cmp( $b || '' ) } @vertices;
    my @neighbors = sort { ( $a || '' ) cmp( $b || '' ) } @$neighbors;

    return 1 if @vertices ~~ @neighbors;

    return;
}

sub add_vertex {
    ## String -> State!
    my ( $self, $vertex ) = @_;
    push @$self, [ $vertex, [] ];
}

sub add_edge {
    ## String String -> State!
    my ( $self, $v1, $v2, $attrs ) = @_;
    $self->_add_neighbor( $v1, [$v2], $attrs );
    $self->_add_neighbor( $v2, [$v1], $attrs );
}

}

sub generate_random_vertices {
    ## HashRef -> State!
    my ( $self, $args ) = @_;

    my $n          = $args->{n};
    my $p          = $args->{p};
    my $max_weight = $args->{max_weight};

    my %seen;

    for my $node ( 1 .. $n ) {
        my $name = $self->_make_vertex_name;
        redo if $seen{$name};
        $seen{$name}++;
    }

    my @nodes = keys %seen;

    my @pairs;

    for my $node (@nodes) {
        my $maybe_neighbor = $nodes[ rand $#nodes ];
        next if $maybe_neighbor eq $node;
        my $connection_prob = rand 1;
        my $dist            = int rand $max_weight;
        if ( $connection_prob > $p ) {
            push @pairs, [ $node,           $maybe_neighbor, $dist ];
            push @pairs, [ $maybe_neighbor, $node,           $dist ];
        }
        redo
          if rand 1 > 0.8;    # Sometimes, add more neighbors to this node.
    }

    for my $pair (@pairs) {
        $self->_add_neighbor(
            $pair->[0],
            [ $pair->[1] ],
            { weight => $pair->[2] }
        );
    }
}

sub _make_vertex_name {
    ## -> String
    my $n     = int rand 10000;
    my $chars = qq[a b c d e f g h i j k l m n o p q r s t u v w x y z];
    my @chars = split / /, $chars;

    my $i  = rand scalar @chars;
    my $c1 = $chars[$i];
    my $c2 = $chars[ rand scalar @chars ];

    return qq[$c1$c2$n];
}

1;
