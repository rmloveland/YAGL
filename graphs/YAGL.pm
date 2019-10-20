package YAGL;

use strict;
use warnings;
use experimentals;
use Smart::Match;
use Text::CSV;
use GraphViz;
use Hash::PriorityQueue;
use Storable;

=head1 YAGL - Yet Another Graph Library

=head2 GRAPH INITIALIZATION AND RANDOMIZATION

=cut

sub new {
    my $self  = shift;
    my $graph = {};

    $graph->{_INTERNAL}->{edge_attrs}   = {};
    $graph->{_INTERNAL}->{vertex_attrs} = {};

    bless $graph, $self;
    return $graph;
}

sub generate_random_vertices {
    ## HashRef -> State!
    my ( $self, $args ) = @_;

    my $n = $args->{n};

    # As we loop through the nodes, for each node A, this is the
    # probability that another randomly selected node B is NOT
    # connected to A.  In other words, this is the probability that
    # there is NOT an edge A-B.
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

=head2 GRAPH SERIALIZATION

=cut

sub write_csv {
    ## Filename -> State! IO!
    my ( $self, $f ) = @_;

    open my $fh, '>:encoding(utf8)', $f or die "Can't open file '$f': $!\n";

    say $fh qq[node,neighbor,weight];

    my @vertices = $self->get_vertices;

    for my $vertex (@vertices) {
        my $neighbors = $self->get_neighbors($vertex);
        for my $neighbor (@$neighbors) {
            my $weight =
              $self->get_edge_attribute( $vertex, $neighbor, 'weight' ) || 0;
            my @cols = ( $vertex, $neighbor, $weight );
            my $line = join ',', @cols;
            say $fh $line;
        }
    }
    close $fh;
}

sub read_csv {
    ## Filename -> State! IO!
    my ( $self, $f ) = @_;

    my $csv = Text::CSV->new( { binary => 1 } );

    open my $fh, "<:encoding(utf8)", $f or die "Can't open file '$f': $!\n";

    while ( my $line = $csv->getline($fh) ) {
        my @cols     = @$line;
        my $vertex   = $cols[0];
        my $neighbor = $cols[1];
        my $weight   = $cols[2];

        next if $vertex eq 'node';

        $self->_add_neighbor( $vertex, [$neighbor] );
        $self->set_edge_attribute( $vertex, $neighbor, { weight => $weight } );
    }
}

sub to_graphviz {
    ## ArrayRef -> String
    my ( $self, $path ) = @_;
    my $gv = GraphViz->new( directed => 0 );

    my %seen;
    for my $vertex ( $self->get_vertices ) {
        $gv->add_node( $vertex, style => 'filled' );
        my $neighbors = $self->get_neighbors($vertex);
        for my $neighbor (@$neighbors) {
            $gv->add_node($neighbor);
            $gv->add_edge( $vertex, $neighbor )
              unless $seen{ $vertex . $neighbor };
            if ( $neighbor ~~ @$path ) {
                $gv->add_node( $neighbor, fillcolor => 'red' );
            }
            $seen{ $neighbor . $vertex }++;
            $seen{ $vertex . $neighbor }++;
        }
    }

    return $gv->as_canon;
}

sub to_weighted_graphviz {
    ## ArrayRef -> String
    my ( $self, $path ) = @_;

    my %seen;
    my @path = map { $_->{vertex} } @$path;

    my $gv = GraphViz->new( directed => 0, style => 'filled' );

    for my $vertex ( $self->get_vertices ) {
        $gv->add_node( $vertex, style => 'filled' );
        my $neighbors = $self->get_neighbors($vertex);
        for my $neighbor (@$neighbors) {
            my $edge_weight =
              $self->get_edge_attribute( $vertex, $neighbor, 'weight' );
            $gv->add_node($neighbor);
            $gv->add_edge( $vertex, $neighbor, label => $edge_weight )
              unless $seen{ $vertex . $neighbor };
            if ( $neighbor ~~ @path ) {
                $gv->add_node( $neighbor, fillcolor => 'red' );
            }
            $seen{ $neighbor . $vertex }++;
            $seen{ $vertex . $neighbor }++;
        }
    }

    return $gv->as_canon;
}

=head2 BOOLEAN METHODS

=cut

sub is_empty {
    ## -> Boolean
    my $self     = shift;
    my @vertices = $self->get_vertices;

    if ( scalar @vertices >= 1 ) {
        return;
    }
    else {
        return 1;
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

sub is_tree {
    my ($self) = @_;

    return unless $self->is_connected;

    my @e = $self->get_edges;
    my @v = $self->get_vertices;

    my $e = @e;
    my $v = @v;

    return unless $e == $v - 1;
}

sub is_connected {
    my ($self) = @_;

    my @vertices = $self->get_vertices;

    my $start = pop @vertices;

    for my $v (@vertices) {
        return unless $self->find_path_between( $start, $v );
    }
    return 1;
}

sub has_cycle {
    my ($self) = @_;
    return $self->is_tree ? undef : 1;
}

sub is_weighted {
    ## -> Number
    my ($self) = @_;

    my @vertices = $self->get_vertices;
    my $sum;
    my %seen;
  VERTEX: for my $vertex (@vertices) {
        my $neighbors = $self->get_neighbors($vertex);
        for my $neighbor (@$neighbors) {
            next VERTEX if $seen{ $vertex . $neighbor };
            next VERTEX if $seen{ $neighbor . $vertex };
            $sum += $self->get_edge_attribute( $vertex, $neighbor, 'weight' );
            $seen{ $vertex . $neighbor }++;
            $seen{ $neighbor . $vertex }++;
        }
    }
    return $sum;
}

sub is_colored {
    ## -> Number
    my ($self) = @_;
    my @vertices = $self->get_vertices;
    my @colors = grep { $self->get_vertex_color($_) } @vertices;

    return scalar @vertices == scalar @colors;
}

=head2 METHODS ON VERTICES

=cut

sub add_vertex {
    ## String -> State!
    my ( $self, $vertex ) = @_;
    return if $self->has_vertex($vertex);
    $self->{$vertex} = [];
}

sub add_vertices {
    my ( $self, @vertices ) = @_;
    $self->add_vertex($_) for @vertices;
}

sub get_neighbors {
    ## String -> ArrayRef
    my ( $self, $vertex ) = @_;

    if ( exists $self->{$vertex} ) {
        if ( defined $self->{$vertex} ) {
            my $neighbors = $self->{$vertex};
            @$neighbors = grep { defined $_ } @$neighbors;
            return $neighbors;
        }
    }
    else {
        return;
    }
}

sub has_vertex {
    ## String -> Boolean
    my ( $self, $vertex ) = @_;
    if ( exists $self->{$vertex} && defined $self->{$vertex} ) {
        return 1;
    }
    return;
}

sub remove_vertex {
    ## String -> State!
    my ( $self, $vertex ) = @_;

    my $neighbors = $self->get_neighbors($vertex);

    # Our general strategy for deleting things is to set the vertex's
    # position in the array of arrays graph representation to undef.
    #
    # In this pass, we delete all edges between each of this vertex's
    # neighbor and the vertex. Note the order of the arguments:
    #
    # - Because we are deleting *this* vertex (and *not* the
    # neighbor), we have the neighbor (which is not being deleted) set
    # its connection to this vertex to undef (i.e., deleted).
    #
    # - Then, we delete any edge attributes that exist between the two
    # vertices (since there is no edge there anymore).
    for my $neighbor (@$neighbors) {
        $self->_remove_neighbor( $neighbor, $vertex );
        $self->delete_edge_attributes( $vertex, $neighbor );
    }

    # Then, we delete the "root" reference to the vertex by setting it
    # to undef.
    if ( exists $self->{$vertex} ) {
        delete $self->{$vertex};
    }
}

sub get_vertices {
    ## -> Array
    my $self = shift;
    my @vertices;
    for my $vertex ( keys %$self ) {
        next unless defined $vertex;
        next if $vertex eq '_INTERNAL';
        push @vertices, $vertex;
    }
    @vertices = sort @vertices;
    return @vertices;
}

sub get_degree {
    my ( $self, $vertex ) = @_;
    if ( $self->has_vertex($vertex) ) {
        my $neighbors = $self->get_neighbors($vertex);
        return scalar @$neighbors;
    }
    return;
}

sub set_vertex_attribute {
    ## String HashRef -> State!
    my ( $self, $vertex, $new_attrs ) = @_;
    return unless $self->has_vertex($vertex);

    # Attributes hashref already exists, so we add to it.  NOTE:
    # this is a hash so the update is destructive.
    for ( my ( $k, $v ) = each %$new_attrs ) {
        next unless defined $k;
        next if $k eq '';
        $self->{_INTERNAL}->{vertex_attrs}->{$vertex}->{$k} = $v;
    }
}

sub get_vertex_attribute {
    ## String String -> Value OR undef
    my ( $self, $vertex, $attribute ) = @_;
    return $self->{_INTERNAL}->{vertex_attrs}->{$vertex}->{$attribute};
}

sub get_vertex_attributes {
    ## String -> HashRef OR undef
    my ( $self, $vertex ) = @_;
    return unless $self->has_vertex($vertex);
    return $self->{_INTERNAL}->{vertex_attrs}->{$vertex};
}

sub delete_vertex_attributes {
    ## String -> Undefined OR State!
    my ( $self, $vertex ) = @_;
    return unless $self->has_vertex($vertex);
    delete $self->{_INTERNAL}->{vertex_attrs}->{$vertex};
}

sub set_vertex_color {
    ## String String -> Undefined OR State!
    my ( $self, $vertex, $color ) = @_;
    $self->set_vertex_attribute( $vertex, { color => $color } );
}

sub get_vertex_color {
    ## String -> String OR Undefined
    my ( $self, $vertex ) = @_;
    $self->get_vertex_attribute( $vertex, 'color' );
}

=head2 METHODS ON EDGES

=cut

sub get_edge {
    ## String String -> ArrayRef
    my ( $self, $a, $b ) = @_;

    return unless $self->edge_between( $a, $b );

    my $attrs = $self->get_edge_attributes( $a, $b );

    return [ $a, $b, $attrs ];
}

sub get_edges {
    ## -> Array
    my ($self) = @_;

    my @vertices = $self->get_vertices;
    my @answer;
    my %seen;

    for my $vertex (@vertices) {
        my $neighbors = $self->get_neighbors($vertex);

        for my $neighbor (@$neighbors) {
            next if $seen{ $vertex . $neighbor };
            push @answer, $self->get_edge( $vertex, $neighbor );
            $seen{ $vertex . $neighbor }++;
            $seen{ $neighbor . $vertex }++;
        }
    }

    @answer = sort { $a->[0] lt $b->[0] } @answer;
    return @answer;
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

sub get_edge_attributes {
    ## String String -> HashRef OR undef
    my ( $self, $start, $end ) = @_;
    my $pairkey = $start . $end;
    return $self->{_INTERNAL}->{edge_attrs}->{$pairkey};
}

sub get_edge_attribute {
    ## String String String -> Value OR undef
    my ( $self, $start, $end, $attribute ) = @_;

    my $pairkey = $start . $end;
    return $self->{_INTERNAL}->{edge_attrs}->{$pairkey}->{$attribute};
}

sub set_edge_attribute {
    ## String String HashRef -> State!
    # set_edge_attribute('s', 'a', { weight => 12 });
    my ( $self, $start, $end, $new_attrs ) = @_;

    my $pairkey1 = $start . $end;
    my $pairkey2 = $end . $start;

    # Attributes hashref already exists, so we add to it.  NOTE:
    # this is a hash so the update is destructive.
    for ( my ( $k, $v ) = each %$new_attrs ) {
        next unless defined $k;
        next if $k eq '';
        $self->{_INTERNAL}->{edge_attrs}->{$pairkey1}->{$k} = $v;
        $self->{_INTERNAL}->{edge_attrs}->{$pairkey2}->{$k} = $v;
    }
}

sub delete_edge_attributes {
    ## String String -> Undefined OR State!
    my ( $self, $start, $end ) = @_;
    return unless defined $start && defined $end;

    my $pairkey1 = $start . $end;
    my $pairkey2 = $end . $start;
    return
      unless ( exists $self->{_INTERNAL}->{edge_attrs}->{$pairkey1}
        && exists $self->{_INTERNAL}->{edge_attrs}->{$pairkey2} );
    delete $self->{_INTERNAL}->{edge_attrs}->{$pairkey1};
    delete $self->{_INTERNAL}->{edge_attrs}->{$pairkey2};
}

sub add_edge {
    ## String String -> State!
    my ( $self, $v1, $v2, $attrs ) = @_;
    $self->_add_neighbor( $v1, [$v2], $attrs );
    $self->_add_neighbor( $v2, [$v1], $attrs );
}

sub add_edges {
    my ( $self, @edges ) = @_;

    for my $elem (@edges) {
        ## ['a', 'b', { weight => 123 }]
        my ( $a, $b, $attrs ) = @$elem;
        $self->add_edge( $a, $b, $attrs );
    }
}

sub remove_edge {
    ## String String -> Boolean State! OR Undef
    my ( $self, $a, $b ) = @_;

    return unless $self->edge_between( $a, $b );

    # We delete A from B's list of neighbors, and delete B from A's
    # list of neighbors.  Then, we delete any edge attributes, since
    # said edge no longer exists.

    $self->_remove_neighbor( $a, $b );
    $self->_remove_neighbor( $b, $a );
    $self->delete_edge_attributes( $a, $b );

    return 1;
}

=head2 PATH SEARCH METHODS

=cut

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
        my $neighbors = $self->get_neighbors($v);

        for my $neighbor (@$neighbors) {
            next if $seen{$neighbor};
            $seen{$neighbor}++;

            # In this block, we are setting up the information we will
            # need to answer the question "Have we found a new
            # shortest path (by distance)?"
            my $distance_to_self         = $st->{$v}->{distance};
            my $old_distance_to_neighbor = $st->{$neighbor}->{distance};
            my $neighbor_edge_weight =
              $self->get_edge_attribute( $v, $neighbor, 'weight' );
            my $new_distance_to_neighbor =
              $distance_to_self + $neighbor_edge_weight;

            # This is the core of Dijkstra's algorithm: Have we
            # discovered a path whose distance to the neighbor is
            # shorter than the previously discovered path's distance?
            # If yes, we update the spanning tree with this new path
            # information.
            if ( $new_distance_to_neighbor < $old_distance_to_neighbor ) {
                $st->{$neighbor}->{distance} = $new_distance_to_neighbor;
                $st->{$neighbor}->{prev}     = $v;
            }

            if ( $neighbor eq $end ) {
                @path = $self->_st_walk( $st, $start, $end );
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
            next if $seen{$neighbor};
            $st->{$neighbor}->{prev} = $v;
            if ( $neighbor eq $end ) {
                $found++;
                @path = $self->_st_walk( $st, $start, $end );
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

=head2 GRAPH CLONING (OBJECT COPYING) AND EQUALITY CHECKS

=cut

sub clone {
    my ($self) = @_;
    my $copy = Storable::dclone($self);
    return $copy;
}

sub equals {
    my ( $self, $other ) = @_;

    return
      unless $self->isa('YAGL')
      && $other->isa('YAGL');

    my @xs = $self->get_vertices;
    my @ys = $other->get_vertices;

    return unless @xs ~~ @ys;

    my @es = $self->get_edges;
    my @fs = $other->get_edges;

    return unless @es ~~ @fs;

    my $self_attrs  = $self->_edge_attrs;
    my $other_attrs = $other->_edge_attrs;

    return unless %$self_attrs ~~ %$other_attrs;

    return 1;
}

=head2 INTERNAL HELPER METHODS

=cut

sub _add_neighbor {
    ## String ArrayRef HashRef -> State!
    my ( $self, $vertex, $neighbor, $data ) = @_;

    unless ( ref($neighbor) eq 'ARRAY' ) {
        my ( $package, $filename, $line ) = caller();
        die <<"EOF";
on line $line of file $filename:
  $package\:\:_add_neighbor('$vertex', '$neighbor', '$data'):
    expected arrayref, got '$neighbor'
EOF
    }

    if ( $self->has_vertex($vertex) ) {
        my $neighbors = $self->get_neighbors($vertex);
        for my $value (@$neighbor) {
            push @$neighbors, $value;
        }
        $self->{$vertex} = $neighbors;
    }
    else {
        $self->{$vertex} = $neighbor;
    }

    if ($data) {
        $self->set_edge_attribute( $vertex, $neighbor->[0], $data );
    }
}

sub _remove_neighbor {
    ## String String -> State! OR Undef
    my ( $self, $vertex, $neighbor ) = @_;

    return unless $self->edge_between( $vertex, $neighbor );

    # Graphs are represented as an array of arrays that look like the
    # following:
    #
    # my $example = [
    #     [ 's', [ 'a', 'd' ] ],
    #     [ 'a', [ 's', 'b', 'd' ] ],
    #     [ 'b', [ 'a', 'c', 'e' ] ],
    #     [ 'c', ['b'] ],
    #     [ 'd', [ 's', 'a', 'e' ] ],
    #     [ 'e', [ 'b', 'd', 'f' ] ]
    # ];
    #
    # To delete a specific neighbor, we have to walk this vertex's
    # list of neighbors (skipping any already deleted neighbors) and
    # set the neighbor's value to undef.

    return unless $self->has_vertex($vertex);
    my $neighbors = $self->get_neighbors($vertex);

    for ( my $i = 0 ; $i <= @$neighbors ; $i++ ) {
        my $this = $self->{$vertex}->[$i];
        next unless defined $this;
        if ( $this eq $neighbor ) {
            $self->{$vertex}->[$i] = undef;
        }
    }
}

sub _st_walk {
    ## String String HashRef -> Array
    my ( $self, $st, $start, $end ) = @_;

    my @path;

    if ( exists $st->{$start}->{distance} ) {
        push @path, { vertex => $end, distance => $st->{$end}->{distance} };
        my $prev = $st->{$end}->{prev};

        while (1) {
            if ( $prev eq $start ) {

                push @path,
                  { vertex => $prev, distance => $st->{$prev}->{distance} };
                last;
            }
            push @path,
              { vertex => $prev, distance => $st->{$prev}->{distance} };
            $prev = $st->{$prev}->{prev};
            next;
        }
    }
    else {
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
    }
    return reverse @path;
}

sub _edge_attrs {
    my ($self) = @_;
    return $self->{_INTERNAL}->{edge_attrs};
}

sub _vertex_attrs {
    my ($self) = @_;
    return $self->{_INTERNAL}->{vertex_attrs};
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

=head2 GRAPH COLORING METHODS

=cut

sub get_color_degree {
    ## String -> Integer
    my ( $self, $vertex ) = @_;
    my $count = 0;
    my @colors;
    my $neighbors = $self->get_neighbors($vertex);
    for my $neighbor (@$neighbors) {
        my $color = $self->get_vertex_color($neighbor);
        if ($color) {
            $count++;
            push @colors, $color;
        }
    }
    return ( $count, @colors );
}

sub color_vertices {
    ## -> State!
    my ($self) = @_;

    # This algorithm is due to Brelaz, as described in Skiena,
    # _Implementing Discrete Mathematics_.
    #
    # 1. Number the colors from 1 to k.
    #
    # 2. Color the vertex of largest degree with color 1.
    #
    # 3. Then repeatedly select the vertex with highest _color
    # degree_, where the color degree is the number of adjacent
    # vertices which have already been colored, and color it with the
    # smallest possible color.

    my @colors =
      qw/ violet indigo orange yellow blue green red/;    # Ordered by indices
    my @vertices_by_degree =
      sort { $self->get_degree($a) > $self->get_degree($b) }
      $self->get_vertices;

    my $v = pop @vertices_by_degree;
    $self->set_vertex_color( $v, $colors[0] );

    my @vertices_by_color_degree =
      sort { $self->get_color_degree($a) > $self->get_color_degree($b) }
      $self->get_vertices;

    while ( my $v = pop @vertices_by_color_degree ) {
        my ( $count, @adjacent_colors ) = $self->get_color_degree($v);
        for my $color (@colors) {
            $self->set_vertex_color( $v, $color )
              unless $color ~~ @adjacent_colors;
        }
        @vertices_by_color_degree =
          sort { $self->get_color_degree($a) > $self->get_color_degree($b) }
          @vertices_by_color_degree;
    }
}

sub uncolor_vertices {
    my ($self) = @_;
    for my $vertex ( $self->get_vertices ) {
        $self->set_vertex_color( $vertex, undef );
    }
}

sub vertex_colors {
    ## -> Array[Hashref]
    my ($self) = @_;
    my @colors;
    for my $vertex ( $self->get_vertices ) {
        push @colors,
          [ $vertex, { color => $self->get_vertex_color($vertex) } ];
    }
    return @colors;
}

sub chromatic_number {
    ## -> Integer OR Undef
    my ($self) = @_;
    my $n      = 0;
    my @colors = $self->vertex_colors;
    return unless @colors;

    my %colors;
    for my $elem (@colors) {
        my $color = $elem->[1]->{color};
        $colors{$color}++;
    }
    my @keys = keys %colors;
    if (@keys) {
        $n = scalar @keys;
    }
    return $n;
}

sub to_colored_graphviz {
    ## -> String
    my ($self) = @_;
    my $gv = GraphViz->new( directed => 0, style => 'filled' );

    my %seen;
    for my $vertex ( $self->get_vertices ) {
        $gv->add_node(
            $vertex,
            style => 'filled',
            color => $self->get_vertex_color($vertex)
        );
        my $neighbors = $self->get_neighbors($vertex);
        for my $neighbor (@$neighbors) {
            my $color = $self->get_vertex_color($neighbor);
            $gv->add_node( $neighbor, fillcolor => $color );
            $gv->add_edge( $vertex, $neighbor )
              unless $seen{ $vertex . $neighbor };
            $seen{ $neighbor . $vertex }++;
            $seen{ $vertex . $neighbor }++;
        }
    }

    return $gv->as_canon;
}

1;
